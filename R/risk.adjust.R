#' Calculates the quantile for each of a list of numbers, can be provided a list 
#' of numbers that it will round the percentile to. That should be useful for
#' using the correct quantile from the quantile regression to correct the
#' output.variable.
#' 
#' @param input.vector Vector of numbers to calculate quantiles for.
#' @param possible.values The values that the quantiles will be rounded to.
#' @return Vector of quantiles the same size as input
calculate.quantiles = function(input.vector, possible.values = NULL) {
  #Find quantile for each entry
  perc.rank = ecdf(input.vector)
  input.quantiles = perc.rank(input.vector)
  
  if (!is.null(possible.values)){
    #Find nearest quantile to those assessed from https://stackoverflow.com/questions/12861061/round-to-nearest-arbitrary-number-from-list
    # arbitrary.numbers is assumed to be sorted.
    # find the index of the number just below each number, and just above.
    # So for 6 in c(2,4,7,8) we would find 2 and 3.
    low = findInterval(input.quantiles,possible.values) # find index of number just below
    high = low+1 # find the corresponding index just above.
    # Find the actual absolute difference between the arbitrary number above and below.
    # So for 6 in c(2,4,7,8) we would find 2 and 1.
    # (The absolute differences to 4 and 7).
    low.diff = input.quantiles-possible.values[ifelse(low==0,NA,low)]
    high.diff = possible.values[ifelse(high==0,NA,high)]-input.quantiles
    # Find the minimum difference.
    # In the example we would find that 6 is closest to 7,
    # because the difference is 1.
    mins = pmin(low.diff,high.diff,na.rm=TRUE)
    # For each number, pick the arbitrary number with the minimum difference.
    # So for 6 pick out 7.
    pick = ifelse(!is.na(low.diff) & mins==low.diff,low,high)
    input.quantiles = possible.values[pick]
  }
  
  return(input.quantiles)
}

#' This function will ideally take an output variable name and a list of input
#' variable names, and risk adjust output.variable using quantile regression.
#'
#' Quantile regression fits lines to different quantiles, so this risk
#' adjustment procedure will find the quantile of output.variable for that
#' level, and subtract the effect of those variable from the patients outcome.
#'
#' Less quantiles will be faster, more might allow more finely grained
#' correction, but might also be deleterious as lower quantiles have quite wide
#' variability.
#'
#' @param input.dt Data.table with output.variable and covariates.
#' @param output.variable Character name of outcome to be adjusted.
#' @param covariates Character vector of column names to adjust upon.
#' @param jitter.output.variable controls whether the output.variable will be
#'   jittered. It seems like DAOH is not quite continuous enough for quantreg.
#'   Perhaps not inherent to DAOH per se, but related to the distribution of the
#'   CheckWHO data (Default: FALSE).
#' @param quantiles.to.assess Numeric vector, values between 0 and 1, of models
#'   to fit. Output variable will be adjusted by the model associated with the
#'   nearest quantile.
#' @param by.reference Logical, whether to modify the input data.table by
#'   inserting the risk adjusted variable, and any dummy variables (Default:
#'   FALSE).
#'
#' @export
risk.adjust.quantreg = function(input.dt,
                                output.variable,
                                covariates,
                                jitter.output.variable = FALSE,
                                quantiles.to.assess = seq(0.25, 0.75, by = 0.1),
                                by.reference = FALSE) {
  

  qr.model = generate.quantreg.model(
    input.dt = input.dt,
    output.variable = output.variable,
    covariates = covariates,
    jitter.output.variable = jitter.output.variable,
    quantiles.to.assess = quantiles.to.assess
  )
  
  result = risk.adjust.on.quantreg.model(input.dt = input.dt,
                                         qr.model = qr.model,
                                         by.reference = by.reference)
  
  return(result)
}

#' Generate a quantile regression model on which to risk adjust.
#' 
#' @param input.dt Data.table with output.variable and covariates.
#' @param output.variable Character name of outcome to be adjusted.
#' @param covariates Character vector of column names to adjust upon.
#' @param jitter.output.variable controls whether the output.variable will be
#'   jittered. It seems like DAOH is not quite continuous enough for quantreg.
#'   Perhaps not inherent to DAOH per se, but related to the distribution of the
#'   CheckWHO data (Default: FALSE).
#' @param quantiles.to.assess Numeric vector, values between 0 and 1, of models
#'   to fit. Output variable will be adjusted by the model associated with the
#'   nearest quantile.
#' @return A quantile regression model fitted at requested quantiles.
#'
#' @export
generate.quantreg.model = function(input.dt,
                                   output.variable,
                                   covariates,
                                   jitter.output.variable = FALSE,
                                   quantiles.to.assess = seq(0.25, 0.75, by = 0.1)) {
  
  
  # Jitter if requested.
  old.output.variable = output.variable
  if (jitter.output.variable) {
    new.output.variable = paste(output.variable,'jit',sep=".")
    
    data.table::set(x = input.dt,
                    j = new.output.variable,
                    value = jitter(unlist(as.list(input.dt[, output.variable, with =
                                                             FALSE])[[1]])))
    output.variable = new.output.variable
  }
  
  #Generate the formula from string inputs.
  formula = as.formula(paste(output.variable, '~', paste(covariates, collapse= "+")))
  #Do quantile regression.
  qr.model = quantreg::rq(formula, data=input.dt, tau = quantiles.to.assess)
  
  
  return(qr.model)
}

#' Apply the quantile regression models to risk adjust a data.table.
#'
#' @param input.dt Data.table with output.variable and covariates.
#' @param qr.model Quantreg model generated by generate.quantreg.model
#' @param by.reference Logical, whether to modify the input data.table by
#'   inserting the risk adjusted variable, and any dummy variables (Default:
#'   FALSE).
#'
#' @export
risk.adjust.on.quantreg.model = risk.adjust.quantreg = function(input.dt,
                                                                qr.model,
                                                                by.reference = FALSE) {
  
  if (!by.reference) {
    input.dt = data.table::copy(input.dt)
  }
  
  output.variable = colnames(qr.model$model)[1]
  covariates = colnames(qr.model$model)[-1]
  quantiles.to.assess = qr.model$tau
  
  #Name for risk adjusted column.
  adjusted.variable.name = paste(output.variable,'risk.adj', sep=".")
  
  
  #Generate dummy variables if required and calculate quantiles
  dummy.vars = NULL
  for (covariate in covariates) {
    print(covariate)
    var.levels = levels(unlist(input.dt[,covariate,with=FALSE]))
    #Check for factors
    if (!is.null(var.levels)) {
      #Create dummy variables for factors
      print(paste('Dummy variables constructed:', covariate))
      if (is.null(dummy.vars)) {
        dummy.vars = data.table::data.table(dummies::dummy(covariate,
                                                           data = input.dt))
      } else {
        dummy.vars = cbind(dummy.vars, data.table::data.table(dummy(covariate, 
                                                                    data = input.dt)))
      }
      # #Remove the first dummy variable, as it's a linear combination of the others.
      # dummy.vars[,1 := NULL]
    } else {
      #Do nothing for continuous variables
      print(paste('Continuous variable detected:', covariate))
    }
  }
  
  #Attach dummy variables to data table
  if (!is.null(dummy.vars)) {
    input.dt = cbind(input.dt,dummy.vars)
  }
  
  #Iterate through dummy variables in quantreg model, and adjust output.variable
  #for them. Hopefully the dummies and quantreg packages stay similar enough
  #that the dummy variables are named the same.
  
  #Copy output.variable column for adjusting.
  data.table::set(x = input.dt,
                  j = adjusted.variable.name,
                  value = input.dt[, output.variable, with = FALSE])
  
  #Get quantile for all subjects and levels of input variables, restricted to
  #the quantiles used by quantReg.
  input.dt[,quantile.overall := calculate.quantiles(input.vector = input.dt[,output.variable,with=FALSE][[1]],
                                                    possible.values = quantiles.to.assess)]
  
  #Now get the quantiles for each group for each dummy variable, and find
  #restricted quantiles. Only do this for factors,
  for (varInd in 2:nrow(qr.model$coefficients)) {
    #Get rowname (intercept is ignored for now).
    rn = rownames(qr.model$coefficients)[varInd]
    
    #Get the columns for each subject's quantile from quantreg coefficient table.
    quantileCols = match(input.dt[,quantile.overall],quantiles.to.assess)
    #Subtract effect in participants quantile from adjusted value.
    data.table::set(
      x = input.dt,
      j = adjusted.variable.name,
      value = input.dt[, adjusted.variable.name, with = FALSE] - input.dt[, rn, with = FALSE] *
        as.numeric(qr.model$coefficients[varInd, quantileCols])
    )
    
    
    
  }
  
  return(input.dt)
}

#' Functions for generating the data needed for adjustment.
#'
#' Creates a number of risk groups from a regression model. Higher risk groups
#' are more likely to score highly in the model, and vice versa.
#'
#' @param input.df A data.frame with the columns from the regression model.
#' @param model The regression model, should be compatible with base::predict
#' @param n.groups Number of groups to generate.
#' @param riskgp.level.name.suffix Suffix to place after the number of the risk
#'   group (default: '')
#'
#' @return A vector of risk groups in the same order as the rows of input.df.
#'   Higher risk groups are more likely to score highly in the model, and vice
#'   versa. Groups will be names e.g. '01' + riskgp.level.name.suffix
#' @export
#'
generate.riskgp.vector = function(input.df,
                                  model,
                                  n.groups,
                                  riskgp.level.name.suffix = '') {
  # Generate pred probabilities for each person and then assign to ordered groups
  predicted <- predict(model, newdata = input.df)
  # Can adjust the rounding level to increase or decrease the total number of
  # groups - we usually aim for ~10 but if there's not much difference in the
  # predicted values (small model) then it might be hard to get 10 want to try
  # avoid centers having 0 people in lots of risk groups - similar problems to
  # direct standardisation
  
  # Could maybe make this faster using data.table frank, but it's not very slow
  # anyway.
  riskgp <-
    ceiling(10 * rank(n.groups * predicted, ties = "random") / nrow(input.df))
  # riskgp<-round(rank(df$p,ties="random"),-4)
  
  fmt.string = paste0('%0', nchar(as.character(n.groups)), 'd')
  
  riskgp = sprintf(fmt = paste0(fmt.string, riskgp.level.name.suffix), riskgp)
  
  return(riskgp)
}

#' Generates risk group columns.
#'
#' Generates risk group columns from a list of models and inserts them as
#' reference. Component columns for each model will named with a common prefix,
#' followed by the name of that value in model.list, and an overall risk group.
#'
#' @param input.dt Input data.table, will be modified by reference.
#' @param model.list Models to risk adjust on, columns will be given the suffix
#'   of the name of the corresponding list value. Will be coerced to list if not
#'   provided (which may mess with naming, e.g. providing a single would result
#'   in a list with no name).
#' @param riskgp.col.name What should the risk group column be called? (And what
#'   should component columns start with?). (default: 'riskgp').
#' @param n.groups Number of groups to generate per risk factor (default: 10)
#'
#' @return The modified input.dt with length(model.list)+1 extra columns,
#'   unnecessary to use it as it will be modified by reference. Column
#'   riskgp.col.name will be a factor, designed to use in the generate.weights
#'   function.
#' @export
#'
#' @examples
generate.dra.riskgp.columns = function(input.dt,
                                   model.list,
                                   riskgp.col.name = 'riskgp',
                                   n.groups = 10) {
  
  if (!is.list(model.list)) {
    model.list = list(model.list)
  }
  model.names = names(model.list)
  col.names = paste0(riskgp.col.name, model.names)
  
  
  # Go through and create component columns
  for (model.ind in 1:length(model.list)) {
    
    model = model.list[[model.ind]]
    model.name = model.names[[model.ind]]
    col.name = col.names[[model.ind]]
    
    data.table::set(x = input.dt,
                    j = col.name,
                    value = generate.riskgp.vector(
                      input.df = risk.adjusted.regression.dt,
                      model = model,
                      riskgp.level.name.suffix = model.name,
                      n.groups = n.groups
                    ))
    
  }
  
  # Create overall risk group column.
  data.table::set(x = input.dt,
                  j = riskgp.col.name,
                  value = interaction(input.dt[, col.names, with = FALSE], sep = ':'))
  
  
  
  return(input.dt)
  # risk.adjusted.regression.dt[, riskgp.mortality := generate.riskgp(
  #   input.df = risk.adjusted.regression.dt,
  #   model = mort.model,
  #   riskgp.level.name.suffix = 'MORT'
  # )]
  
}



#' Generate weights for direct risk adjustment in each risk group reflecting how
#' likely they are to occur in the provided data. This will add a few columns to
#' input.dt, might bugger up if you have similarly named ones.
#'
#' @param input.dt Input data.table with at least columns group.col.name and
#'   riskgp.col.name.
#' @param group.col.name The name of the column that is being adjusted against.
#' @param riskgp.col.name The name of the risk group column (default: 'riskgp')
#' @param weight.col.name  The name of the column in output that contains
#'   weights (default: 'wt')
#'
#' @return
#' @export
calculate.dra.weights = function(input.dt,
                                group.col.name,
                                riskgp.col.name = 'riskgp',
                                weight.col.name = 'wt') {
  
  
  old.colorder = names(input.dt)
  
  # Get number of patients by risk group and group to be adjusted on, then
  # order.
  weight.dt = input.dt[,.(pop.N = .N),by = c(riskgp.col.name, group.col.name)][order(get(group.col.name), get(riskgp.col.name))]
  
  # Get the proportion of patients in each group overall.
  weight.dt = data.table:::merge.data.table(x = weight.dt,
                                            y = input.dt[, .(pop.prob = .N / nrow(input.dt)), by = riskgp.col.name],
                                            by = riskgp.col.name)
  
  # Get the proportion of total patients in each risk group in each group.
  group.col.name.prob = paste0(group.col.name, '.prob')
  weight.dt = data.table:::merge.data.table(x = weight.dt,
                                            y = weight.dt[, .(riskgp = get(riskgp.col.name),
                                                              group.prob = pop.N / sum(pop.N)), by = group.col.name],
                                            by = c(group.col.name, 'riskgp'))
  
  
  # Calculate weight.
  weight.dt[,wt := pop.prob/group.prob]
  data.table::set(x = weight.dt,
                  j = weight.col.name,
                  value = weight.dt[, pop.prob/group.prob])
  
  output.dt = merge(
    x = input.dt,
    y = weight.dt,
    by = c(group.col.name, riskgp.col.name)
  )
  
  # Put columns in original order
  setcolorder(x = output.dt, neworder = old.colorder)
  
  return(output.dt)
}

#' Calculates a GLM for using in the survey direct risk adjustment process.
#'
#' @param input.dt Input data.table with all cited columns present.
#' @param outcome.col.name The outcome column.
#' @param covariate.col.names A character vector of column names to add to the
#'   model.
#' @param outlier.limits Vector air of numeric. Outcomes lower than the first or
#'   higher than the second will be excluded from the model. Might be a bit
#'   bizarre for factors (default: c(-Inf, Inf))
#' @param ... Extra variables to be passed to stats::glm. In particular, you'll
#'   probably want to specify the family (e.g. binomial(link = 'logit') for
#'   binary, poisson for length of stay)
#'
#' @return A GLM object.
#' @export
calculate.dra.risk.adjustment.glm = function(input.dt,
                                         outcome.col.name,
                                         covariate.col.names,
                                         outlier.limits = c(-Inf, Inf),
                                         ...) {
  
  input.dt = input.dt[get(outcome.col.name) >= outlier.limits[1]]
  input.dt = input.dt[get(outcome.col.name) <= outlier.limits[2]]
  
  risk.adjustment.glm = glm(as.formula(paste(
    outcome.col.name,
    '~',
    '(',
    paste(covariate.col.names, collapse = "+"),
    ')'
  )),
  data = input.dt,
  ...)
  
  return(risk.adjustment.glm)
}