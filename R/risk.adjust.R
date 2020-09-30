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
