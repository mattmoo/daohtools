#' Plot DAOH in a histogram
#'
#' @param input.dist The DAOH dataset on which to base the sampling
#'   distribution, as a vector.
#' @param values The value that can the sampling distribution can take (default
#'   min(input.dist):max(input.dist), which will fail if not integer input)
#' @return ggplot2 plot
#'
#' @export
generate.smoothed.sampling.dist = function(input.dist, values = NULL, smooth.func = smooth.daoh) {


  #Count outcome including zeroes
  freq.dt = data.table::data.table(table(input.dist))
  data.table::setnames(freq.dt, c('outcome','N'))
  data.table::set(x = freq.dt,
                  j = 'outcome',
                  value = as.numeric(freq.dt$outcome))

  #Append absent DAOH values if necessary
  if (length(values) == 0) {
    values = min(input.dist):max(input.dist)
  }

  #Add missing rows if there are some zeroes
  if ((sum(!values %in% freq.dt$outcome) > 0)) {
    freq.dt = rbind(freq.dt, data.table::data.table(outcome = values[!values %in% freq.dt$outcome], N=0))
  }
  setorderv(x = freq.dt,
            cols = 'outcome')
  freq.prob.dt = smooth.func(freq.dt)

  #Return input parameters for base::sample
  result = list()
  result$x = freq.prob.dt[,outcome]
  result$prob = freq.prob.dt[,probs/min(freq.prob.dt[probs>0,probs])]


  return(result)

}

#' A smoothing function particularly for DAOH.
#'
#' @param input.freq.dt Data.table with columns 'outcome' and 'N'
#' @return Data.table with same columns as input, but also 'probs' for feeding
#'   into sample()
#'
#' @export
smooth.daoh = function(input.freq.dt) {

  output.freq.prob.dt = data.table::copy(input.freq.dt)

  # #Pump up the numbers for a smoother smooth
  # output.freq.prob.dt[,probs := N*1000/min(output.freq.prob.dt[N>0,N])]
  data.table::set(x = output.freq.prob.dt,
                  j = 'probs',
                  value = output.freq.prob.dt[,N]*1000/min(output.freq.prob.dt[N>0,N]))

  #Smooth between Day 1 (i.e. skipping zero peak), and the day with maximum
  #frequency (it's usually pretty smooth after that)
  daySmoothLimits = c(1,output.freq.prob.dt[N == max(output.freq.prob.dt$N), outcome])
  indSmoothLimits = match(daySmoothLimits, output.freq.prob.dt[,outcome])

  #First smooth the whole range, then progressively smaller ones.
  nSmooths = 5
  smoothProportions = c(1,.75,.65,.5,.25)
  for (smoothInd in 1:nSmooths) {
    smoothProportion = smoothProportions[min(smoothInd, length(smoothProportions))]

    indsToSmooth = indSmoothLimits[1]:round(smoothProportion*(indSmoothLimits[2]-1))

    data.table::set(x = output.freq.prob.dt,
                    i = indsToSmooth,
                    j = 'probs',
                    value = bezierCurve(output.freq.prob.dt[indsToSmooth, outcome],
                                        output.freq.prob.dt[indsToSmooth, probs],
                                        n = length(indsToSmooth))$y)

    # output.freq.prob.dt[indsToSmooth,N := bezierCurve(output.freq.prob.dt[indsToSmooth, outcome],
    #                                         output.freq.prob.dt[indsToSmooth, N],
    #                                         n = length(indsToSmooth))$y]

  }

  # There might be some issue with low input n where daoh 1 is zero, copy that value from daoh 2 if so.
  if (output.freq.prob.dt[outcome==1, probs==0]) {
    data.table::set(x = output.freq.prob.dt,
                    i = which(output.freq.prob.dt[,outcome==1]),
                    j = 'probs',
                    value = output.freq.prob.dt[outcome==2, probs])

  }

  return(output.freq.prob.dt)
}

# x, y: the x and y coordinates of the hull points
# n: the number of points in the curve.
#'@export
bezierCurve <- function(x, y, n=10)
{
  outx <- NULL
  outy <- NULL

  i <- 1
  for (t in seq(0, 1, length.out=n))
  {
    b <- bez(x, y, t)
    outx[i] <- b$x
    outy[i] <- b$y

    i <- i+1
  }


  return (list(x=outx, y=outy))
}

bez <- function(x, y, t)
{
  outx <- 0
  outy <- 0
  n <- length(x)-1
  for (i in 0:n)
  {
    outx <- outx + choose(n, i)*((1-t)^(n-i))*t^i*x[i+1]
    outy <- outy + choose(n, i)*((1-t)^(n-i))*t^i*y[i+1]
  }

  return (list(x=outx, y=outy))
}



#Modifies a DAOH distribution (provided as a set of probabilities) by
#manipulating it until a certain quantile is equal to the provided.
#Multiplies the distribution with a straight line, from minMult to maxMult.

#' Modify a DAOH distribution, until at.quantile is target.quantile.val.
#' Uses data.table so that things can be done by reference.
#'
#' @param x Possible values for daoh
#' @param prob Probability of each daoh value
#' @param target.quantile.val The target value for at.quantile
#' @param relative.quantile.val Value that the target quantile should be relative to its current value.
#' @param at.quantile The quantile at which to insert a difference (default: 0.5).
#' @param multiple.per.step Probabilities will be adjusted as a function of this number.
#' @param output.to.console Print progress to console.
#' @param empirical.dist Calculate quantiles for calculating cost empirically (slower)
#' @param n.emp Number of samples that will be generated when empirically finding quantiles
#'
#' @return The modified distribution as a list containing values and weights.
#'
#' @export
modify.daoh.distribution = function(x = 0:90,
                                    prob = rep(1,90),
                                    target.quantile.val = NULL,
                                    relative.quantile.val = NULL,
                                    at.quantile = .5,
                                    multiple.per.step = .01,
                                    output.to.console = T,
                                    empirical.dist = F,
                                    n.emp = 100000) {

  #Set target quantile if provided in relative terms.
  if (is.null(target.quantile.val) & is.null(relative.quantile.val)) {
    stop('Wait a second... You didn\'t give modify.daoh.distribution a value to modify the quantile...')
  } else if (is.null(target.quantile.val) & !is.null(relative.quantile.val)) {
    if (empirical.dist) {
      target.quantile.val = quantile.dist.emp(x = x,
                                              prob = prob,
                                              quantile = at.quantile,
                                              n = n.emp) - relative.quantile.val
    } else {
      target.quantile.val = quantile.dist(x = x,
                                          prob = prob,
                                          quantile = at.quantile) + relative.quantile.val
    }
  }

  maxIterations = 1000
  iterations = 1
  tolerance = .01

  cost = 1000
  refineFurther = T

  #Do a few iterations after reaching the first distribution satisfying the criteria.
  lastCosts = rep(NA_real_,10)

  # targetQuantileIndex = which(x == target.quantile.val)
  best.cost = cost
  best.mean.last.costs = Inf
  best.x = NA_real_
  best.prob = NA_real_

  #Loop through modifying the distribution
  # while (( abs(cost) > tolerance) & (iterations < maxIterations)) {
  # print(( abs(cost) > tolerance) & (mean(abs(lastCosts))  tolerance) & (iterations < maxIterations))
  while (refineFurther & (iterations < maxIterations)) {

    if (empirical.dist) {
      current.quantile.val = quantile.dist.emp(x=x, prob=prob, quantile = at.quantile, n=n.emp)
    } else {
      current.quantile.val = quantile.dist(x=x, prob=prob, quantile = at.quantile)
    }
    # print(x)
    # print(prob)
    cost = current.quantile.val - target.quantile.val
    cost = unname(cost)

    lastCosts[iterations %% (length(lastCosts)+1)] = cost

    mean.last.costs = mean(lastCosts[!is.na(lastCosts)])
    #TODO: Could do something with zeroes to make things more organic?

    #If current quantile is greater than target, then increase probability of
    #lower values, in proportion to current value.
    if (cost > 0) {
      mult = (max(prob)/prob)^multiple.per.step
    } else if (cost < 0) {
      mult = 1/((max(prob)/prob)^multiple.per.step)
    } else if (cost == 0) {
      if (mean(lastCosts[!is.na(lastCosts)]) > 0) {
        mult = (max(prob)/prob)^(multiple.per.step/2)
      } else if (mean(lastCosts[!is.na(lastCosts)]) < 0) {
        mult = 1/((max(prob)/prob)^(multiple.per.step/2))
      }
    }
    prob = prob * mult

    #Remove pesky NaNs that crop up if the probability is zero
    prob[is.nan(prob)] = 0

    #Ensure that current cost is below tolerance, and that the last n were on average.
    refineFurther = !((abs(cost) <= tolerance) & (abs(mean(lastCosts[!is.na(lastCosts)])) <= tolerance))

    # #Check quantile value empirically if the function thinks that it's done.
    # if (!refineFurther) {
    #   if (abs(quantile.dist.emp(x=x, prob=prob, quantile = at.quantile) - target.quantile.val)) {
    #     refineFurther == T
    #   }
    # }

    #Check if it's a better one than the next best.
    if (abs(cost) <= abs(best.cost) | (abs(cost) == abs(best.cost) & (abs(mean.last.costs) < abs(best.mean.last.costs)))) {
      best.cost = cost
      best.mean.last.costs = mean.last.costs
      best.x = x
      best.prob = prob
    }

    iterations = iterations + 1


    if (output.to.console) {
      message(paste("Iteration:",iterations))
      message(paste("Target:",target.quantile.val))
      message(paste("Current:",current.quantile.val))
      message(paste("Last costs:", paste(lastCosts[!is.na(lastCosts)], collapse = ', ')))
      message(paste("Current cost:",cost))
      message(paste("Mean last costs:",mean.last.costs))
      message('')
    }

    if (iterations == maxIterations) {
      message(paste(maxIterations, 'iterations reached.'))
      if (output.to.console) {
        message(paste("Best cost:",best.cost))
        message(paste("Best mean last costs:",best.mean.last.costs))
      }
      return(list(x = best.x,
                  prob = best.prob))
    }

  }

  return(list(x = x,
              prob = prob))
}


#' Find a quantile from a sampling distribution.
#'
#' @param x The values.
#' @param prob The weights (same length as values).
#' @param quantile The quantile to find.
#' @return Value at requested quantile.
#'
#' @export
quantile.dist = function(x, prob, quantile = 0.5) {
  cs = cumsum(prob)/sum(prob)
  csq = cs-quantile
  x[which(csq == min((csq)[csq > 0]))][1]
}

#' Empirically a quantile from a sampling distribution.
#'
#' @param x The values.
#' @param prob The weights (same length as values).
#' @param quantile The quantile to find.
#' @param n The number of random values to generate.
#' @return Value at requested quantile.
#'
#' @export
quantile.dist.emp = function(x, prob, quantile = 0.5, n=1000000) {
  unname(quantile(sample(x = x, prob = prob, size = n, replace = T), probs = quantile))
}

#' Generate a timed.sampling.dist object from an input distribution (with
#' timings obvs)
#'
#' @param input.dt The (data.table) input distribution to emulate with outcome
#'   and time columns
#' @param outcome.col.name The name of the outcome column
#' @param n.sampling.period How many sampling periods?
#' @param sampling.period.in.start At what time should the sampling start?
#'   Should be integer.
#' @param sampling.period.in.length How long should each time period be in the
#'   output? Should be integer.
#' @param sampling.period.out.start At what time should the probability
#'   distribution in the output start? Should be integer.
#' @param sampling.period.out.start How long should each period be in the
#'   output? Should be integer.
#' @param smooth.func What function should be used for smoothing? Mainly made
#'   for DAOH, so that's default. Should be integer.
#' @param values The value that can the sampling distribution can take (default
#'   min(input.dist):max(input.dist), which will fail if not integer input)
#' @return A Timed.Sampling.Dist object
#'
#' @export
generate.timed.sampling.dist = function(input.dt,
                                        outcome.col.name,
                                        n.sampling.period = 1,
                                        sampling.period.in.start = NULL,
                                        sampling.period.in.length = NULL,
                                        sampling.period.out.start = NULL,
                                        sampling.period.out.length = NULL,
                                        smooth.func = smooth.daoh,
                                        values = NULL) {

  #Append absent DAOH values if necessary
  if (length(values) == 0) {
    values = min(input.dt[,get(outcome.col.name)]):max(input.dt[,get(outcome.col.name)])
  }


  #Check for required column names.
  required.col.names = c('time',outcome.col.name)
  if (!Reduce('&', required.col.names %in% colnames(input.dt))) {
    stop(paste0('Input distribution missing ',
                paste0(required.col.names[required.col.names %in% colnames(input.dt)], collapse = ', ')))
  }
  #Try and set missing parameters automatically.
  #Generate only one period if no input parameters.
  time.min = min(input.dt$time)
  time.max = max(input.dt$time)
  if (is.null(sampling.period.in.start)) {
    sampling.period.in.start = time.min
  }
  if (is.null(sampling.period.in.start)) {
    sampling.period.in.length = time.max - time.min
  }
  #Copy input parameters if no output parameters
  if (is.null(sampling.period.out.start)) {
    sampling.period.out.start = sampling.period.in.start
  }
  if (is.null(sampling.period.out.length)) {
    sampling.period.out.length = sampling.period.in.length
  }

  #When should each period start? (plus one for the end of the last period)
  sampling.period.in.start.times = seq(from = sampling.period.in.start,
                                       to = sampling.period.in.start + sampling.period.in.length*(n.sampling.period),
                                       by = sampling.period.in.length)


  #When should each period start? (plus one for the end of the last period)
  # sampling.period.out.start.times = seq()
  sampling.period.out.start.times = seq(from = sampling.period.out.start,
                                        to = sampling.period.out.start + sampling.period.out.length*(n.sampling.period),
                                        by = sampling.period.out.length)

  #Ensure the last period encompasses the last day.
  sampling.period.out.start.times[length(sampling.period.out.start.times)] = sampling.period.out.start.times[length(sampling.period.out.start.times)]+2
  # print(sampling.period.out.start.times)


  #Create a new timed sampling distribution
  tsd = new('Timed.Sampling.Dist')
  #Iterate through requested time periods
  for (period.ind in 1:n.sampling.period) {
    #Get beginning and end of both input and output periods.
    time.begin.in = sampling.period.in.start.times[period.ind]
    time.end.in = sampling.period.in.start.times[period.ind+1]-1
    time.begin.out = sampling.period.out.start.times[period.ind]
    time.end.out = sampling.period.out.start.times[period.ind+1]-1

    #Get the values from the time period.
    input.dist = input.dt[time >= time.begin.in & time < time.end.in,
                          get(outcome.col.name)]

    #Smooth the distribtion.
    dist = generate.smoothed.sampling.dist(input.dist, values = values)

    #Add to sampling distribution.
    tsd$add.sampling.dist(values = dist$x,
                          weights = dist$prob,
                          time.begin = time.begin.out,
                          time.end = time.end.out)
  }
#   print(time.begin.out)
#   print(time.end.out)
# asassas
  return(tsd)
}

#'Modify a timed DAOH distribution.
#'
#'If there is no split at time.begin and/or time.end, one will be inserted. Each
#'period between mod.time.begin and mod.time.end will then be individually
#'modified.
#'
#'
#'@param tsd The Timed.Sampling.Distribution object.
#'@param mod.time.begin The time to start the modification.
#'@param mod.time.end The time at which to end the modification.
#'@param target.quantile.val The target value for the quantile of interest (by
#'  default the median)
#'@param ... Arguments to be sent to modify.daoh.distribution
#'
#'@export
modify.timed.daoh.distribution = function(tsd, mod.time.begin, mod.time.end, target.quantile.val = NULL, relative.quantile.val = NULL, ...) {
  #Insert splits if needed.
  tsd$split.period(mod.time.begin, silent = T)
  tsd$split.period(mod.time.end, silent = T)
  #Get periods from requested times.
  mod.period.sampling.dt = tsd$sampling.dt[mod.time.begin <= time.begin & mod.time.end > time.end,]
  #Remove from the actual sampling period
  remaining.sampling.dt = tsd$sampling.dt[!(mod.time.begin <= time.begin & mod.time.end > time.end),]
  #Modify all distributions in selected time period.
  mod.period.sampling.dt[,
                         weights := modify.daoh.distribution(x = .SD[,values],
                                                             prob = .SD[,weights],
                                                             target.quantile.val = target.quantile.val,
                                                             relative.quantile.val = relative.quantile.val,
                                                             multiple.per.step = 0.01,
                                                             empirical.dist = F,
                                                             ...)$prob,
                         by=.(time.begin,time.end)]

  tsd$sampling.dt = rbind(remaining.sampling.dt,mod.period.sampling.dt)
}


#' Generate the timed sampling distributions from real data.
#'
#' Will also set the patients per unit time.
#'
#' @param this.site.dt The data as a data.table, with time and outcome columns.
#' @param n.sampling.period How many periods should the distribution be divided
#'   into?
#' @param outcome.col.name Name of outcome column.
#' @param sampling.period.out.start Time to start the timed sampling dist
#' @param sampling.period.out.length Length of each period in timed sampling
#'   dist
#' @param values Values that timed sampling dist can take.
#' @param smooth.func How to smooth the distribution.
#' @param site The SW.Site object to set by reference
#'
#' @return The timed sampling distribution (note however that it is set in the
#'   site by reference if site is not NULL)
#'
#' @export
generate.site.tsd = function(this.site.dt,
                             n.sampling.period,
                             outcome.col.name,
                             sampling.period.out.start,
                             sampling.period.out.length,
                             values = 0:89,
                             smooth.func = smooth.daoh,
                             site = NULL) {
  
  print(1)
  total.time.length = floor((this.site.dt[,max(time)] - this.site.dt[,min(time)]))
  
  print(2)
  tsd = generate.timed.sampling.dist(input.dt = this.site.dt,
                                     outcome.col.name = outcome.col.name,
                                     n.sampling.period = n.sampling.period,
                                     sampling.period.in.start = this.site.dt[,min(time)],
                                     sampling.period.in.length = total.time.length/n.sampling.period,
                                     sampling.period.out.start = sampling.period.out.start,
                                     sampling.period.out.length = sampling.period.out.length,
                                     smooth.func = smooth.func,
                                     values = values)
  
  print(3)
  if(!is.null(site)) {
    site$set.timed.sampling.dist(tsd)
    site$set.sim.ppt.per.unit.time(nrow(this.site.dt)/total.time.length)
  }

  return(tsd)
}

#' Generate timed sampling distributions for all sites in a
#'
#' @param input.dt Data for all sites as data.table
#' @param site.dt Data.table about sites and their clusters.
#' @param cluster.dt Data.table about clusters and when they start/stop
#' @param n.sampling.period How many periods should the distribution be divided
#'   into?
#' @param outcome.col.name Name of outcome column.
#' @param sampling.period.out.start Time to start the timed sampling dist
#' @param sampling.period.out.length Length of each period in timed sampling
#'   dist
#' @param values Values that timed sampling dist can take.
#' @param smooth.func How to smooth the distribution.
#' @param output.to.console Output progress to console.
#'
#' @return The timed sampling distribution (note however that it is set in the
#'   site by reference)
#'
#' @export
generate.all.site.tsds = function(input.dt, site.dt, cluster.dt, n.sampling.period, outcome.col.name, values = 0:89, smooth.func = smooth.daoh, output.to.console = F) {

  site.cluster.dt = merge(site.dt, cluster.dt, by = 'cluster.id') #Merge implementation dates onto sites
  # for (site.ind in 9:9) {
  for (site.ind in 1:nrow(site.dt)) {
    if (output.to.console) {
      message(site.ind)
      message(site.cluster.dt[site.ind, site.id])
      message(site.cluster.dt[site.ind, site.name])
    }
    this.site.dt = input.dt[site.id == site.cluster.dt[site.ind, site.id]]
    if (nrow(this.site.dt) == 0) {
      ms = paste0("No data for ", site.cluster.dt[site.ind, site.name], ' (', site.cluster.dt[site.ind, site.id], ')')
      message(ms)
      warning(ms)
      next
    }

    cl.id = site.cluster.dt[site.ind, cluster.id]

    site = site.cluster.dt[site.ind, site.obj][[1]]

    sampling.period.out.start = as.numeric(site.cluster.dt[site.ind, cluster.start.time])
    sampling.period.out.length = floor((site.cluster.dt[site.ind, cluster.end.time] - site.cluster.dt[site.ind, cluster.start.time])/n.sampling.period)

    print(sampling.period.out.start)
    print(sampling.period.out.length)


    generate.site.tsd(
      this.site.dt = this.site.dt,
      n.sampling.period = n.sampling.period,
      outcome.col.name = outcome.col.name,
      sampling.period.out.start = sampling.period.out.start,
      sampling.period.out.length = sampling.period.out.length,
      values = 0:88,
      smooth.func = smooth.daoh,
      site = site
    )


  }

}
