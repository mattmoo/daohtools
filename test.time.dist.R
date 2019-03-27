
devtools::document('../stepmywedge')
devtools::install('../stepmywedge',dependencies = F, reload = T)
library(stepmywedge)

#
devtools::document()
devtools::install(dependencies = F, reload = T)
library(daohtools)

#Load data.
daoh.ssc.dt = fread('data/daoh-ssc.csv')
data.table::setnames(daoh.ssc.dt, 'opdate', 'time')

#Just get the first group.
input.dt = daoh.ssc.dt[group == 1]

time.min = daoh.ssc.dt[,min(time)]
time.max = daoh.ssc.dt[,max(time)]

outcome.col.name = 'daoh'


#Divide into this number of periods.
n.sampling.period = 4
#How long should each period be, and when should they start?
sampling.period.in.start = 1
sampling.period.in.length = 40
#How long should the period during which each sampling distribution is applied
#be in the simulated data?
sampling.period.out.start = 1
sampling.period.out.length = 40



tsd = generate.timed.sampling.dist(input.dt,
                                   outcome.col.name,
                                    n.sampling.period,
                                    sampling.period.in.start,
                                    sampling.period.in.length,
                                    sampling.period.out.start,
                                    sampling.period.out.length)


mod.time.begin = 10
mod.time.end = 40
atQuantile = 0.5
target.quantile.val = NULL
relative.quantile.val = NULL

bef = copy(tsd$sampling.dt)

modify.timed.daoh.distribution(tsd,
                               mod.time.begin,
                               mod.time.end,
                               target.quantile.val = NULL,
                               relative.quantile.val = -1,
                               at.quantile = atQuantile)


# tsd$split.period(mod.time.begin, silent = T)
# tsd$split.period(mod.time.end+1, silent = T)
#
# mod.period.sampling.dt = tsd$sampling.dt[mod.time.begin <= time.begin & mod.time.end >= time.end,]
#
# sampling.dt = tsd$sampling.dt[!(mod.time.begin <= time.begin & mod.time.end >= time.end),]
#
# mod.period.sampling.dt[,print(.SD), by=.(time.begin,time.end)]
#
# mod.period.sampling.dt[,
#                        weights := modify.daoh.distribution(x = .SD[,values],
#                                                            prob = .SD[,weights],
#                                                            atQuantile = atQuantile,
#                                                            relative.quantile.val = -4,
#                                                            multiplePerStep = 0.01,
#                                                            empirical.dist = F)$prob,
#                        by=.(time.begin,time.end)]
#
# # asas  = modify.daoh.distribution(x = mod.period.sampling.dt[,values],
# #                                  prob = mod.period.sampling.dt[,weights],
# #                                  atQuantile = atQuantile,
# #                                  relative.quantile.val = -2,
# #                                  multiplePerStep = .01,
# #                                  empirical.dist = F)
# # tsd$split.period(150)
# # tsd$merge.periods.between(41,150)
# sampling.dt = rbind(sampling.dt,mod.period.sampling.dt)
# # aft = copy(tsd$sampling.dt)
#
print(sampling.dt[,
                             quantile.dist(x = .SD[,values],
                                           prob = .SD[,weights],
                                           quantile = atQuantile),
                             by=.(time.begin,time.end)])
#
# p = plot.distribution(sampling.dt[time.begin==10, values], sampling.dt[time.begin==10, weights], n =100000)
# plot(p)
#
#
# p = plot.distribution(sampling.dt[time.begin==1, values], sampling.dt[time.begin==1, weights], n =100000)
# plot(p)
#
# # tsd$split.period(60)
# # tsd$split.period(60)
