library(MASS)
library(fitdistrplus)

# p = seq(0,1, length=1000)
# bet = dbeta(p, 8, .8, ncp = 0)
# p = p[!is.infinite(bet)]
# bet = bet[!is.infinite(bet)]
#
# x = dist1$x[2:length(dist1$x)]
# x = x/max(x)
# prob = dist1$prob[2:length(dist1$prob)]
# prob = prob/max(prob)
#
# plot(x/max(x), prob/max(prob), col=2)
# sasa
#
# lines(p, bet/max(bet), ylab="density")
#
# fit = fitdistrplus::fitdist(prob, "beta")
#
# plot(fit, las = 1)
ecdf(daohProp)

asasas

daohProp = inData[daoh != 0,daoh/max(daoh)]
my_data = daohProp[daohProp<0.9]
my_data = daohProp
my_data = max(my_data) - my_data + 0.1
my_data = my_data/max(my_data)
plotdist(my_data)
descdist(my_data, discrete=FALSE, boot=500)
fit_w  <- fitdist(my_data, "weibull")
fit_g  <- fitdist(my_data, "gamma")
fit_ln <- fitdist(my_data, "lnorm")
fit_beta <- fitdist(my_data, "beta")
summary(fit_beta)
# asassa
# fit_w  <- fitdist(my_data, "weibull")
# fit_g  <- fitdist(my_data, "gamma")
# fit_ln <- fitdist(my_data, "lnorm")
# fit_beta <- fitdist(my_data, "beta")
# summary(fit_beta)

par(mfrow=c(2,2))
plot.legend <- c("Weibull", "lognormal", "gamma", "beta")
denscomp(list(fit_w, fit_g, fit_ln, fit_beta), legendtext = plot.legend)
cdfcomp (list(fit_w, fit_g, fit_ln, fit_beta), legendtext = plot.legend)
qqcomp  (list(fit_w, fit_g, fit_ln, fit_beta), legendtext = plot.legend)
ppcomp  (list(fit_w, fit_g, fit_ln, fit_beta), legendtext = plot.legend)
