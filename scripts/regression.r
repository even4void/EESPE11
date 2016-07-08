## regression.R
## Illustration pour le modèle linéaire
##

source("./mobilite.R")

library(MASS)

d <- mvrnorm(6053, mu = rep(0, 8), Sigma = mobility.cov, empirical = TRUE)

cov(d)

d <- as.data.frame(d)


m <- sem(mobility.model, data = d)

b <- coef(m)

pp <- predict(m)

pp <- as.data.frame(pp)

library(ggplot2)

p <- ggplot(data = pp, 
            aes(x = PsychSocLV, y = PsyHealthLV))
p + geom_point()

p <- ggplot(data = pp, aes(x = PsychSocLV))
p + geom_histogram()

tot <- apply(d[,1:4], 1, mean)

dd <- data.frame(total = tot, factor = pp$PsychSocLV)

p <- ggplot(data = dd, 
            aes(x = total, y = factor))
p + geom_point()

library(reshape2)

dm <- melt(dd)

p <- ggplot(data = dm, aes(x = value))
p + geom_histogram() + facet_grid(~ variable)

aggregate(value ~ variable, dm, var)

pp$PersMobility <- d$PersMobility

reg <- lm(PersMobility ~ PsychSocLV + PsyHealthLV, data = pp)
summary(reg)

b[6:7]

library(GGally)

ggpairs(pp)
