## Analyses factorielles des données HS


library(psych)

## ACP
principal(HS[,c("visual", "cubes", "paper")], nfactors = 3, rotate = "none")

## AF
fa(HS[,c("visual", "cubes", "paper")], nfactors = 1)

## AF avec le package lavaan
library(lavaan)

d <- HS[,7:15]
describe(d)

m <- 'Visual =~ visual + cubes + paper
      Verbal =~ paragrap + sentence + wordm
      Speed  =~ addition + counting + straight'

r <- cfa(m, data = d)

summary(r, fit.measures = TRUE)

