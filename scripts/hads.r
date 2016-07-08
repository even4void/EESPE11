## Analyse des données HADS
## Bertolucci et al. 2015
load("data/HADS.RData")

head(data)

table(data$Y1)
table(data$Y2)
table(data$Y3)

summary(data)

## ACP
library(FactoMineR)
pca <- PCA(data, nfactors = 14)

pca$eig        ## valeurs propres
pca$var$coord  ## loadings

## Analyse d'items
dep <- c(5,8,9,11,12,13,14)
anx <- seq(1,14)[-c(5,8,9,11,12,13,14)]

summary(data[,dep])

round(cor(data[,dep]), 3)

data$dep <- rowSums(data[,dep])  ## score total depression
data$anx <- rowSums(data[,anx])

round(cor(data[,c(dep, 15)]), 3) ## corrélation item/total dépression

cor(data$dep, data$anx)          ## corrélation dépression/anxiété

library(psych)
alpha(data[,dep])
alpha(data[,anx])

## EFA (1, 2 et 3 facteurs)
fa(data[,1:14], nfactors = 1)
fa(data[,1:14], nfactors = 2)
fa(data[,1:14], nfactors = 3)

## CFA
library(lavaan)
m <- 'Dep =~ Y5 + Y8 + Y9 + Y11 + Y12 + Y13 + Y14 
      Anx =~ Y1 + Y2 + Y3 + Y4 + Y6 + Y7 + Y10'
r <- cfa(data = data[,1:14], model = m, std.lv = TRUE)
r
summary(r, fit.measures = TRUE, standardized = TRUE)

## analyse des indices de modification
summary(r, fit.measures = TRUE, standardized = TRUE, modindices = TRUE)
