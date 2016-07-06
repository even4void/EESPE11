## Analyse des données Holzinger & Swineford (1939)

library(lavaan)
data(HolzingerSwineford1939)

## renommage du tableau pour faciliter l'écriture
HS <- HolzingerSwineford1939

names(HS)[7:15] = c("visual", "cubes", "paper",
                    "paragrap", "sentence", "wordm",
                    "addition", "counting", "straight")

summary(HS[,c("visual", "cubes", "paper")])

## calcul des scores totaux pour chaque dimension
HS$spatial <- rowSums(HS[,c("visual","cubes","paper")])
HS$verbal <- rowSums(HS[,c("paragrap","sentence","wordm")])
HS$speed <- rowSums(HS[,c("addition","counting","straight")])

library(FactoMineR)

## ACP sur les variables de la dimension "spatial"
pca <- PCA(HS[,c("visual", "cubes", "paper")],
           scale.unit = TRUE, graph = FALSE)

pca$eig
pca$var

round(cor(HS[,7:15]), 2)

## ACP sur l'ensemble des variables
pca <- PCA(HS[,7:15], scale.unit=TRUE)
pca$eig
pca$var

## corrélation item / score total
cor(HS[,c(7:9,16)])

