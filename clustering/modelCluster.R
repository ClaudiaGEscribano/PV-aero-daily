## Crea modelos para cada uno de los clusters:

## Empiezo a partir del cluster 8 (naranja).

km8  <- read.table("cluster_8.txt", header=TRUE)

myTheme  <- brewer.pal(12,"Paired")
splom(km8[,c(2,3,4,7)], par.settings=list(superpose.symbol=list(col=myTheme, pch=20)))

km9  <- read.table("cluster_9.txt", header=TRUE)
splom(km9[,c(2,3,4,5,7)], par.settings=list(superpose.symbol=list(col=myTheme, pch=20)))
summary(lm(km9$pv~km9$ghidfm+km9$Mean+km9$cfcdfm)+km9$Kt))

km10  <- read.table("cluster_10.txt", header=TRUE)
splom(km10[,c(2,3,4,7)], par.settings=list(superpose.symbol=list(col=myTheme, pch=20)))

km11  <- read.table("cluster_11.txt", header=TRUE)
splom(km11[,c(2,3,4,7)], par.settings=list(superpose.symbol=list(col=myTheme, pch=20)))

km1  <- read.table("cluster_1.txt", header=TRUE)
splom(km1[,c(2,3,4,7)], par.settings=list(superpose.symbol=list(col=myTheme, pch=20)))

km2  <- read.table("cluster_2.txt", header=TRUE)
splom(km2[,c(2,3,4,7)], par.settings=list(superpose.symbol=list(col=myTheme, pch=20)))

km6  <- read.table("cluster_6.txt", header=TRUE)
splom(km6[,c(2,3,4,7)], par.settings=list(superpose.symbol=list(col=myTheme, pch=20)))

km5  <- read.table("cluster_5.txt", header=TRUE)
splom(km5[,c(2,3,4,7)], par.settings=list(superpose.symbol=list(col=myTheme, pch=20)))


summary(lm(km2$pv~km2$ghidfm+km2$Mean+km2$Kt))
summary(lm(km8$pv~km8$ghidfm+km8$Mean+km8$Kt))
summary(lm(km6$pv~km6$ghidfm+km6$Mean+km6$Kt))
summary(lm(km9$pv~km9$ghidfm+km9$Mean+km9$Kt))
summary(lm(km9$pv~km9$ghidfm+km9$Mean+km9$Kt))

splom(km5[,c(2,9,10,11)], par.settings=list(superpose.symbol=list(col=myTheme, pch=20)))
summary(lm(km5$pv~km5$PC1+km5$PC2+km5$PC3))

splom(km8[,c(2,9,10,11)], par.settings=list(superpose.symbol=list(col=myTheme, pch=20)))
summary(lm(km8$pv~km8$PC1+km8$PC2+km8$PC3))

splom(km1[,c(2,9,10,11)], par.settings=list(superpose.symbol=list(col=myTheme, pch=20)))
summary(lm(km1$pv~km1$PC1+km1$PC2+km1$PC3))

km4  <- read.table("cluster_4.txt", header=TRUE)
splom(km4[,c(2,9,10,11)], par.settings=list(superpose.symbol=list(col=myTheme, pch=20)))
summary(lm(km4$pv~km4$PC1+km4$PC2+km4$PC3))

splom(km9[,c(2,9,10,11)], par.settings=list(superpose.symbol=list(col=myTheme, pch=20)))
summary(lm(km9$pv~km9$PC1+km9$PC2+km9$PC3))

splom(km10[,c(2,9,10,11)], par.settings=list(superpose.symbol=list(col=myTheme, pch=20)))
summary(lm(km10$pv~km10$PC1+km10$PC2+km10$PC3))

splom(km3[,c(2,9,10,11)], par.settings=list(superpose.symbol=list(col=myTheme, pch=20)))
summary(lm(km3$pv~km3$PC1+km3$PC2+km3$PC3))

splom(km2[,c(2,9,10,11)], par.settings=list(superpose.symbol=list(col=myTheme, pch=20)))
summary(lm(km2$pv~km2$PC1+km2$PC2+km2$PC3))
