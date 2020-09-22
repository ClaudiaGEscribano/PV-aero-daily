## Create clusters: obtaining classes for different days depending on variables like ghi,dni,aod...

data  <- read.table("data4clustering.txt")
data  <- data[366:nrow(data),]

############################
## PCA

head(sum(is.na(data[,c(2,3,4,5,6,8,10)])) > 0)

data4pca  <- data[which(!is.na(data$Mean)),c(1,2,3,4,5,6,8,10)]
 
datapca <- prcomp(data4pca[,2:7], center=TRUE, scale=TRUE) ## PCA
 
cumvar <- cumsum(datapca$sdev^2)/sum(datapca$sdev^2)
b <- which(cumvar < 0.96)
c <- length(b)
datapca <- data.frame(datapca$x[,1:c])
 
data.df <-  datapca

############################
## K-MEANS
 
## uso el método jerarquico antes:

dataMatrix <- as.matrix(data.df)
dataDist <- dist(dataMatrix) ## computes the distance matrix in order to use it in the hc algorithm

datahclus <- hclust(dataDist)

## I get the cut the clustering tree for k=2:50 clusters. I will decide the optimun number in next steps.

kut <- cutree(datahclus, k=2:50)

## I have to select the centroids of the clusters in order to use them in the kmeans clustering.

cluster.centroid <- function(datahc, data, n){ ## n es el numero de clusters max. menos uno.
         lapply(seq(from=1, to=n),
                FUN=function(x) lapply(seq(from=1, to=x+1),
                                       FUN=function(i){
                                           ind  <- which(datahc[,x]==i) ## cambio aqui  i por x
                                           if (length(ind) > 1) {
                                               r <- data[ind,]
                                               r <-  as.data.frame(r)
                                           }
                                           else {
                                               r <-  data.frame(t(data[ind,]))}
                                           c <- colMeans(r)
                                           return(c)}
                                       ))}

centroids <- cluster.centroid(kut, dataMatrix, 49) ## contiene una lista de 70 elementos. Cada uno de los elementos es otra lista con los valores de de los centroides, que son vectores de dimensión d igual a las columnas de data.

centros <- lapply(seq(from=1, to=49),
                  FUN=function(i) do.call(rbind, centroids[[i]]))

km_exp <- lapply(centros,
                    FUN=function(i) kmeans(dataMatrix, centers=i, iter.max=3000))

save(km_exp, file='hckm_exp.Rdata')

############################
## optimo:

## L-method (visual):

wss <-  c(2:49)
for (i in 2:49) wss[i] <- sum(km_exp[[i]]$withinss)
  
pdf("plot_L_visual_hckm.pdf")
plot(1:49, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")
dev.off() 

library(clusterCrit)

criterioDB <- lapply(seq(from=1, to=49), 
	FUN= function(x) intCriteria(dataMatrix, km_exp[[x]]$cluster, 'Davies_Bouldin')
                     )
 
criterioC <- lapply(seq(from=1, to=49), 
	FUN= function(x) intCriteria(dataMatrix, km_exp[[x]]$cluster, 'C_index')
)

criterioCH <- lapply(seq(from=1, to=49), 
	FUN= function(x) intCriteria(dataMatrix, km_exp[[x]]$cluster, 'Calinski_Harabasz')
                     )
criterioSH <- lapply(seq(from=1, to=49), 
	FUN= function(x) intCriteria(dataMatrix, km_exp[[x]]$cluster, 'Silhouette')
)

db <- do.call(rbind, criterioDB)
c <- do.call(rbind, criterioC)
ch <- do.call(rbind, criterioCH)
sh <- do.call(rbind, criterioSH)

plot(db, type='b')

## BIC:

bic  <- Mclust(data.df)
bic$BIC
plot(bic)
########################
## ajuste a rectas:
rmse <- list()
 
ajuste  <- function(x){
       for(j in 1:46){
        r1L <- lm(x[1:j+1]~c(1:j+1))
        r1R <- lm(x[j+2:49]~c(j+2:49))
	rmse[[j]] <- c(sqrt(sum((r1L$residuals)^2)), sqrt(sum((r1R$residuals)^2)))}
	return(rmse)
}

## I apply this 'ajuste' function to every element of 'Indice' (they are lists of kmeans results)

rmse_exp <- ajuste(as.vector(unlist(criterioDB)))

## Ponderate the rmse from lm results.
ajuste_ponderado  <- function(x){
	rmseT <- c()
	for(i in 1:46)
	rmseT[i] <-((i+1)-1)/((49)-1)*(x[[i]][1])+(49-(i+1))/(49-1)*(x[[i]][2])
	return(rmseT)
}

rmse_expP <- ajuste_ponderado(rmse_exp)

minimo <- which(rmse_expP == min(rmse_expP))

############################
##some plots:

km  <- km_exp[[11]]$cluster
 
pl  <- cbind(data4pca, km)
pl  <- cbind(pl, data.df)

library(RColorBrewer)
library(lattice)

myTheme  <- brewer.pal(12,"Paired")

xyplot(pv~ghidfm|km, groups=km, pch=20, auto.key=TRUE,cex=1,data=pl,
       key=list(columns=12, space='top',cex=1,between=0.5,
               points=list(col=myTheme, pch=20,cex=1.3)),
       par.settings=list(superpose.symbol=list(col=myTheme, pch=20, cex=0.5)))

xyplot(ghidfm~Mean|km, groups=km, pch=20, auto.key=TRUE,cex=1,data=pl,
       key=list(columns=12, space='top',cex=1,between=0.5,
               points=list(col=myTheme, pch=20,cex=1.3)),
       par.settings=list(superpose.symbol=list(col=myTheme, pch=20, cex=0.5)))

xyplot(pv~Mean|km, groups=km, pch=20, auto.key=TRUE,cex=1,data=pl,
       key=list(columns=12, space='top',cex=1,between=0.5,
               points=list(col=myTheme, pch=20,cex=1.3)),
       par.settings=list(superpose.symbol=list(col=myTheme, pch=20, cex=0.5)))

xyplot(ghidfm~cfcdfm, groups=km, pch=20, auto.key=TRUE,cex=1,data=pl,
       key=list(columns=12, space='top',cex=1,between=0.5,
               points=list(col=myTheme, pch=20,cex=1.3)),
       par.settings=list(superpose.symbol=list(col=myTheme, pch=20, cex=0.5)))

xyplot(pv~Mean, groups=km, pch=20, auto.key=TRUE,cex=1,data=pl,
       key=list(columns=12, space='top',cex=1,between=0.5,
               points=list(col=myTheme, pch=20,cex=1.3)),
       par.settings=list(superpose.symbol=list(col=myTheme, pch=20, cex=0.5)))

xyplot(Mean~cfcdfm, groups=km, pch=20, auto.key=TRUE,cex=1,data=pl,
       key=list(columns=12, space='top',cex=1,between=0.5,
               points=list(col=myTheme, pch=20,cex=1.3)),
       par.settings=list(superpose.symbol=list(col=myTheme, pch=20, cex=0.5)))


########################
## density plot:

densityplot(~pv, groups=km, data=pl, plot.points=FALSE, lwd=3, auto.key=TRUE, par.settings=list(col=myTheme))

## eliminio los datos del cluster 11
 
#pl2  <-  pl[pl$km != 12,]
pdf("pvdensity_bycluster_km12.pdf")
densityplot(~pv, groups=km, data=pl, plot.points=FALSE, lwd=4,
            key=list(columns=12, space='top',cex=1, between=0.5,
                     points=list(col=myTheme, pch=20, cex=2)),
            par.settings=list(superpose.line=list(col=myTheme, lwd=3)))
dev.off()

pdf("pvdensity_bycluster_separatekm12.pdf")
densityplot(~pv|as.factor(km), groups=km, data=pl, plot.points=FALSE, lwd=4,
            key=list(columns=12, space='top',cex=1, between=0.5,
                     points=list(col=myTheme, pch=20, cex=2)),
            par.settings=list(superpose.line=list(col=myTheme, lwd=3)))
dev.off()
 
densityplot(~pv|Month, groups=km, data=pl, plot.points=FALSE, lwd=4,
            key=list(columns=12, space='top',cex=1, between=0.5,
                     points=list(col=myTheme, pch=20, cex=2)),
            par.settings=list(superpose.line=list(col=myTheme, lwd=3)))

pdf("AODdensity_byclusterkm12.pdf")
densityplot(~Mean, groups=km, data=pl, plot.points=FALSE, lwd=4,
            key=list(columns=12, space='top',cex=1, between=0.5,
                     points=list(col=myTheme, pch=20, cex=2)),
            par.settings=list(superpose.line=list(col=myTheme, lwd=3)))
dev.off()

pdf("AODdensity_bycluster_separatekm12.pdf")
densityplot(~Mean|as.factor(km), groups=km, data=pl, plot.points=FALSE, lwd=4,
            key=list(columns=12, space='top',cex=1, between=0.5,
                     points=list(col=myTheme, pch=20, cex=2)),
            par.settings=list(superpose.line=list(col=myTheme, lwd=3)))
dev.off()

pdf("cfcdensity_byclusterkm12.pdf")
densityplot(~cfcdfm, groups=km, data=pl, plot.points=FALSE, lwd=4,
            key=list(columns=12, space='top',cex=1, between=0.5,
                     points=list(col=myTheme, pch=20, cex=2)),
            par.settings=list(superpose.line=list(col=myTheme, lwd=3)))
dev.off()

pdf("cfcdensity_byclusterSeparatekm12.pdf")
densityplot(~cfcdfm|as.factor(km), groups=km, data=pl, plot.points=FALSE, lwd=4,
            key=list(columns=12, space='top',cex=1, between=0.5,
                     points=list(col=myTheme, pch=20, cex=2)),
            par.settings=list(superpose.line=list(col=myTheme, lwd=3)))
dev.off()

########################################
pdf("matrix_bycluster.pdf")
splom(pl[,c(1,2,3,4,6,7)],groups=km, par.settings=list(superpose.symbol=list(col=myTheme, pch=20)))
dev.off()


######################################
pdf("histogram_pv_bycluster.pdf")
histogram(~pv|as.factor(km), data=pl, breaks=5)
dev.off()

pdf("histogram_cfc_bycluster.pdf")
histogram(~cfcdfm|as.factor(km), data=pl, breaks=5)
dev.off()

#####################################

month  <- function(x) format(as.Date(x), '%m')

pl$Month  <- as.factor(month(pl$Date))
##pl2  <-  pl[pl$km != 11,]
 
densityplot(~cfcdfm|Month, groups=km, data=pl, plot.points=FALSE, lwd=4,
            key=list(columns=12, space='top',cex=1, between=0.5,
                     points=list(col=myTheme, pch=20, cex=2)),
            par.settings=list(superpose.line=list(col=myTheme, lwd=3)))

xyplot(Mean~dnidfm|Month, groups=km, pch=20,data=pl, lwd=4,
            key=list(columns=12, space='top',cex=1, between=0.5,
                     points=list(col=myTheme, pch=20, cex=1.5)),
       par.settings=list(superpose.symbol=list(col=myTheme, cex=2,lwd=3)))

pdf("AOD_cfc_bycluster_bymonth.pdf", width=10, height=7)
xyplot(Mean~cfcdfm|Month, groups=km, pch=20,data=pl, lwd=4,
            key=list(columns=12, space='top',cex=1, between=0.5,
                     points=list(col=myTheme, pch=20, cex=1.5)),
       par.settings=list(superpose.symbol=list(col=myTheme, cex=1.5,lwd=3)))
dev.off()

pdf("pv_AOD_bycluster_bymonth.pdf", width=10, height=7)
xyplot(pv~Mean|Month, groups=km, pch=20,data=pl, lwd=4,
            key=list(columns=12, space='top',cex=1, between=0.5,
                     points=list(col=myTheme, pch=20, cex=1.5)),
       par.settings=list(superpose.symbol=list(col=myTheme, cex=2,lwd=3)))
dev.off()

pdf("pv_kt_bycluster_bymonth.pdf", width=10, height=7)
xyplot(pv~Kt|Month, groups=km, pch=20,data=pl, lwd=4,
            key=list(columns=12, space='top',cex=1, between=0.5,
                     points=list(col=myTheme, pch=20, cex=1.5)),
       par.settings=list(superpose.symbol=list(col=myTheme, cex=2,lwd=3)))
dev.off()

pdf("kt_AOD_bycluster_bymonth.pdf")
xyplot(Kt~Mean|Month, groups=km, pch=20,data=pl, lwd=4,
            key=list(columns=12, space='top',cex=1, between=0.5,
                     points=list(col=myTheme, pch=20, cex=1.5)),
       par.settings=list(superpose.symbol=list(col=myTheme, cex=2,lwd=3)))
dev.off()

pdf("cfcdfm_kt_bycluster_bymonth.pdf")
xyplot(cfcdfm~Kt|Month, groups=km, pch=20,data=pl, lwd=4,
            key=list(columns=12, space='top',cex=1, between=0.5,
                     points=list(col=myTheme, pch=20, cex=1.5)),
       par.settings=list(superpose.symbol=list(col=myTheme, cex=2,lwd=3)))
dev.off()

xyplot(pv~taxdfm|Month, groups=km, pch=20,data=pl, lwd=4,
            key=list(columns=12, space='top',cex=1, between=0.5,
                     points=list(col=myTheme, pch=20, cex=1.5)),
       par.settings=list(superpose.symbol=list(col=myTheme, cex=2,lwd=3)))


xyplot(ghidfm~taxdfm|km, groups=km, pch=20,data=pl, lwd=4,
            key=list(columns=12, space='top',cex=1, between=0.5,
                     points=list(col=myTheme, pch=20, cex=1.5)),
       par.settings=list(superpose.symbol=list(col=myTheme, cex=2,lwd=3)))


xyplot(dnidfm~ghidfm|Month, groups=km, pch=20,data=pl, lwd=4,
            key=list(columns=12, space='top',cex=1, between=0.5,
                     points=list(col=myTheme, pch=20, cex=1.5)),
       par.settings=list(superpose.symbol=list(col=myTheme, cex=2,lwd=3)))


xyplot(Kt~ghidfm|Month, groups=Month, pch=20,data=pl, lwd=4,
            key=list(columns=12, space='top',cex=1, between=0.5,
                     points=list(col=myTheme, pch=20, cex=1.5)),
       par.settings=list(superpose.symbol=list(col=myTheme, cex=2,lwd=3)))
 
xyplot(Kt~taxdfm|Month, groups=Month, pch=20,data=pl, lwd=4,
            key=list(columns=12, space='top',cex=1, between=0.5,
                     points=list(col=myTheme, pch=20, cex=1.5)),
       par.settings=list(superpose.symbol=list(col=myTheme, cex=2,lwd=3)))


write.table(pl, file='clustering_output.txt')

## plot PCS
pdf("PCS_scatterkm12.pdf")
splom(pl[,c(10,11,12,13)], groups=km, pch=20, lwd=4,
            key=list(columns=12, space='top',cex=1, between=0.5,
                     points=list(col=myTheme, pch=20, cex=1.5)),
       par.settings=list(superpose.symbol=list(col=myTheme, cex=1,lwd=3)))
dev.off()

splom(pl[,c(2,3,4,5,7,8)], groups=km, pch=20, lwd=4,
            key=list(columns=12, space='top',cex=1, between=0.5,
                     points=list(col=myTheme, pch=20, cex=1.5)),
       par.settings=list(superpose.symbol=list(col=myTheme, cex=1,lwd=3)))


xyplot(pv~cfcdfm|Month, groups=km, pch=20,data=pl, lwd=4,
            key=list(columns=12, space='top',cex=1, between=0.5,
                     points=list(col=myTheme, pch=20, cex=1.5)),
       par.settings=list(superpose.symbol=list(col=myTheme, cex=2,lwd=3)))

xyplot(pv~Kt|Month, groups=km, pch=20,data=pl, lwd=4,
            key=list(columns=12, space='top',cex=1, between=0.5,
                     points=list(col=myTheme, pch=20, cex=1.5)),
       par.settings=list(superpose.symbol=list(col=myTheme, cex=2,lwd=3)))

xyplot(cfcdfm~Kt|Month, groups=km, pch=20,data=pl, lwd=4,
            key=list(columns=12, space='top',cex=1, between=0.5,
                     points=list(col=myTheme, pch=20, cex=1.5)),
       par.settings=list(superpose.symbol=list(col=myTheme, cex=2,lwd=3)))
