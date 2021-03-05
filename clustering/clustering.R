## Create clusters: obtaining classes for different days depending on variables like ghi,dni,aod...

data  <- read.table("data4clustering.txt")
data  <- read.table("data4clusteringCORRELATED.txt")
data  <- read.table("data4clusteringANOM.txt")

##data  <- data[366:nrow(data),]

############################
## PCA
  
head(sum(is.na(data[,c(1,4,7,13,15,17,22)])) > 0)
 
data4pca  <- data[which(!is.na(data$Mean)),c(1,4,7,13,15,17,22)] ##,c(1,2,3,4,5,6,8,10)]
 
datapca <- prcomp(data4pca[,3:7], center=TRUE, scale=TRUE) ## PCA
 
cumvar <- cumsum(datapca$sdev^2)/sum(datapca$sdev^2)
b <- which(cumvar < 0.9)
c <- length(b)
datapca <- data.frame(datapca$x[,1:c])
 
data.df <-  datapca

#############################
## PCA WITH CORRELATED DATA

head(sum(is.na(data[,c(1,4,13,22,24,25,26)])) > 0)
 
data4pca  <- data[which(!is.na(data$Mean)),c(1,4,13,22,24,25,26)] ##,c(1,2,3,4,5,6,8,10)]
 
datapca <- prcomp(data4pca[,3:7], center=TRUE, scale=TRUE) ## PCA
 
cumvar <- cumsum(datapca$sdev^2)/sum(datapca$sdev^2)
b <- which(cumvar < 0.9)
c <- length(b)
datapca <- data.frame(datapca$x[,1:c])
 
data.df <-  datapca

#############################
## PCA WITH ANOMALIES

datapca <- prcomp(data4pca[,2:6], center=TRUE, scale=TRUE) ## PCA
 
cumvar <- cumsum(datapca$sdev^2)/sum(datapca$sdev^2)
b <- which(cumvar < 0.9)
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

save(km_exp, file='hckm_exp_correlated.Rdata') ## save each cluster classification

############################
## número óptimo de clusters:

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

plot(ch, type='b')

########################
## ajuste a rectas para encontrar el punto de inflexión:
rmse <- list()
 
ajuste  <- function(x){
       for(j in 1:46){
        r1L <- lm(x[1:j+1]~c(1:j+1))
        r1R <- lm(x[j+2:49]~c(j+2:49))
	rmse[[j]] <- c(sqrt(sum((r1L$residuals)^2)), sqrt(sum((r1R$residuals)^2)))}
	return(rmse)
}

## I apply this 'ajuste' function to every element of 'Indice' (they are lists of kmeans results)

rmse_exp <- ajuste(as.vector(unlist(criterioC)))

## Ponderate the rmse from lm results.
ajuste_ponderado  <- function(x){
	rmseT <- c()
	for(i in 1:46)
	rmseT[i] <-((i+1)-1)/((49)-1)*(x[[i]][1])+(49-(i+1))/(49-1)*(x[[i]][2])
	return(rmseT)
}

rmse_expP <- ajuste_ponderado(rmse_exp)

minimo <- which(rmse_expP == min(rmse_expP))


