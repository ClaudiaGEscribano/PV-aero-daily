library(factoExtra)
library(cluster)
library(mclust)

data  <- read.table("data4clustering.txt") 
data  <- read.table("data4clusteringCORRELATED.txt")
data  <- read.table("data4clusteringANOM.txt")
data  <- read.table("data4clustering_allcells.txt")

############################
## DATA MEAN
  
head(sum(is.na(data[,c(1,4,7,13,15,17,22)])) > 0)
 
data4pca  <- data[which(!is.na(data$Mean)),c(1,4,7,13,15,17,22)]  ##,c(1,2,3,4,5,6,8,10)]

dat  <- data4pca[,c(3:7)]

bic  <- Mclust(dat, G=1:30)
bic$BIC
plot(bic)

gmm  <-  bic$classification
save(gmm, file='gmm_bic_classifiation_data.Rdata')
#############################
## DATA CORRELATED

head(sum(is.na(data[,c(1,4,13,22,24,25,26)])) > 0)
 
data4pca  <- data[which(!is.na(data$Mean)),c(1,4,13,22,24,25,26)] ##,c(1,2,3,4,5,6,8,10)]

dat  <- data4pca[,c(3:7)]

bic  <- Mclust(dat, G=1:30)
bic$BIC
plot(bic)

gmm  <-  bic$classification
save(gmm, file='gmm_bic_classifiation_dataCORRELATED.Rdata')
#############################
## DATA ANOM

data4pca <- data[,2:6]

bic  <- Mclust(dat, G=1:30)
bic$BIC
plot(bic)

save(bic$classification, file='gmm_bic_classifiation_dataANOM.Rdata')
####################################
## DATA ALL CELLS. It considers each cell and its associated vector as a variable. It takes too long and no clear result is obtained.

## dat  <- data[which(!is.na(data$all.Mean)),] 

## bic  <- Mclust(dat, G=1:9)
## bic$BIC
## plot(bic)

## save(bic$classification, file='gmm_bic_classifiation_data_allcells.Rdata')


