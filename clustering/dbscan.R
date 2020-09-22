## Clustering using dbscan method

library(dbscan)
library(mclust)


data  <- read.table("data4clustering.txt")
data  <- data[366:nrow(data),]

data4clus  <- data[which(!is.na(data$Mean)),1:6]

################################################
data  <- data.df

##
db  <- dbscan(data4clus[,2:5], eps=0.05, minPts = 5)

dbscan::kNNdistplot(data4clus[,2:5], k =  5)
abline(h = 6, lty = 2)

db  <- dbscan(data4clus[,2:5], eps=5, minPts = 5)

pairs(data4clus[,2:5], col = db$cluster + 1L)

##
db  <- dbscan(data, eps=0.05, minPts = 5)

dbscan::kNNdistplot(data, k =  5)
abline(h = 1, lty = 2)

db  <- dbscan(data,, eps=1, minPts = 5)

pairs(data, col = db$cluster + 1L)

###################################################

BIC <- mclustBIC(data4clus[,2:5])
summary(BIC)
plot(BIC)
 
mod1 <- Mclust(data4clus[,2:5], x = BIC)
summary(mod1, parameters = TRUE)

plot(mod1, what = "classification")

data4clus$cluster  <- mod1$classification

plot(mod1, what = "uncertainty")

#############

ICL <- mclustICL(data4clus[,2:5])
summary(ICL)

plot(ICL)

########################

mod2 <- MclustDA(data4clus[,2:5], class, modelType = "EDDA")

#######################

hc1 <- hc(X, modelName = "VVV", use = "SVD")
## Call:
## hc(data = X, modelName = "VVV", use = "SVD") 
## 
## Model-Based Agglomerative Hierarchical Clustering 
## Model name        = VVV 
## Use               = SVD 
## Number of objects = 145
BIC1 <- mclustBIC(X, initialization = list(hcPairs = hc1)) # default 
summary(BIC1)
## Best BIC values:
##              VVV,3       VVV,4       EVE,6
## BIC      -4751.316 -4784.32213 -4785.24591
## BIC diff     0.000   -33.00573   -33.92951

(hc2 <- hc(X, modelName = "VVV", use = "VARS"))
## Call:
## hc(data = X, modelName = "VVV", use = "VARS") 
## 
## Model-Based Agglomerative Hierarchical Clustering 
## Model name        = VVV 
## Use               = VARS 
## Number of objects = 145
BIC2 <- mclustBIC(X, initialization = list(hcPairs = hc2))
summary(BIC2)
## Best BIC values:
##              VVV,3       VVE,3       EVE,4
## BIC      -4760.091 -4775.53693 -4793.26143
## BIC diff     0.000   -15.44628   -33.17079

(hc3 <- hc(X, modelName = "EEE", use = "SVD"))
## Call:
## hc(data = X, modelName = "EEE", use = "SVD") 
## 
## Model-Based Agglomerative Hierarchical Clustering 
## Model name        = EEE 
## Use               = SVD 
## Number of objects = 145
BIC3 <- mclustBIC(X, initialization = list(hcPairs = hc3))
summary(BIC3)
## Best BIC values:
##              VVV,3        VVE,4       VVE,3
## BIC      -4751.354 -4757.091572 -4775.69587
## BIC diff     0.000    -5.737822   -24.34212
Update BIC by merging the best results:

BIC <- mclustBICupdate(BIC1, BIC2, BIC3)
summary(BIC)
## Best BIC values:
##              VVV,3        VVE,4       VVE,3
## BIC      -4751.316 -4757.091572 -4775.53693
## BIC diff     0.000    -5.775172   -24.22053
plot(BIC)

####################

mod5 <- densityMclust(X)
summary(mod5)

plot(mod5, what="BIC")

plot(mod5, what = "density")

plot(mod5, what = "density", type = "hdr")

plot(mod5, what = "density", type = "persp")

#############################

mod1dr <- MclustDR(mod1, lambda=1)
summary(mod1dr)
## ----------------------------------------------------------------- 
## Dimension reduction for model-based clustering and classification 
## ----------------------------------------------------------------- 
## 
## Mixture model type: Mclust (VVV, 3) 
##         
## Clusters  n
##        1 81
##        2 36
##        3 28
## 
## Estimated basis vectors: 
##              Dir1     Dir2      Dir3
## glucose -0.988671  0.76532 -0.966565
## insulin  0.142656 -0.13395  0.252109
## sspg    -0.046689  0.62955  0.046837
## 
##                Dir1     Dir2      Dir3
## Eigenvalues  1.3506  0.75608   0.53412
## Cum. %      51.1440 79.77436 100.00000
plot(mod1dr, what = "pairs")

plot(mod1dr, what = "boundaries", ngrid = 200)


###############3
Class <- factor(wine$Class, levels = 1:3,
                labels = c("Barolo", "Grignolino", "Barbera"))

X <- data.matrix(wine[,-1])
mod <- Mclust(X)
summary(mod$BIC)

plot(mod, what = "BIC", ylim = range(mod$BIC[,-(1:2)], na.rm = TRUE),
legendArgs = list(x = "bottomleft"))
