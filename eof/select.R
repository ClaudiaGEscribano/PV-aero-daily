## Select the dates that I have for the analysis of the PV data:

library(raster)
library(rasterVis)

load("output/slp/clusters_kmeans_yearly_slp.Rdata")

cls  <-  km$cluster
idx  <- seq(as.Date('1989-01-01'),as.Date('2019-12-31'), "day")

cls  <- as.data.frame(cls)
cls$Date  <-  idx

i1  <- rownames(cls[cls$Date == '2010-01-01', ]) ## 7671
i2  <- rownames(cls[cls$Date == '2016-12-31', ]) ## 10227

cls  <- cls[7671:10227,]

write.csv(cls, file = "cls_eof_yearly.csv", row.names=FALSE) 
