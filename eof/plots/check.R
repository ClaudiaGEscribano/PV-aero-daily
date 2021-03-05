## Check that the cluster for different days represent similar patterns.

library(raster)
library(rasterVis)

## 1. load the cluster data

load("/home/claudia/ideas/PV-aero-daily/eof/output/slp/clusters_kmeans_yearly_slp.Rdata")

## 2. load the slp anom data

slp  <- stack("/home/claudia/ideas/PV-aero-daily/data/era5/spl/anom_slpdm_1989_2019.nc")
##mslp  <-  mean(slp)
##anom  <- slp-mslp

## 3. creo un vector con los días y el cluster:

cluster  <-  km$cluster
cluster  <-  as.data.frame(cluster)
cluster$Date  <-  seq(as.Date("1989-01-01"), as.Date("2019-12-31"), "day")
rownames(cluster)  <- seq(1:11322)
## 4. Extraigo algunos de los clusters para representarlos

hist(cluster$cluster)

## cojo el 3 que es el que menos tiene.

c3  <- cluster[cluster$cluster == 3, ]

## tomo los 6 primeros días, que son las capas 48,49,54,55,56,60 de los datos de spl

selection  <- as.integer(rownames(c3)[1:20])

## 4. extraigo de los datos slp
 
levelplot(slp[[selection]])

## 5. el cluster que más hay es el 4

c4  <- cluster[cluster$cluster == 4, ]
selection  <- as.integer(rownames(c4)[100:110])
 
levelplot(slp[[selection]])

######### MAPA ####

## lineas costa
data('worldMapEnv')

ext <- extent(slp[[1]])
crs.lonlat  <- CRS("+proj=longlat +datum=WGS84 +no_defs")

boundaries_CI <- map('worldHires',
                     xlim = ext[1:2], ylim = ext[3:4],
                     fill=TRUE,
                     exact=FALSE,
                     plot = FALSE)
 
## Convert the result to a SpatialPolygons object:

IDs <- sapply(strsplit(boundaries_CI$names, ":"), function(x) x[1])
boundaries_spCI<- map2SpatialPolygons(boundaries_CI,
                                    IDs=IDs, proj4string=crs.lonlat)

 
## or convert the result to a SpatialLines object:
boundaries_linesCI <- map2SpatialLines(boundaries_CI,
                               proj4string = crs.lonlat)

#############################################################
 
## Representción de la media de todos los días:
c4  <- cluster[cluster$cluster == 4, ]
selection  <- as.integer(rownames(c4))
data  <- mean(slp[[selection]])

pdf("mean_pattern_c4_slp.pdf")
levelplot(data, par.settings=rasterTheme(region=rev(brewer.pal(11,'RdBu'))), contour=TRUE, at=seq(-1200,1200,100), margin=FALSE, main='C4')+layer(sp.lines(boundaries_linesCI))
dev.off()
 
c1  <- cluster[cluster$cluster == 1, ]
selection  <- as.integer(rownames(c1))
data  <- mean(slp[[selection]])

pdf("mean_pattern_c1_slp.pdf")
levelplot(data, margin=FALSE, par.settings=rasterTheme(region=rev(brewer.pal(11,'RdBu'))), contour=TRUE, at=seq(-2200,2200,200), main='C1')+layer(sp.lines(boundaries_linesCI))
dev.off()
 
c2  <- cluster[cluster$cluster == 2, ]
selection  <- as.integer(rownames(c2))
data  <- mean(slp[[selection]])

pdf("mean_pattern_c2_slp.pdf") 
levelplot(data, margin=FALSE, par.settings=rasterTheme(region=rev(brewer.pal(11,'RdBu'))), contour=TRUE, at=seq(-2200,2200,200), main='C2')+layer(sp.lines(boundaries_linesCI))
dev.off()

c3  <- cluster[cluster$cluster == 3, ]
selection  <- as.integer(rownames(c3))
data  <- mean(slp[[selection]])

pdf("mean_pattern_c3_slp.pdf")
levelplot(data,margin=FALSE, par.settings=rasterTheme(region=rev(brewer.pal(11,'RdBu'))), contour=TRUE, at=seq(-2600,2600,200), main='C3')+layer(sp.lines(boundaries_linesCI))
dev.off()
 
c5  <- cluster[cluster$cluster == 5, ]
selection  <- as.integer(rownames(c5))
data  <- mean(slp[[selection]])

pdf("mean_pattern_c5_slp.pdf")
levelplot(data,margin=FALSE, par.settings=rasterTheme(region=rev(brewer.pal(11,'RdBu'))), contour=TRUE,at=seq(-1700,1700,100), main='C5')+layer(sp.lines(boundaries_linesCI))
dev.off()

c6  <- cluster[cluster$cluster == 6, ]
selection  <- as.integer(rownames(c6))
data  <- mean(slp[[selection]])
 
pdf("mean_pattern_c6_slp.pdf")
levelplot(data,margin=FALSE, par.settings=rasterTheme(region=rev(brewer.pal(11,'RdBu'))), contour=TRUE,at=seq(-1600,1600,200), main='C6')+layer(sp.lines(boundaries_linesCI))
dev.off()

c7  <- cluster[cluster$cluster == 7, ]
selection  <- as.integer(rownames(c7))
data  <- mean(slp[[selection]])

pdf("mean_pattern_c7_slp.pdf")
levelplot(data,margin=FALSE, par.settings=rasterTheme(region=rev(brewer.pal(11,'RdBu'))), contour=TRUE,at=seq(-1400,1400,100), main='C7')+layer(sp.lines(boundaries_linesCI))
dev.off()
