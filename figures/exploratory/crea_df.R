## Create a data.frame with all the variables to investigate.

library(dplyr)
library(reshape2)
library(latticeExtra)
library(ggplot2)

## load pv
pv  <-  read.table("../../data/pv/ree/elec_day_ree_2011_2013.txt")
pv$Date  <-  seq(as.Date('2011-01-01'),as.Date('2013-12-31'), "day") 

## load aod

aod  <-  read.table("../../data/aero/aeronet/aod_aeronet_all_CI_20102013.txt")
aod$Date  <-  as.Date(aod$Date)

## merge:

df  <- pv %>%
    merge(aod, by='Date', all=TRUE) 

month  <-  function(x) {format(as.Date(x), '%m')}
df$Month  <-  month(df$Date)

m  <-  melt(df, 'Date')

## try to plot scatters.

## all the data:

splom(~df[,c(3,4,10)]|Month, data=df, cex=0.3)

## elimino días por debao de 0.2

splom(~df[df$Mean >= 0.2,c(3,4,10)]|Month, data=df, cex=0.3)
xyplot(Mean~pv|Month, data=df, groups=Month, cex=0.6, auto.key=TRUE)

## by month:

s  <-  df %>%
    group_by('Month')


xyplot(value~Date, group=variable, data=m, type='b', cex=0.5, alpha=0.8, auto.key=TRUE, grid=TRUE)



