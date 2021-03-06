## Una vez obtenidos los resultados de la clasificación, qué nos dicen estos datos:

pl  <- read.table('clustering_output.txt')

## amount of days in each cluster:
 
type  <- lapply(1:12, FUN=function(x) sum(pl$km == x))
type_pcg  <- lapply(1:12, FUN=function(x) (type[[x]]/sum(unlist(type))*100))

type_pcg  <- do.call(rbind, type_pcg)
write.table(type_pcg, file='percentage_per_cluster.txt')

## Statistical values of pv in each cluster:

l  <- lapply(1:12, FUN=function(x) summary(pl[pl$km == x,2]))

clusterStatistics  <- do.call(rbind,l)
write.table(clusterStatistics, file='clusterStatistics.txt')

## cluster 12

km <- lapply(1:12, FUN=function(x) {pl[pl$km == x,]})
names(km)  <- lapply(1:12, FUN=function(i) paste('cluster', i, sep="_"))
 
sapply(names(km), function(x)
    write.table(km[[x]], file=paste(x, "txt", sep="."), col.names=TRUE, row.names=FALSE))

