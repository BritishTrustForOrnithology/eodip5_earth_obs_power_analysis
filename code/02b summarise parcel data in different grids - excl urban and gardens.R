#analyse Norfolk Map parcels within different grid resolutions, optional removal of some very common habitats
#Simon Gillings, modified by Stuart Newson
#February 2016
library(plyr)

# LOAD DATA ---------------------------------------------------------------
#parcel data
df0<-readRDS('X:\\Shared Projects\\Defra80 - EODIP5\\gis analysis\\parcels_with_grids.rds')

# CLEANING AND SUMMARISING ---------------------------------------------------------------
#combine the two habs as agreed by JNCC
df$CLASS<-ifelse(df$CLASS=='Semi-improved (poor condition)','Semi-improved grassland',df$CLASS)

# FILTER TO REMOVE SOME OF THE COMMONEST HABITATS -----------------------------------------------------
# Remove either Gardens, Urban, Arable, Gardens & Urban, Gardens, Urban and Arable
#df <- subset(df0, CLASS!="Gardens")
#df <- subset(df0, CLASS!="Urban")
#df <- subset(df0, CLASS!="Arable")
#df <- subset(df0, CLASS!="Gardens"& CLASS!="Urban")
df <- subset(df0, CLASS!="Gardens"& CLASS!="Urban" & CLASS!="Arable") 

# PROCESS AS NORMAL TO EXTRACT SUMMARY STATS ----------------------------------------------------------
#number of parcels per grid
#nparcel10km<-aggregate(data=dftemp,count~ref10km,NROW)
nparcel01km<-aggregate(data=df,count~ref01km,NROW)
nparcel500m<-aggregate(data=df,count~ref500m,NROW)
nparcel200m<-aggregate(data=df,count~ref200m,NROW)
nparcel100m<-aggregate(data=df,count~ref100m,NROW)

#get numbers of squares with habitat
#get habs
habs<-(unique(df$CLASS))
habs<-habs[order(habs)]



#get stats on number of all patches per grid square
stats01km<-c('1km',quantile(nparcel01km$count,0.10),quantile(nparcel01km$count,0.25),quantile(nparcel01km$count,0.5),quantile(nparcel01km$count,0.75),quantile(nparcel01km$count,0.90))
stats500m<-c('500m',quantile(nparcel500m$count,0.10),quantile(nparcel500m$count,0.25),quantile(nparcel500m$count,0.5),quantile(nparcel500m$count,0.75),quantile(nparcel500m$count,0.90))
stats200m<-c('200m',quantile(nparcel200m$count,0.10),quantile(nparcel200m$count,0.25),quantile(nparcel200m$count,0.5),quantile(nparcel200m$count,0.75),quantile(nparcel200m$count,0.90))
stats100m<-c('100m',quantile(nparcel100m$count,0.10),quantile(nparcel100m$count,0.25),quantile(nparcel100m$count,0.5),quantile(nparcel100m$count,0.75),quantile(nparcel100m$count,0.90))

n_patches_per_grid<-rbind(stats01km,stats500m,stats200m,stats100m)
table.loc<-paste0('X:\\Shared Projects\\Defra80 - EODIP5\\gis analysis\\n_patches_per_grid_all_scales_excl_gardens_urban_arable.csv')
write.table(n_patches_per_grid,table.loc,row.names=F,sep=',')


#plot freq dist of numbers of patches per grid square
#par(mfrow=c(2,2))
png('X:\\Shared Projects\\Defra80 - EODIP5\\gis analysis\\hist_n_patches_per_grid_01km_exl_gardens_urban_arable.png',width=300,height=300)
hist(nparcel01km$count,main=paste0('Parcels per 1-km square (n = ',nrow(nparcel01km),')\nMedian = ',median(nparcel01km$count)),xlab='Parcels',cex.main=1,breaks=20)
dev.off()
png('X:\\Shared Projects\\Defra80 - EODIP5\\gis analysis\\hist_n_patches_per_grid_500m_exl_gardens_urban_arable.png',width=300,height=300)
hist(nparcel500m$count,main=paste0('Parcels per 500-m square (n = ',nrow(nparcel500m),')\nMedian = ',median(nparcel500m$count)),xlab='Parcels',cex.main=1,breaks=20)
dev.off()
png('X:\\Shared Projects\\Defra80 - EODIP5\\gis analysis\\hist_n_patches_per_grid_200m_exl_gardens_urban_arable.png',width=300,height=300)
hist(nparcel200m$count,main=paste0('Parcels per 200-m square (n = ',nrow(nparcel200m),')\nMedian = ',median(nparcel200m$count)),xlab='Parcels',cex.main=1,breaks=20)
dev.off()
png('X:\\Shared Projects\\Defra80 - EODIP5\\gis analysis\\hist_n_patches_per_grid_100m_exl_gardens_urban_arable.png',width=300,height=300)
hist(nparcel100m$count,main=paste0('Parcels per 100-m square (n = ',nrow(nparcel100m),')\nMedian = ',median(nparcel100m$count)),xlab='Parcels',cex.main=1,breaks=20)
dev.off()
