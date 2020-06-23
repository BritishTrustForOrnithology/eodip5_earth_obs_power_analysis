#analyse Norfolk Map parcels within different grid resolutions and by geogrpahical zones (as defined by the layers of EObs data used)
#Simon Gillings
#February 2016
library(plyr)

# LOAD DATA ---------------------------------------------------------------
#parcel data
df<-readRDS('X:\\Shared Projects\\Defra80 - EODIP5\\gis analysis\\parcels_with_grids.rds')

#lookup lists of squares by zone. Note that some parcels in the original Living Map files fall outside the zones shapefile provided
zone.500m<-read.table(file='X:\\Shared Projects\\Defra80 - EODIP5\\gis analysis\\centroids_500m_zone.csv',sep=',',header=T,colClasses = rep('character',2))
zone.200m<-read.table(file='X:\\Shared Projects\\Defra80 - EODIP5\\gis analysis\\centroids_200m_zone.csv',sep=',',header=T,colClasses = rep('character',2))
zone.100m<-read.table(file='X:\\Shared Projects\\Defra80 - EODIP5\\gis analysis\\centroids_100m_zone.csv',sep=',',header=T,colClasses = rep('character',2))



# CLEANING AND SUMMARISING ---------------------------------------------------------------
#combine the two habs as agreed by JNCC
df$CLASS<-ifelse(df$CLASS=='Semi-improved (poor condition)','Semi-improved grassland',df$CLASS)

#get definitive list of squares at each resolution
#sqrlist.01km<-unique(df$ref01km)
sqrlist.500m<-unique(df$ref500m)
sqrlist.200m<-unique(df$ref200m)
sqrlist.100m<-unique(df$ref100m)

#centroid list was created using a grid so may have offshore points; filter using df to ensure only real squares are retained
zone.500m<-zone.500m[zone.500m$grid %in% sqrlist.500m,]
zone.200m<-zone.200m[zone.200m$grid %in% sqrlist.200m,]
zone.100m<-zone.100m[zone.100m$grid %in% sqrlist.100m,]

#create sorted habitat list
habs<-data.frame(CLASS=unique(df$CLASS),stringsAsFactors = F)
habs<-habs[order(habs$CLASS), ,drop=F]




# GET HABITAT SPECIFIC STATISTICS AT EACH SCALE  ---------------------------------
#process a scale
procscale<-function(scale) {
  #which column contains the grid ref at this scale?
  i<-which(names(df)==scale)
  
  #create temporary dataset with only this scale's label and rename as grid
  temp<-df[,c(2,i,10)]
  names(temp)[2]<-'grid'
  
  #get number of patches of each hab by square
  temp<-aggregate(data=temp,count~grid+CLASS,NROW)
  
  #count number of squares that have each habitat
  nsquareswith<-aggregate(data=temp,count~CLASS,NROW)
  
  #count total number of squares at this resolution
  nsquares<-length(unique(temp$grid))
  
  #subtract one from other to get number of squares at this resolution that don't have any of habitat
  nsquareswith$sqrswithouthab<-nsquares-nsquareswith$count
  names(nsquareswith)[2]<-'sqrswithhab'
  
  #save results
  table.loc<-paste0('X:\\Shared Projects\\Defra80 - EODIP5\\gis analysis\\n_squares_with_without_hab_',scale,'.csv')
  write.table(nsquareswith,table.loc,row.names=F,sep=',')
  
  #get information on frequency distribution of number of parcels of habitat per square
  patchstats10<-aggregate(data=temp,count~CLASS,FUN=quantile,probs=0.10)
  patchstats25<-aggregate(data=temp,count~CLASS,FUN=quantile,probs=0.25)
  patchstats50<-aggregate(data=temp,count~CLASS,FUN=quantile,probs=0.50)
  patchstats75<-aggregate(data=temp,count~CLASS,FUN=quantile,probs=0.75)
  patchstats90<-aggregate(data=temp,count~CLASS,FUN=quantile,probs=0.90)
  patchstats<-join_all(list(patchstats10,patchstats25,patchstats50,patchstats75,patchstats90), by = 'CLASS')
  names(patchstats)<-c('CLASS','percent10','lowerq','median','upperq','percent90')
  
  #save results
  table.loc<-paste0('X:\\Shared Projects\\Defra80 - EODIP5\\gis analysis\\n_patches_per_hab_per_grid_',scale,'.csv')
  write.table(patchstats,table.loc,row.names=F,sep=',')
  return(patchstats)
}

#call function to get stats on number of patches of each hab type per grid square
patchstats01km<-procscale(scale='ref01km')
patchstats500m<-procscale(scale='ref500m')
patchstats200m<-procscale(scale='ref200m')
patchstats100m<-procscale(scale='ref100m')




# GET STATS ON TOTAL NUMBER OF PARCELS PER GRID SQUARE --------------------
#calculate total number of parcels per grid
#nparcel10km<-aggregate(data=dftemp,count~ref10km,NROW)
nparcel01km<-aggregate(data=df,count~ref01km,NROW)
nparcel500m<-aggregate(data=df,count~ref500m,NROW)
nparcel200m<-aggregate(data=df,count~ref200m,NROW)
nparcel100m<-aggregate(data=df,count~ref100m,NROW)

#summarise various frequency distribution values
stats01km<-c('1km',quantile(nparcel01km$count,0.10),quantile(nparcel01km$count,0.25),quantile(nparcel01km$count,0.5),quantile(nparcel01km$count,0.75),quantile(nparcel01km$count,0.90))
stats500m<-c('500m',quantile(nparcel500m$count,0.10),quantile(nparcel500m$count,0.25),quantile(nparcel500m$count,0.5),quantile(nparcel500m$count,0.75),quantile(nparcel500m$count,0.90))
stats200m<-c('200m',quantile(nparcel200m$count,0.10),quantile(nparcel200m$count,0.25),quantile(nparcel200m$count,0.5),quantile(nparcel200m$count,0.75),quantile(nparcel200m$count,0.90))
stats100m<-c('100m',quantile(nparcel100m$count,0.10),quantile(nparcel100m$count,0.25),quantile(nparcel100m$count,0.5),quantile(nparcel100m$count,0.75),quantile(nparcel100m$count,0.90))

#save results
n_patches_per_grid<-rbind(stats01km,stats500m,stats200m,stats100m)
table.loc<-paste0('X:\\Shared Projects\\Defra80 - EODIP5\\gis analysis\\n_patches_per_grid_all_scales.csv')
write.table(n_patches_per_grid,table.loc,row.names=F,sep=',')


#plot freq dist of numbers of patches per grid square
#par(mfrow=c(2,2))
png('X:\\Shared Projects\\Defra80 - EODIP5\\gis analysis\\hist_n_patches_per_grid_01km.png',width=300,height=300)
hist(nparcel01km$count,main=paste0('Parcels per 1-km square (n = ',nrow(nparcel01km),')\nMedian = ',median(nparcel01km$count)),xlab='Parcels',cex.main=1,breaks=20)
dev.off()
png('X:\\Shared Projects\\Defra80 - EODIP5\\gis analysis\\hist_n_patches_per_grid_500m.png',width=300,height=300)
hist(nparcel500m$count,main=paste0('Parcels per 500-m square (n = ',nrow(nparcel500m),')\nMedian = ',median(nparcel500m$count)),xlab='Parcels',cex.main=1,breaks=20)
dev.off()
png('X:\\Shared Projects\\Defra80 - EODIP5\\gis analysis\\hist_n_patches_per_grid_200m.png',width=300,height=300)
hist(nparcel200m$count,main=paste0('Parcels per 200-m square (n = ',nrow(nparcel200m),')\nMedian = ',median(nparcel200m$count)),xlab='Parcels',cex.main=1,breaks=20)
dev.off()
png('X:\\Shared Projects\\Defra80 - EODIP5\\gis analysis\\hist_n_patches_per_grid_100m.png',width=300,height=300)
hist(nparcel100m$count,main=paste0('Parcels per 100-m square (n = ',nrow(nparcel100m),')\nMedian = ',median(nparcel100m$count)),xlab='Parcels',cex.main=1,breaks=20)
dev.off()



# REPEAT ANALYSES WITHIN ZONES --------------------------------------
procscalebyzone<-function(scale, zone) {
  #which column contains the grid ref at this scale?
  i<-which(names(df)==scale)
  
  #create temporary dataset with only this scale's label and rename as grid
  temp<-df[,c(2,i,10)]
  names(temp)[2]<-'grid'

  #get the definition of zones at this resolution
  assign('zonedef',get(apropos(paste0('zone.',substr(scale,4,8)))))
  
  dim(temp)
  #merge the zone definitions with the parcel data. Note that if some parcels fall in squares outside the classificaiton zones, these will be dropped
  temp<-merge(temp,zonedef,by='grid')
  
  #get number of patches of each hab by square
  temp<-aggregate(data=temp,count~zone+grid+CLASS,NROW)
  
  #count number of squares that have each habitat
  nsquareswith<-aggregate(data=temp,count~zone+CLASS,NROW)
  head(nsquareswith)
  
  #count total number of squares at this resolution in each zone
  nsquares<-unique(temp[,c(1,2)])
  nsquares$nsquares<-1
  nsquares<-aggregate(data=nsquares,nsquares~zone,sum)
  
  #merge and subtract one from other to get number of squares at this resolution that don't have any of habitat
  nsquareswith<-merge(nsquareswith,nsquares,by='zone')
  nsquareswith$sqrswithouthab<-nsquareswith$nsquares-nsquareswith$count
  names(nsquareswith)[3]<-'sqrswithhab'
  
  #merge with master hab*zone list
  habzone<-expand.grid(habs$CLASS,c('A','B','C','D','E'))
  names(habzone)<-c('CLASS','zone')
  nsquareswith<-merge(habzone,nsquareswith,by=c('zone','CLASS'),all.x=T)
  nsquareswith[is.na(nsquareswith)]<-0

  #save results
  table.loc<-paste0('X:\\Shared Projects\\Defra80 - EODIP5\\gis analysis\\n_squares_with_without_hab_byzone_',scale,'.csv')
  write.table(nsquareswith,table.loc,row.names=F,sep=',')
  
  #get information on frequency distribution of number of parcels of habitat per square
  patchstats10<-aggregate(data=temp,count~CLASS+zone,FUN=quantile,probs=0.10)
  patchstats25<-aggregate(data=temp,count~CLASS+zone,FUN=quantile,probs=0.25)
  patchstats50<-aggregate(data=temp,count~CLASS+zone,FUN=quantile,probs=0.50)
  patchstats75<-aggregate(data=temp,count~CLASS+zone,FUN=quantile,probs=0.75)
  patchstats90<-aggregate(data=temp,count~CLASS+zone,FUN=quantile,probs=0.90)
  patchstats<-join_all(list(patchstats10,patchstats25,patchstats50,patchstats75,patchstats90), by = c('zone','CLASS') )
  names(patchstats)<-c('CLASS','zone','percent10','lowerq','median','upperq','percent90')
  patchstats<-merge(habzone,patchstats,by=c('zone','CLASS'),all.x=T)
  patchstats[is.na(patchstats)]<-0
  
  #save results
  table.loc<-paste0('X:\\Shared Projects\\Defra80 - EODIP5\\gis analysis\\n_patches_per_hab_per_grid_byzone_',scale,'.csv')
  write.table(patchstats,table.loc,row.names=F,sep=',')
  return(patchstats)
}

#call function to get stats on number of patches of each hab type per grid square by zone
patchstatsbyzone01km<-procscalebyzone(scale='ref01km')
patchstatsbyzone500m<-procscalebyzone(scale='ref500m')
patchstatsbyzone200m<-procscalebyzone(scale='ref200m')
patchstatsbyzone100m<-procscalebyzone(scale='ref100m')


# GET STATS ON TOTAL NUMBER OF PARCELS PER GRID SQUARE BY ZONE --------------------
#need to merge in zones at relevant scale
df<-merge(df,zone.500m,by.x='ref500m',by.y='grid',all.x=T)
names(df)[which(names(df)=='zone')]<-'zone.500m'
df<-merge(df,zone.200m,by.x='ref200m',by.y='grid',all.x=T)
names(df)[which(names(df)=='zone')]<-'zone.200m'
df<-merge(df,zone.100m,by.x='ref100m',by.y='grid',all.x=T)
names(df)[which(names(df)=='zone')]<-'zone.100m'


#calculate total number of parcels per grid
nparcel500m<-aggregate(data=df,count~ref500m+zone.500m,NROW)
nparcel200m<-aggregate(data=df,count~ref200m+zone.200m,NROW)
nparcel100m<-aggregate(data=df,count~ref100m+zone.100m,NROW)

#summarise various frequency distribution values
statsbyzone<-function(zone) {
  this.nparcel500m<-nparcel500m[nparcel500m$zone.500m==zone,]
  this.nparcel200m<-nparcel200m[nparcel200m$zone.200m==zone,]
  this.nparcel100m<-nparcel100m[nparcel100m$zone.100m==zone,]
  stats500m<-c('500m',quantile(this.nparcel500m$count,0.10),quantile(this.nparcel500m$count,0.25),quantile(this.nparcel500m$count,0.5),quantile(this.nparcel500m$count,0.75),quantile(this.nparcel500m$count,0.90))
  stats200m<-c('200m',quantile(this.nparcel200m$count,0.10),quantile(this.nparcel200m$count,0.25),quantile(this.nparcel200m$count,0.5),quantile(this.nparcel200m$count,0.75),quantile(this.nparcel200m$count,0.90))
  stats100m<-c('100m',quantile(this.nparcel100m$count,0.10),quantile(this.nparcel100m$count,0.25),quantile(this.nparcel100m$count,0.5),quantile(this.nparcel100m$count,0.75),quantile(this.nparcel100m$count,0.90))
  #save results
  n_patches_per_grid<-rbind(stats500m,stats200m,stats100m)
  table.loc<-paste0('X:\\Shared Projects\\Defra80 - EODIP5\\gis analysis\\n_patches_per_grid_all_scales_zone_',zone,'.csv')
  write.table(n_patches_per_grid,table.loc,row.names=F,sep=',')
}

statsbyzone(zone='A')
statsbyzone(zone='B')
statsbyzone(zone='C')
statsbyzone(zone='D')
statsbyzone(zone='E')

