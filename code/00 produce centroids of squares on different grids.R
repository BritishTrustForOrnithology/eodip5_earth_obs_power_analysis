#for assigning different grid squares to the classification zones, need to create centroids for 
#different grids for subsequent processing in GIS
#Simon Gillings
#February 2016

#set working directory
setwd('X:\\Shared Projects\\Defra80 - EODIP5\\gis analysis\\')

#WHAT EXTENT OF GRID IS NEEDED
#df<-readRDS('X:\\Shared Projects\\Defra80 - EODIP5\\gis analysis\\parcels_with_grids.rds')
#summary(df$easting)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#543900  580600  607900  602800  624200  653800 
#summary(df$northing)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#276900  299500  310200  310800  321400  347400 

bounds.e<-c(543000,656000)
bounds.n<-c(276000,348000)

#get the grid ref conversion function
source('function_grid_conversion.R')



#500m grid
offset<-250
step<-500
easts<-seq(from=bounds.e[1]+offset,to=bounds.e[2]+offset,by=step)
norths<-seq(from=bounds.n[1]+offset,to=bounds.n[2]+offset,by=step)
n.easts<-length(easts)
n.norths<-length(norths)
output500m<-list()
for(e in 1:n.easts) {
  this.east<-easts[e]
  for(n in 1:n.norths) {
    this.north<-norths[n]
    this.coords<-c(this.east,this.north)
    output500m<-rbind(output500m,this.coords)
  }
}
dim(output500m)
output500m<-data.frame(output500m)
names(output500m)<-c('easting','northing')
output500m$easting<-as.numeric(output500m$easting)
output500m$northing<-as.numeric(output500m$northing)

#assign the grid refs and trim to relevant columns
output500m<-makegridref(df=output500m)
output500m<-subset(output500m,select=c('ref500m','easting','northing'))
head(output500m)
#export
write.table(output500m,'centroids_500m.csv',sep=',',row.names=F)



#200m grid
offset<-100
step<-200
easts<-seq(from=bounds.e[1]+offset,to=bounds.e[2]+offset,by=step)
norths<-seq(from=bounds.n[1]+offset,to=bounds.n[2]+offset,by=step)
n.easts<-length(easts)
n.norths<-length(norths)
output200m<-list()
for(e in 1:n.easts) {
  this.east<-easts[e]
  for(n in 1:n.norths) {
    this.north<-norths[n]
    this.coords<-c(this.east,this.north)
    output200m<-rbind(output200m,this.coords)
  }
}
dim(output200m)
output200m<-data.frame(output200m)
names(output200m)<-c('easting','northing')
output200m$easting<-as.numeric(output200m$easting)
output200m$northing<-as.numeric(output200m$northing)

#assign the grid refs and trim to relevant columns
output200m<-makegridref(df=output200m)
output200m<-subset(output200m,select=c('ref200m','easting','northing'))
head(output200m)
#export
write.table(output200m,'centroids_200m.csv',sep=',',row.names=F)

#100m grid
offset<-50
step<-100
easts<-seq(from=bounds.e[1]+offset,to=bounds.e[2]+offset,by=step)
norths<-seq(from=bounds.n[1]+offset,to=bounds.n[2]+offset,by=step)
n.easts<-length(easts)
n.norths<-length(norths)
#at this point, format of output is slightly different to above to be more efficient
out.east<-numeric()
out.north<-numeric()
for(e in 1:n.easts) {
  this.east<-easts[e]
  for(n in 1:n.norths) {
    this.north<-norths[n]
    #assign using subscripts for efficiency
    out.east[((e-1)*n.norths)+n]<-this.east
    out.north[((e-1)*n.norths)+n]<-this.north
  }
}

#bind eastings and northings
output100m<-data.frame(easting=out.east,northing=out.north)

#assign the grid refs and trim to relevant columns
output100m<-makegridref(df=output100m)
output100m<-subset(output100m,select=c('ref100m','easting','northing'))
head(output100m)
#export
write.table(output100m,'centroids_100m.csv',sep=',',row.names=F)
