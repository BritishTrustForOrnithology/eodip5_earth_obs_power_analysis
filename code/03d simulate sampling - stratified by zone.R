#simulate sampling and error - simple random sampling within classification zones
#Simon Gillings
#February 2016
library(plyr)

#set working directory
setwd('X:\\Shared Projects\\Defra80 - EODIP5\\gis analysis\\')

# LOAD DATA ------------------------------------------------------------------------------------------
#load data
df<-readRDS('parcels_with_grids.rds')

#load habitat short names
hablabs<-read.table(file='hablabels.csv',sep=',',header=T,colClasses = rep('character',2))

#lookup lists of squares by zone. Note that some parcels in the original Living Map files fall outside the zones shapefile provided
zone.500m<-read.table(file='centroids_500m_zone.csv',sep=',',header=T,colClasses = rep('character',2))
names(zone.500m)<-c('ref500m','zone500m')
zone.200m<-read.table(file='centroids_200m_zone.csv',sep=',',header=T,colClasses = rep('character',2))
names(zone.200m)<-c('ref200m','zone200m')
zone.100m<-read.table(file='centroids_100m_zone.csv',sep=',',header=T,colClasses = rep('character',2))
names(zone.100m)<-c('ref100m','zone100m')

# PREPARE DATA -----------------------------------------------------------------------------------------
#combine the two habs as agreed by JNCC
df$CLASS<-ifelse(df$CLASS=='Semi-improved (poor condition)','Semi-improved grassland',df$CLASS)

#merge zones with df for each grid resolution
df<-merge(df,zone.500m,by='ref500m',all.x=T)
df<-merge(df,zone.200m,by='ref200m',all.x=T)
df<-merge(df,zone.100m,by='ref100m',all.x=T)

#some parcels fall in squares outside the zones - mark up as zone X
df$zone500m[is.na(df$zone500m)]<-'X'
df$zone200m[is.na(df$zone200m)]<-'X'
df$zone100m[is.na(df$zone100m)]<-'X'

#get definitive list of squares at each resolution
#sqrlist.01km<-unique(df$ref01km)
sqrlist.500m<-unique(subset(df,select=c('ref500m','zone500m')))
sqrlist.200m<-unique(subset(df,select=c('ref200m','zone200m')))
sqrlist.100m<-unique(subset(df,select=c('ref100m','zone100m')))

#how many squares per zone per grid resolution
table(sqrlist.500m$zone500m)
table(sqrlist.200m$zone200m)
table(sqrlist.100m$zone100m)

#get definitive list of habitats
hablist<-data.frame(CLASS=unique(df$CLASS),stringsAsFactors = F)
#add error rates
hablist$error.low<-0.05
hablist$error.mid<-0.15
hablist$error.high<-0.30
#sort
hablist<-hablist[order(hablist$CLASS),]
hablist<-merge(hablist,hablabs,by='CLASS')

#simulate random sampling in each zone
simulate.random<-function(repnum,nsqr,gridres,errorlevel) {
  #repnum = counter to show progress on reps
  #nsqr = number of squares in sample
  #gridres = what size grid squares to use
  #errorlevel = use low, mid or high error estimate
  cat(paste(repnum,nsqr,gridres,errorlevel,'\n'))
  
  #get the relevant list of squares in the region and assign to object squares 
  assign("squares",get(apropos(gridres) ) )
  names(squares)[2]<-'zone'
  
  #make a sample of squares at this resolution (without replacement)
  #need to  check that sample size is not greater than number of squares in region; if so reduce sample size for this run
  max.samp.size<-table(squares$zone)
  nsqr.A<-ifelse(max.samp.size[['A']]<nsqr,max.samp.size[['A']],nsqr)
  nsqr.B<-ifelse(max.samp.size[['B']]<nsqr,max.samp.size[['B']],nsqr)
  nsqr.C<-ifelse(max.samp.size[['C']]<nsqr,max.samp.size[['C']],nsqr)
  nsqr.D<-ifelse(max.samp.size[['D']]<nsqr,max.samp.size[['D']],nsqr)
  nsqr.E<-ifelse(max.samp.size[['E']]<nsqr,max.samp.size[['E']],nsqr)
  nsqr.X<-ifelse(max.samp.size[['X']]<nsqr,max.samp.size[['X']],nsqr)
  
  #process each zone separately
  this.sample.A<-as.character(sample(squares[squares$zone=='A',1], replace=F, size=nsqr.A, prob=NULL))
  this.sample.B<-as.character(sample(squares[squares$zone=='B',1], replace=F, size=nsqr.B, prob=NULL))
  this.sample.C<-as.character(sample(squares[squares$zone=='C',1], replace=F, size=nsqr.C, prob=NULL))
  this.sample.D<-as.character(sample(squares[squares$zone=='D',1], replace=F, size=nsqr.D, prob=NULL))
  this.sample.E<-as.character(sample(squares[squares$zone=='E',1], replace=F, size=nsqr.E, prob=NULL))
  this.sample.X<-as.character(sample(squares[squares$zone=='X',1], replace=F, size=nsqr.X, prob=NULL))
  
  
  #get all parcels in these squares, after checking which column holds the grid refs and CLASS at this scale
  i<-which(names(df)==paste0('ref',gridres))
  ic<-which(names(df)=='CLASS')
  this.parcels.A<-df[df[,i] %in% this.sample.A,ic]
  this.parcels.B<-df[df[,i] %in% this.sample.B,ic]
  this.parcels.C<-df[df[,i] %in% this.sample.C,ic]
  this.parcels.D<-df[df[,i] %in% this.sample.D,ic]
  this.parcels.E<-df[df[,i] %in% this.sample.E,ic]
  this.parcels.X<-df[df[,i] %in% this.sample.X,ic]
  
  #tally up how many parcels of each type would be checked
  this.hab.freqs.A<-as.data.frame(table(this.parcels.A),stringsAsFactors = F)
  this.hab.freqs.B<-as.data.frame(table(this.parcels.B),stringsAsFactors = F)
  this.hab.freqs.C<-as.data.frame(table(this.parcels.C),stringsAsFactors = F)
  this.hab.freqs.D<-as.data.frame(table(this.parcels.D),stringsAsFactors = F)
  this.hab.freqs.E<-as.data.frame(table(this.parcels.E),stringsAsFactors = F)
  this.hab.freqs.X<-as.data.frame(table(this.parcels.X),stringsAsFactors = F)
  names(this.hab.freqs.A)<-c('CLASS','n.parcels.checked.A')
  names(this.hab.freqs.B)<-c('CLASS','n.parcels.checked.B')
  names(this.hab.freqs.C)<-c('CLASS','n.parcels.checked.C')
  names(this.hab.freqs.D)<-c('CLASS','n.parcels.checked.D')
  names(this.hab.freqs.E)<-c('CLASS','n.parcels.checked.E')
  names(this.hab.freqs.X)<-c('CLASS','n.parcels.checked.X')
  
  #merge results with master list of habs and error rates and convert NAs to zeroes
  this.hab.freqs<-join_all(list(hablist,this.hab.freqs.A,this.hab.freqs.B,this.hab.freqs.C,this.hab.freqs.D,this.hab.freqs.E,this.hab.freqs.X) ,by='CLASS')  
  this.hab.freqs[is.na(this.hab.freqs)]<-0
  
  #simulate how many errors there might be in this many patches. First create a column of 1s to force code to create 1 answer per call of rbinom
  this.hab.freqs$x<-1
  #check which error column to use
  e<-which(names(this.hab.freqs)==paste0('error.',errorlevel))
  #simulate the number of errors
  n.errors.A<-rbinom(this.hab.freqs$x,this.hab.freqs$n.parcels.checked.A,this.hab.freqs[,e])
  n.errors.B<-rbinom(this.hab.freqs$x,this.hab.freqs$n.parcels.checked.B,this.hab.freqs[,e])
  n.errors.C<-rbinom(this.hab.freqs$x,this.hab.freqs$n.parcels.checked.C,this.hab.freqs[,e])
  n.errors.D<-rbinom(this.hab.freqs$x,this.hab.freqs$n.parcels.checked.D,this.hab.freqs[,e])
  n.errors.E<-rbinom(this.hab.freqs$x,this.hab.freqs$n.parcels.checked.E,this.hab.freqs[,e])
  n.errors.X<-rbinom(this.hab.freqs$x,this.hab.freqs$n.parcels.checked.X,this.hab.freqs[,e])
  
  #bind the results together and create output
  this.hab.freqs<-cbind(this.hab.freqs,n.errors.A,n.errors.B,n.errors.C,n.errors.D,n.errors.E,n.errors.X)
  this.hab.freqs$error.rate.A<-this.hab.freqs$n.errors.A/this.hab.freqs$n.parcels.checked.A
  this.hab.freqs$error.rate.B<-this.hab.freqs$n.errors.B/this.hab.freqs$n.parcels.checked.B
  this.hab.freqs$error.rate.C<-this.hab.freqs$n.errors.C/this.hab.freqs$n.parcels.checked.C
  this.hab.freqs$error.rate.D<-this.hab.freqs$n.errors.D/this.hab.freqs$n.parcels.checked.D
  this.hab.freqs$error.rate.E<-this.hab.freqs$n.errors.E/this.hab.freqs$n.parcels.checked.E
  this.hab.freqs$error.rate.X<-this.hab.freqs$n.errors.X/this.hab.freqs$n.parcels.checked.X
  this.hab.freqs$nsqr<-nsqr
  this.hab.freqs$gridres<-gridres
  this.hab.freqs$errorlevel<-errorlevel
  this.hab.freqs$error.expected<-this.hab.freqs[,e]
  this.hab.freqs<-this.hab.freqs[,-c(2,3,4)]
  return(this.hab.freqs)
}



##################################################################################################################################################
# ALL SCALES, WIDE RANGE OF SAMPLE SIZES, 15% ERROR ---------------------------------------------------------------
##################################################################################################################################################
#create population of scenarios
sqrs.min<-100
sqrs.max<-2000
sqrs.step<-100
reps<-100
#expand.grid creates all permutations of whatever vectors are fed in. 
scenarios<-expand.grid(seq(from=1,to=reps,by=1),
                       seq(sqrs.min,sqrs.max,by=sqrs.step),
                       c('500m','200m','100m'),
                       'mid',
                       stringsAsFactors = F)
#rename columns
names(scenarios)<-c('repnum','nsqr','gridres','errorlevel')
#how many scenarios
dim(scenarios)

#apply the simulate function to all scenarios
results<-mapply(simulate.random,scenarios$repnum,scenarios$nsqr,scenarios$gridres,scenarios$errorlevel,SIMPLIFY=F)
#convert list of dataframes to one big dataframe
results <- ldply(results, data.frame)
saveRDS(results,file='sim_random_sampling_by_zone_15pc_error\\sim_random_sampling_by_zone_results_3scales_100-2000sqrs_error15.rds')
#results<-readRDS(file='sim_random_sampling_by_zone_15pc_error\\sim_random_sampling_by_zone_results_3scales_100-2000sqrs_error15.rds')




#make boxplots
#how many habs to plot
nhabs<-nrow(hablist)
for(h in 1:nhabs) {
  #which habitat
  this.hab<-hablist$CLASS[h]
  this.lab<-hablist$LABEL[h]
  #create output for image for this habitat
  loc.name<-paste0('sim_random_sampling_by_zone_15pc_error\\sim_random_sampling_error15_',gsub(" ", "_", this.hab),'.png')
  png(loc.name,width=1000,height=1500)
  #set image parameters for png output
  par(mfrow=c(5,2))
  par(mar=c(6,9,4,2))
  par(mgp=c(0,2.5,0))
  par(tcl=-1)
  
  #get results for this habitat
  this.results<-results[results$CLASS==this.hab,]
  #process a single grid scale
  this.results<-this.results[this.results$gridres=='500m',]
  #for this habitat, convert NaN values to zeroes
  this.results$error.rate.A<-ifelse(is.nan(this.results$error.rate.A),0,this.results$error.rate.A)
  this.results$error.rate.B<-ifelse(is.nan(this.results$error.rate.B),0,this.results$error.rate.B)
  this.results$error.rate.C<-ifelse(is.nan(this.results$error.rate.C),0,this.results$error.rate.C)
  this.results$error.rate.D<-ifelse(is.nan(this.results$error.rate.D),0,this.results$error.rate.D)
  this.results$error.rate.E<-ifelse(is.nan(this.results$error.rate.E),0,this.results$error.rate.E)
  this.results$error.rate.X<-ifelse(is.nan(this.results$error.rate.X),0,this.results$error.rate.X)
  
  this.error<-unique(this.results$error.expected)
  boxplot(data=this.results,n.parcels.checked.A~nsqr,cex.axis=2.5,main='Zone A: No. parcels checked',cex.main=3.25,las=1)
  boxplot(data=this.results,error.rate.A~nsqr,cex.axis=2.5,main='Zone A: Error estimates',cex.main=3.25,ylim=c(0,1),las=1)
  abline(h=this.error,col='red')
  boxplot(data=this.results,n.parcels.checked.B~nsqr,cex.axis=2.5,main='Zone B: No. parcels checked',cex.main=3.25,las=1)
  boxplot(data=this.results,error.rate.B~nsqr,cex.axis=2.5,main='Zone B: Error estimates',cex.main=3.25,ylim=c(0,1),las=1)
  abline(h=this.error,col='red')
  boxplot(data=this.results,n.parcels.checked.C~nsqr,cex.axis=2.5,main='Zone C: No. parcels checked',cex.main=3.25,las=1)
  boxplot(data=this.results,error.rate.C~nsqr,cex.axis=2.5,main='Zone C: Error estimates',cex.main=3.25,ylim=c(0,1),las=1)
  abline(h=this.error,col='red')
  boxplot(data=this.results,n.parcels.checked.D~nsqr,cex.axis=2.5,main='Zone D: No. parcels checked',cex.main=3.25,las=1)
  boxplot(data=this.results,error.rate.D~nsqr,cex.axis=2.5,main='Zone D: Error estimates',cex.main=3.25,ylim=c(0,1),las=1)
  abline(h=this.error,col='red')
  boxplot(data=this.results,n.parcels.checked.E~nsqr,cex.axis=2.5,main='Zone E: No. parcels checked',cex.main=3.25,las=1)
  boxplot(data=this.results,error.rate.E~nsqr,cex.axis=2.5,main='Zone E: Error estimates',cex.main=3.25,ylim=c(0,1),las=1)
  abline(h=this.error,col='red')
  #boxplot(data=this.results.grid,n.parcels.checked.X~nsqr,cex.axis=2.5,main='Zone X: No. parcels checked',cex.main=3.25,las=1)
  #boxplot(data=this.results.grid,error.rate.X~nsqr,cex.axis=2.5,main='Zone X: Error estimates',cex.main=3.25,ylim=c(0,1),las=1)
  dev.off()
}


#summarise how often the "correct" answer is found. Acceptable precision of +/-5% (arithmetic) on the known error rate
#replace NaNs with zeroes for this classification of whether error was correctly assigned
precision<-0.05
error<-0.15
precision.lower<-error-precision
precision.upper<-error+precision
results$error.rate.A<-ifelse(is.nan(results$error.rate.A),0,results$error.rate.A)
results$error.rate.B<-ifelse(is.nan(results$error.rate.B),0,results$error.rate.B)
results$error.rate.C<-ifelse(is.nan(results$error.rate.C),0,results$error.rate.C)
results$error.rate.D<-ifelse(is.nan(results$error.rate.D),0,results$error.rate.D)
results$error.rate.E<-ifelse(is.nan(results$error.rate.E),0,results$error.rate.E)
results$est.correct.A<-ifelse(results$error.rate.A>=precision.lower & results$error.rate.A<=precision.upper,100,0)
results$est.correct.B<-ifelse(results$error.rate.B>=precision.lower & results$error.rate.B<=precision.upper,100,0)
results$est.correct.C<-ifelse(results$error.rate.C>=precision.lower & results$error.rate.C<=precision.upper,100,0)
results$est.correct.D<-ifelse(results$error.rate.D>=precision.lower & results$error.rate.D<=precision.upper,100,0)
results$est.correct.E<-ifelse(results$error.rate.E>=precision.lower & results$error.rate.E<=precision.upper,100,0)
p.correct.vals.A<-aggregate(data=results,est.correct.A~CLASS+gridres+nsqr,mean)
p.correct.vals.B<-aggregate(data=results,est.correct.B~CLASS+gridres+nsqr,mean)
p.correct.vals.C<-aggregate(data=results,est.correct.C~CLASS+gridres+nsqr,mean)
p.correct.vals.D<-aggregate(data=results,est.correct.D~CLASS+gridres+nsqr,mean)
p.correct.vals.E<-aggregate(data=results,est.correct.E~CLASS+gridres+nsqr,mean)

p.correct.A<-p.correct.vals.A
p.correct.B<-p.correct.vals.B
p.correct.C<-p.correct.vals.C
p.correct.D<-p.correct.vals.D
p.correct.E<-p.correct.vals.E
p.correct.A$est.correct.A<-ifelse(p.correct.A$est.correct.A>=95,1,0)
p.correct.B$est.correct.B<-ifelse(p.correct.B$est.correct.B>=95,1,0)
p.correct.C$est.correct.C<-ifelse(p.correct.C$est.correct.C>=95,1,0)
p.correct.D$est.correct.D<-ifelse(p.correct.D$est.correct.D>=95,1,0)
p.correct.E$est.correct.E<-ifelse(p.correct.E$est.correct.E>=95,1,0)
habs.correct.A<-aggregate(data=p.correct.A,est.correct.A~nsqr+gridres,sum)
habs.correct.B<-aggregate(data=p.correct.B,est.correct.B~nsqr+gridres,sum)
habs.correct.C<-aggregate(data=p.correct.C,est.correct.C~nsqr+gridres,sum)
habs.correct.D<-aggregate(data=p.correct.D,est.correct.D~nsqr+gridres,sum)
habs.correct.E<-aggregate(data=p.correct.E,est.correct.E~nsqr+gridres,sum)


#produce summary graph per zone
zones<-c('A','B','C','D','E')
#some zones lack certain habitats - this is the max number of habitats present 
max.habs<-c(12,20,16,21,27)
for(z in 1:5) {
  this.zone<-zones[z]
  this.max.habs<-max.habs[z]
  assign("this.results",get(apropos(paste0('habs.correct.',this.zone) ) ) )
  names(this.results)[3]<-'ncorrect'
  loc.name<-paste0('sim_random_sampling_by_zone_15pc_error\\nhabs_correctly_assessed_zone_',this.zone,'.png')
  png(loc.name,width=400,height=400)
  par(mar=c(5.1,4.1,0.2,0.2))
  plot(this.results$nsqr[this.results$gridres=='100m'],this.results$ncorrect[this.results$gridres=='100m'],ylim=c(0,this.max.habs),xlab='Sample size',ylab='Habitats correctly assessed',pch=16,type='l',lty=1, cex.lab=1.25)
  par(new=T)
  plot(this.results$nsqr[this.results$gridres=='200m'],this.results$ncorrect[this.results$gridres=='200m'],ylim=c(0,this.max.habs),xlab='Sample size',ylab='Habitats correctly assessed',pch=16,type='l',lty=2, cex.lab=1.25)
  par(new=T)
  plot(this.results$nsqr[this.results$gridres=='500m'],this.results$ncorrect[this.results$gridres=='500m'],ylim=c(0,this.max.habs),xlab='Sample size',ylab='Habitats correctly assessed',pch=16,type='l',lty=3, cex.lab=1.25)
  abline(h=this.max.habs,col='red')
  leg.text<-c('100-m','200-m','500-m')
  legend('topleft',leg.text,lty=c(1,2,3))
  dev.off()
}

#save results
#table.loc<-'X:\\Shared Projects\\Defra80 - EODIP5\\gis analysis\\sim_random_sampling_15pc_error\\how_often_correct.csv'
#write.table(p.correct,table.loc,row.names=F,sep=',')


#check which habitats are covered in each zone using 500m grid
zonehabstats<-read.table('n_squares_with_without_hab_byzone_ref500m.csv',sep=',',header=T, colClasses = c(rep('character',2),rep('numeric',3)))
zonehabstats$pwith<-with(data=zonehabstats,sqrswithhab/nsquares)
head(zonehabstats)
#Zone A
habs.A<-subset(zonehabstats,pwith>=0.01 & zone=='A') 
#trim to hab, grid res and rows with adequate precision
p.correct.vals.A2<-subset(p.correct.vals.A,CLASS %in% habs.A$CLASS & gridres=='500m' & est.correct.A>=95)
#get largest sample size that works
p.correct.vals.A2[!duplicated(p.correct.vals.A2$CLASS),]
#print best achieved error rate for those that fail
subset(p.correct.vals.A,CLASS %in% habs.A$CLASS & gridres=='500m' & est.correct.A<95 & nsqr==2000)

#Zone B
habs.B<-subset(zonehabstats,pwith>=0.01 & zone=='B') 
#trim to hab, grid res and rows with adequate precision
p.correct.vals.B2<-subset(p.correct.vals.B,CLASS %in% habs.B$CLASS & gridres=='500m' & est.correct.B>=95)
#get largest sample size that works
p.correct.vals.B2[!duplicated(p.correct.vals.B2$CLASS),]
#print best achieved error rate for those that fail
subset(p.correct.vals.B,CLASS %in% habs.B$CLASS & gridres=='500m' & est.correct.B<95 & nsqr==2000)

#Zone C
habs.C<-subset(zonehabstats,pwith>=0.01 & zone=='C') 
#trim to hab, grid res and rows with adequate precision
p.correct.vals.C2<-subset(p.correct.vals.C,CLASS %in% habs.C$CLASS & gridres=='500m' & est.correct.C>=95)
#get largest sample size that works
p.correct.vals.C2[!duplicated(p.correct.vals.C2$CLASS),]
#print best achieved error rate for those that fail
subset(p.correct.vals.C,CLASS %in% habs.C$CLASS & gridres=='500m' & est.correct.C<95 & nsqr==2000)

#Zone D
habs.D<-subset(zonehabstats,pwith>=0.01 & zone=='D') 
#trim to hab, grid res and rows with adequate precision
p.correct.vals.D2<-subset(p.correct.vals.D,CLASS %in% habs.D$CLASS & gridres=='500m' & est.correct.D>=95)
#get largest sample size that works
p.correct.vals.D2[!duplicated(p.correct.vals.D2$CLASS),]
#print best achieved error rate for those that fail
subset(p.correct.vals.D,CLASS %in% habs.D$CLASS & gridres=='500m' & est.correct.D<95 & nsqr==2000)

#Zone E
habs.E<-subset(zonehabstats,pwith>=0.01 & zone=='E') 
#trim to hab, grid res and rows with adequate precision
p.correct.vals.E2<-subset(p.correct.vals.E,CLASS %in% habs.E$CLASS & gridres=='500m' & est.correct.E>=95)
#get largest sample size that works
p.correct.vals.E2[!duplicated(p.correct.vals.E2$CLASS),]
#print best achieved error rate for those that fail
subset(p.correct.vals.E,CLASS %in% habs.E$CLASS & gridres=='500m' & est.correct.E<95 & nsqr==2000)
