#simulate simple random sampling and error
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

# PREPARE DATA -----------------------------------------------------------------------------------------
#combine the two habs as agreed by JNCC
df$CLASS<-ifelse(df$CLASS=='Semi-improved (poor condition)','Semi-improved grassland',df$CLASS)

#get definitive list of squares at each resolution
#sqrlist.01km<-unique(df$ref01km)
sqrlist.500m<-unique(df$ref500m)
sqrlist.200m<-unique(df$ref200m)
sqrlist.100m<-unique(df$ref100m)

#get definitive list of habitats
hablist<-data.frame(CLASS=unique(df$CLASS),stringsAsFactors = F)
#add error rates
hablist$error.low<-0.05
hablist$error.mid<-0.15
hablist$error.high<-0.30
#sort
hablist<-hablist[order(hablist$CLASS),]
hablist<-merge(hablist,hablabs,by='CLASS')

#simulate random sampling
simulate.random<-function(repnum,nsqr,gridres,errorlevel) {
  #repnum = counter to show progress on reps
  #nsqr = number of squares in sample
  #gridres = what size grid squares to use
  #errorlevel = use low, mid or high error estimate
  cat(paste(repnum,nsqr,gridres,errorlevel,'\n'))
  
  #get the relevant list of squares in the region and assign to object squares 
  assign("squares",get(apropos(gridres) ) )
  
  #make a sample of squares at this resolution (without replacement)
  this.sample<-as.character(sample(squares, replace=F, size=nsqr, prob=NULL))
  
  #get all parcels in these squares, after checking which column holds the grid refs at this scale
  i<-which(names(df)==paste0('ref',gridres))
  this.parcels<-df[df[,i] %in% this.sample,2]
  
  #tally up how many parcels of each type would be checked
  this.hab.freqs<-as.data.frame(table(this.parcels),stringsAsFactors = F)
  names(this.hab.freqs)<-c('CLASS','n.parcels.checked')
  
  #merge results with master list of habs and error rates and convert NAs to zeroes
  this.hab.freqs<-merge(hablist,this.hab.freqs,by='CLASS',all.x=T)  
  this.hab.freqs[is.na(this.hab.freqs)]<-0
  
  #simulate how many errors there might be in this many patches. First create a column of 1s to force code to create 1 answer per call of rbinom
  this.hab.freqs$x<-1
  #check which error column to use
  e<-which(names(this.hab.freqs)==paste0('error.',errorlevel))
  
  n.errors<-rbinom(this.hab.freqs$x,this.hab.freqs$n.parcels.checked,this.hab.freqs[,e])

  this.hab.freqs<-cbind(this.hab.freqs,n.errors)
  this.hab.freqs$error.rate<-this.hab.freqs$n.errors/this.hab.freqs$n.parcels.checked
  this.hab.freqs$nsqr<-nsqr
  this.hab.freqs$gridres<-gridres
  this.hab.freqs$errorlevel<-errorlevel
  this.hab.freqs$error.expected<-this.hab.freqs[,e]
  this.hab.freqs<-this.hab.freqs[,c(1,6,8,9,10,11,12,13)]
  return(this.hab.freqs)
}


##################################################################################################################################################
# ALL SCALES, WIDE RANGE OF SAMPLE SIZES, 30% ERROR ---------------------------------------------------------------
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
                       'high',
                       stringsAsFactors = F)
#rename columns
names(scenarios)<-c('repnum','nsqr','gridres','errorlevel')
#how many scenarios
dim(scenarios)

#apply the simulate function to all scenarios
results<-mapply(simulate.random,scenarios$repnum,scenarios$nsqr,scenarios$gridres,scenarios$errorlevel,SIMPLIFY=F)
#convert list of dataframes to one big dataframe
results <- ldply(results, data.frame)
saveRDS(results,file='sim_random_sampling_30pc_error\\sim_random_sampling_results_3scales_100-2000sqrs_error30.rds')
#results<-readRDS(file='sim_random_sampling_30pc_error\\sim_random_sampling_results_3scales_100-2000sqrs_error30.rds')

#make boxplots
#how many habs to plot
nhabs<-nrow(hablist)
for(h in 1:nhabs) {
  #which habitat
  this.hab<-hablist$CLASS[h]
  this.lab<-hablist$LABEL[h]
  #create output for image for this habitat
  loc.name<-paste0('sim_random_sampling_30pc_error\\sim_random_sampling_error30_',gsub(" ", "_", this.hab),'.png')
  png(loc.name,width=1000,height=1500)
  #set image parameters for png output
  par(mfrow=c(3,2))
  par(mar=c(6,9,4,2))
  par(mgp=c(0,2.5,0))
  par(tcl=-1)
  
  #get results for this habitat
  this.results<-results[results$CLASS==this.hab,]
  #process each grid scale
  grids<-unique(this.results$gridres)
  for(g in 1:length(grids)) {
    this.grid<-grids[g]
    this.results.grid<-this.results[this.results$gridres==this.grid,]
    this.error<-unique(this.results.grid$error.expected)
    boxplot(data=this.results.grid,n.parcels.checked~nsqr,cex.axis=2.5,main=paste0(this.grid,': No. parcels checked'),cex.main=3.25,las=1)
    boxplot(data=this.results.grid,error.rate~nsqr,cex.axis=2.5,main=paste0(this.grid,': Error estimates'),cex.main=3.25,ylim=c(0,1),las=1)
    #add a line for where error is expected to be
    abline(h=this.error,col='red')
  }
  dev.off()
}

#summarise how often the "correct" answer is found. Acceptable precision of +/-5% (arithmetic) on the known error rate
#replace NaNs with zeroes for this classification of whether error was correctly assigned
precision<-0.01
error<-0.30
precision.lower<-error-precision
precision.upper<-error+precision
results$error.rate<-ifelse(is.nan(results$error.rate),0,results$error.rate)
results$est.correct<-ifelse(results$error.rate>=precision.lower & results$error.rate<=precision.upper,100,0)
p.correct<-aggregate(data=results,est.correct~CLASS+gridres+nsqr,mean)
head(p.correct)
#p.correct<-p.correct[p.correct$gridres=='500m',]
p.correct$est.correct<-ifelse(p.correct$est.correct>=95,1,0)
habs.correct<-aggregate(data=p.correct,est.correct~nsqr+gridres,sum)

loc.name<-paste0('sim_random_sampling_30pc_error\\nhabs_correctly_assessed_precision_',precision,'.png')
png(loc.name,width=400,height=400)
par(mar=c(5.1,4.1,0.2,0.2))
plot(habs.correct$nsqr[habs.correct$gridres=='100m'],habs.correct$est.correct[habs.correct$gridres=='100m'],ylim=c(0,35),xlab='Sample size',ylab='Habitats correctly assessed',pch=16,type='l',lty=1, cex.lab=1.25)
par(new=T)
plot(habs.correct$nsqr[habs.correct$gridres=='200m'],habs.correct$est.correct[habs.correct$gridres=='200m'],ylim=c(0,35),xlab='Sample size',ylab='Habitats correctly assessed',pch=16,type='l',lty=2, cex.lab=1.25)
par(new=T)
plot(habs.correct$nsqr[habs.correct$gridres=='500m'],habs.correct$est.correct[habs.correct$gridres=='500m'],ylim=c(0,35),xlab='Sample size',ylab='Habitats correctly assessed',pch=16,type='l',lty=3, cex.lab=1.25)
leg.text<-c('100-m','200-m','500-m')
legend('topleft',leg.text,lty=c(1,2,3))
dev.off()


#save results
#table.loc<-'sim_random_sampling_30pc_error\\how_often_correct.csv'
#write.table(p.correct,table.loc,row.names=F,sep=',')


##################################################################################################################################################
# ALL SCALES, WIDE RANGE OF SAMPLE SIZES, 15% ERROR ---------------------------------------------------------------
##################################################################################################################################################
#create population of scenarios
sqrs.min<-100
sqrs.max<-2000
sqrs.step<-100
reps<-1000
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
saveRDS(results,file='sim_random_sampling_15pc_error\\sim_random_sampling_results_3scales_100-2000sqrs_error15.rds')
#results<-readRDS(file='sim_random_sampling_15pc_error\\sim_random_sampling_results_3scales_100-2000sqrs_error15.rds')


#make boxplots
#how many habs to plot
nhabs<-nrow(hablist)
for(h in 1:nhabs) {
  #which habitat
  this.hab<-hablist$CLASS[h]
  this.lab<-hablist$LABEL[h]
  #create output for image for this habitat
  loc.name<-paste0('sim_random_sampling_15pc_error\\sim_random_sampling_error15_',gsub(" ", "_", this.hab),'.png')
  png(loc.name,width=1000,height=1500)
  #set image parameters for png output
  par(mfrow=c(3,2))
  par(mar=c(6,9,4,2))
  par(mgp=c(0,2.5,0))
  par(tcl=-1)
  
  #get results for this habitat
  this.results<-results[results$CLASS==this.hab,]
  #process each grid scale
  grids<-unique(this.results$gridres)
  for(g in 1:length(grids)) {
    this.grid<-grids[g]
    this.results.grid<-this.results[this.results$gridres==this.grid,]
    this.error<-unique(this.results.grid$error.expected)
    boxplot(data=this.results.grid,n.parcels.checked~nsqr,cex.axis=2.5,main=paste0(this.grid,': No. parcels checked'),cex.main=3.25,las=1)
    boxplot(data=this.results.grid,error.rate~nsqr,cex.axis=2.5,main=paste0(this.grid,': Error estimates'),cex.main=3.25,ylim=c(0,1),las=1)
    #add a line for where error is expected to be
    abline(h=this.error,col='red')
  }
  dev.off()
}


#summarise how often the "correct" answer is found. Acceptable precision of +/-5% (arithmetic) on the known error rate
#replace NaNs with zeroes for this classification of whether error was correctly assigned
precision<-0.05
error<-0.15
precision.lower<-error-precision
precision.upper<-error+precision
results$error.rate<-ifelse(is.nan(results$error.rate),0,results$error.rate)
results$est.correct<-ifelse(results$error.rate>=precision.lower & results$error.rate<=precision.upper,100,0)
p.correct.vals<-aggregate(data=results,est.correct~CLASS+gridres+nsqr,mean)
head(p.correct.vals)
p.correct<-p.correct.vals
p.correct$est.correct<-ifelse(p.correct$est.correct>=95,1,0)
habs.correct<-aggregate(data=p.correct,est.correct~nsqr+gridres,sum)

#for 500-m grid, what is lowest sample size where 95% correct assessment?
p.correct.vals.500m<-subset(p.correct.vals,gridres=='500m')
p.correct.vals.500m<-subset(p.correct.vals.500m,est.correct>=95)
#keep just the first row for each habitat as this is the lowest sample size that achieves >=95% certainty
p.correct.vals.500m[!duplicated(p.correct.vals.500m$CLASS),]



#produce graphs of number of habitats correctly assessed according to sampling design
loc.name<-paste0('sim_random_sampling_15pc_error\\nhabs_correctly_assessed_precision_',precision,'.png')
png(loc.name,width=400,height=400)
par(mar=c(5.1,4.1,0.2,0.2))
plot(habs.correct$nsqr[habs.correct$gridres=='100m'],habs.correct$est.correct[habs.correct$gridres=='100m'],ylim=c(0,35),xlab='Sample size',ylab='Habitats correctly assessed',pch=16,type='l',lty=1, cex.lab=1.25)
par(new=T)
plot(habs.correct$nsqr[habs.correct$gridres=='200m'],habs.correct$est.correct[habs.correct$gridres=='200m'],ylim=c(0,35),xlab='Sample size',ylab='Habitats correctly assessed',pch=16,type='l',lty=2, cex.lab=1.25)
par(new=T)
plot(habs.correct$nsqr[habs.correct$gridres=='500m'],habs.correct$est.correct[habs.correct$gridres=='500m'],ylim=c(0,35),xlab='Sample size',ylab='Habitats correctly assessed',pch=16,type='l',lty=3, cex.lab=1.25)
leg.text<-c('100-m','200-m','500-m')
legend('topleft',leg.text,lty=c(1,2,3))
dev.off()

#save results
#table.loc<-'sim_random_sampling_15pc_error\\how_often_correct.csv'
#write.table(p.correct,table.loc,row.names=F,sep=',')

#which habitats fail at 2000 square sample size?
#subset(p.correct.vals,nsqr==2000 & gridres=='500m' & est.correct<95,select=c('CLASS','est.correct'))
#head(n1000)
