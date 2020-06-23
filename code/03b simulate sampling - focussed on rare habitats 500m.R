#simulate sampling and error - sampling based on taking squares with n rare habitats, and seeing how much of other habitats are covered in this way
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

#summarise number of parcels per hab
n.parcels.byhab<-aggregate(data=df,count~CLASS,NROW)
n.parcels.byhab<-n.parcels.byhab[order(n.parcels.byhab$count),]
head(n.parcels.byhab,n=10)


#simulate random sampling
simulate.random<-function(repnum,nrarehabs=2,nsqr=50,gridres='200m',errorlevel='mid') {
  #repnum = counter to show progress on reps
  #nrarehabs = how many rare habitats to use as basis for sampling
  #nsqr = number of squares in sample
  #gridres = what size grid squares to use
  #errorlevel = use low, mid or high error estimate
  cat(paste(repnum,nsqr,gridres,errorlevel,'\n'))
  
  #get the relevant list of squares in the region and assign to object squares 
  assign("squares",get(apropos(gridres) ) )
  
  #create a deduped list of all habitat classes present in squares of this size
  square.hab.list<-df[,c(2,which(names(df)==paste0('ref',gridres)))]
  square.hab.list<-unique(square.hab.list)
  
  #find the n rare habitats
  rarehabs<-n.parcels.byhab[1:nrarehabs,]
  
  #process each rare habitat
  sample<-character()
  for(r in 1:nrarehabs) {
    #which hab
    this.rarehab<-rarehabs$CLASS[r]
    #get all squares that have this hab
    squares.with.this.rarehab<-square.hab.list[square.hab.list$CLASS==this.rarehab,2]
    #make a sample of squares at this resolution (without replacement)
    #first need to force sample size to be no bigger than availability for this habitat
    this.nsqr<-ifelse(length(squares.with.this.rarehab)<nsqr,length(squares.with.this.rarehab),nsqr)
    this.sample<-as.character(sample(squares.with.this.rarehab, replace=F, size=this.nsqr, prob=NULL))
    sample<-c(sample,this.sample)
  }
  #dedupe sample as a square could be listed >1 times owing to having more than one rare habitat
  sample<-unique(sample)
  
  #how many squares are to be sampled?
  total.squares<-length(sample)
  
  #get all parcels in these squares, after checking which column holds the grid refs at this scale
  i<-which(names(df)==paste0('ref',gridres))
  this.parcels<-df[df[,i] %in% sample,2]
  
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
  this.hab.freqs$nrarehabs<-nrarehabs
  this.hab.freqs$nsqr<-nsqr
  this.hab.freqs$gridres<-gridres
  this.hab.freqs$errorlevel<-errorlevel
  this.hab.freqs$error.expected<-this.hab.freqs[,e]
  this.hab.freqs$total.squares<-total.squares
  this.hab.freqs<-this.hab.freqs[,c(1,6,8,9,10,11,12,13,14,15)]
  return(this.hab.freqs)
}


#create population of scenarios
rarehabs.min<-2
rarehabs.max<-10
reps<-1000
#expand.grid creates all permutations of whatever vectors are fed in. 
scenarios<-expand.grid(seq(from=1,to=reps,by=1),
                       seq(from=rarehabs.min,to=rarehabs.max,by=2),
                       c(50,100,200),
                       '500m',
                       'mid',
                       stringsAsFactors = F)
names(scenarios)<-c('repnum','nrarehabs','nsqr','gridres','errorlevel')
#how many scenarios
dim(scenarios)

#apply the simulate function to all scenarios
Sys.time()
results<-mapply(simulate.random,scenarios$repnum,scenarios$nrarehabs,scenarios$nsqr,scenarios$gridres,scenarios$errorlevel,SIMPLIFY=F)
Sys.time()
#convert list of dataframes to one big dataframe
results <- ldply(results, data.frame)
# str(results)
head(results)

saveRDS(results,file='sim_byrarehab_500m_50-100-200sqrs_15pc_error//sim_byrarehab_sampling_results_500m_50-100-200_error15.rds')
#results<-readRDS(file='sim_byrarehab_500m_50-100-200sqrs_15pc_error//sim_byrarehab_sampling_results_500m_50-100-200_error15.rds')

#check how total sample size across all habitats compares
#results are same for all habitats, so pick one to ensure boxplot/aggregate functions calculated correctly
results.total.sample.size<-subset(results,CLASS=='Arable',select=c(nsqr,gridres,total.squares,nrarehabs))
results.total.sample.size$expected.samp.size<-results.total.sample.size$nsqr*results.total.sample.size$nrarehabs
results.total.sample.size$p.samp.size<-100-(100*(results.total.sample.size$total.squares/results.total.sample.size$expected.samp.size))

#what is mean sample size for each strategy
aggregate(data=results.total.sample.size,total.squares~nsqr+gridres+nrarehabs,mean)
#how much is this smaller than the full samp size * nhabs expectation?
aggregate(data=results.total.sample.size,p.samp.size~nsqr+gridres+nrarehabs,mean)


#make boxplots
#how many habs to plot
nhabs<-nrow(hablist)
for(h in 1:nhabs) {
  #which habitat
  this.hab<-hablist$CLASS[h]
  this.lab<-hablist$LABEL[h]
  #create output for image for this habitat
  loc.name<-paste0('sim_byrarehab_500m_50-100-200sqrs_15pc_error//sim_byrarehab_sampling_500m_50-100-200_error15_',gsub(" ", "_", this.hab),'.png')
  png(loc.name,width=1000,height=1500)
  #set image parameters for png output
  par(mfrow=c(3,2))
  par(mar=c(6,9,4,2))
  par(mgp=c(0,2.5,0))
  par(tcl=-1)
  
  #get results for this habitat
  this.results<-results[results$CLASS==this.hab & results$gridres=='500m',]
  #process each grid scale
  sampsizes<-c(50,100,200)
  for(s in 1:length(sampsizes)) {
    this.sampsize<-sampsizes[s]
    this.results.sampsize<-this.results[this.results$nsqr==this.sampsize,]
    #force instances where zero parcels (NaN error) to -1 to force plotting of x values
    this.results.sampsize$error.rate<-ifelse(is.nan(this.results.sampsize$error.rate),-1,this.results.sampsize$error.rate)
    this.error<-unique(this.results.sampsize$error.expected)
    boxplot(data=this.results.sampsize,n.parcels.checked~nrarehabs,cex.axis=2.5,main=paste0(this.sampsize,'sqrs: No. parcels checked'),cex.main=3.25,las=1)
    boxplot(data=this.results.sampsize,error.rate~nrarehabs,cex.axis=2.5,main=paste0(this.sampsize,'sqrs: Error estimates'),cex.main=3.25,ylim=c(0,1),las=1)
    #add a line for where error is expected to be
    abline(h=this.error,col='red')
    abline(h=this.error+0.05,col='red',lty=3)
    abline(h=this.error-0.05,col='red',lty=3)
    
  }
  dev.off()
}



#how many of the rare habitats are adequately covered for each sample size?
rare.habs<-n.parcels.byhab[1:10,]
just.rares<-results[results$CLASS %in% rare.habs$CLASS,]
head(just.rares)

#summarise how often the "correct" answer is found. Acceptable precision of +/-5% (arithmetic) on the known error rate
just.rares$est.correct<-ifelse(just.rares$error.rate>=0.1 & just.rares$error.rate<=0.2,100,0)
p.correct<-aggregate(data=just.rares,est.correct~CLASS+gridres+nsqr+nrarehabs,mean)
n.parcels.checked<-aggregate(data=just.rares,n.parcels.checked~CLASS+gridres+nsqr+nrarehabs,mean)
p.correct$est.correct<-ifelse(p.correct$est.correct>=95,1,0)
habs.correct<-aggregate(data=p.correct,est.correct~nsqr+gridres+nrarehabs,sum)
habs.correct

#for non-rare habitats how often the "correct" answer is found. Acceptable precision of +/-5% (arithmetic) on the known error rate
not.rares<-results[!results$CLASS %in% rare.habs$CLASS,]
not.rares$est.correct<-ifelse(not.rares$error.rate>=0.1 & not.rares$error.rate<=0.2,100,0)
p.correct.nr<-aggregate(data=not.rares,est.correct~CLASS+gridres+nsqr+nrarehabs,mean)
p.correct.nr$est.correct<-ifelse(p.correct.nr$est.correct>=95,1,0)
habs.correct.nr<-aggregate(data=p.correct.nr,est.correct~nsqr+gridres+nrarehabs,sum)
habs.correct.nr
