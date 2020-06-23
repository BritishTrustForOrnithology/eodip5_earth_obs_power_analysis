#simulate sampling and error - based on earlier results (prog 03c), select particular N squares per habitat type
#Simon Gillings
#March 2016
library(plyr)

#set working directory
setwd('X:\\Shared Projects\\Defra80 - EODIP5\\gis analysis\\')

# LOAD DATA ------------------------------------------------------------------------------------------
#load data
df<-readRDS('parcels_with_grids.rds')

#load habitat short names
hablabs<-read.table(file='hablabels.csv',sep=',',header=T,colClasses = rep('character',2))

#load the required number of squares to be sampled per habitat - this is to be used on a habitat-specific basis instead of 50,100 etc
sqrs.per.hab<-read.table(file='optimal_sqrs_per_hab.csv',sep=',',header=T,colClasses = c('character','numeric'))


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
simulate.random<-function(repnum,nrarehabs=nrow(hablist),gridres='200m',errorlevel='mid') {
  #repnum = counter to show progress on reps
  #nrarehabs = how many habitats to use as basis for sampling - here default is to use all (i.e. not just the rarest)
  #gridres = what size grid squares to use
  #errorlevel = use low, mid or high error estimate
  cat(paste(repnum,gridres,errorlevel,'\n'))
  
  #get the relevant list of squares in the region and assign to object squares 
  assign("squares",get(apropos(gridres) ) )
  
  #create a deduped list of all habitat classes present in squares of this size
  square.hab.list<-df[,c(2,which(names(df)==paste0('ref',gridres)))]
  square.hab.list<-unique(square.hab.list)
  
  #find the n habitats
  rarehabs<-n.parcels.byhab[1:nrarehabs,]
  
  #process each habitat
  sample<-character()
  for(r in 1:nrarehabs) {
    #which hab
    this.rarehab<-rarehabs$CLASS[r]
    #get required sample size for this hab
    this.opt.nsqr<-sqrs.per.hab[sqrs.per.hab$CLASS==this.rarehab,2]
    #get all squares that have this hab
    squares.with.this.rarehab<-square.hab.list[square.hab.list$CLASS==this.rarehab,2]
    #make a sample of squares at this resolution (without replacement)
    this.sample<-as.character(sample(squares.with.this.rarehab, replace=F, size=this.opt.nsqr, prob=NULL))
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
  this.hab.freqs$nsqr<-this.opt.nsqr
  this.hab.freqs$gridres<-gridres
  this.hab.freqs$errorlevel<-errorlevel
  this.hab.freqs$error.expected<-this.hab.freqs[,e]
  this.hab.freqs$total.squares<-total.squares
  this.hab.freqs<-this.hab.freqs[,c(1,6,8,9,10,11,12,13,14,15)]
  return(this.hab.freqs)
}


#create population of scenarios
reps<-100
#expand.grid creates all permutations of whatever vectors are fed in. 
scenarios<-expand.grid(seq(from=1,to=reps,by=1), #how many replicates
                       nrow(hablist), #how many habitats to consider
                       '500m', #what grid scale to choose
                       'mid', #what error level to use
                       stringsAsFactors = F)
names(scenarios)<-c('repnum','nrarehabs','gridres','errorlevel')
#how many scenarios
dim(scenarios)

#apply the simulate function to all scenarios
Sys.time()
results<-mapply(simulate.random,scenarios$repnum,scenarios$nrarehabs,scenarios$gridres,scenarios$errorlevel,SIMPLIFY=F)
Sys.time()
#convert list of dataframes to one big dataframe
results <- ldply(results, data.frame)
# str(results)
# head(results)

saveRDS(results,file='sim_byhab_50-100-200sqrs_15pc_error_no_gardens_optimal\\sim_byhab_50-100-200sqrs_15pc_error_no_gardens_optimal.rds')
#results<-readRDS(file='sim_byhab_50-100-200sqrs_15pc_error_no_gardens_optimal\\sim_byhab_50-100-200sqrs_15pc_error_no_gardens_optimal.rds')

head(results)

#check how total sample size across all habitats compares
#note expected worst case total sample size with no habitat coincidence is:
expected.samp.size<-sum(sqrs.per.hab$opt.nsqr)
#results are same for all habitats, so pick one to ensure boxplot/aggregate functions calculated correctly
results.total.sample.size<-subset(results,CLASS=='Arable',select=c(nsqr,gridres,total.squares,nrarehabs))
results.total.sample.size$p.samp.size<-100-(100*(results.total.sample.size$total.squares/expected.samp.size))

#what is mean sample size for each strategy
aggregate(data=results.total.sample.size,total.squares~nsqr+gridres,mean)
#how much is this smaller than the full samp size * nhabs expectation?
aggregate(data=results.total.sample.size,p.samp.size~nsqr+gridres,mean)


#summarise how often the "correct" answer is found. Acceptable precision of +/-5% (arithmetic) on the known error rate
#need to consider if answer is absolute because all parcels were checked
results$est.correct<-ifelse(results$error.rate>=0.1 & results$error.rate<=0.2,100,0)
n.parcels.checked<-aggregate(data=results,n.parcels.checked~CLASS+gridres+nsqr,mean)
n.parcels.checked<-merge(n.parcels.checked,n.parcels.byhab,by='CLASS')
absolute<-subset(n.parcels.checked,n.parcels.checked==count)
n.parcels.checked[n.parcels.checked$gridres=='500m',]
p.correct<-aggregate(data=results,est.correct~CLASS+gridres+nsqr,mean)
head(p.correct)


