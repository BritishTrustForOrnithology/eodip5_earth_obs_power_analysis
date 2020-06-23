#simulate parcel sampling and error (30% error) - this is for the Desk-based component which will use parcel based sampling
#Simon Gillings
#March 2016

#set working directory
setwd('X:\\Shared Projects\\Defra80 - EODIP5\\gis analysis\\')

reps<-10000
nparcels<-seq(50,10000,by=10)
for(i in 1:length(nparcels)) {
  this.sample.size<-nparcels[i]
  event<-rbinom(reps,this.sample.size,0.30)
  result<-data.frame(nparcels=rep(this.sample.size,reps),events=event)
  if(i==1) {results<-result}
  if(i>1) {results<-rbind(results,result)}
}

#calculate the error rate
results$error.rate<-results$events/results$nparcels
#assess whether error estimate is correct using different levels of precision
results$est.correct5<-ifelse(results$error.rate>=0.25 & results$error.rate<=0.35,100,0)
results$est.correct2<-ifelse(results$error.rate>=0.28 & results$error.rate<=0.32,100,0)
results$est.correct1<-ifelse(results$error.rate>=0.29 & results$error.rate<=0.31,100,0)
#aggregate to get the percentage of simulation runs where error is correctly estimated
p.correct5<-aggregate(data=results,est.correct5~nparcels,mean)
p.correct2<-aggregate(data=results,est.correct2~nparcels,mean)
p.correct1<-aggregate(data=results,est.correct1~nparcels,mean)

#produce graph
loc.name<-paste0('sim_parcel_random_sampling\\how_often_correctly_assessed_30pc.png')
png(loc.name,width=400,height=400)
par(mar=c(5.1,4.1,0.2,0.2))
plot(p.correct5$nparcels,p.correct5$est.correct5,xlim=c(0,2000), ylim=c(0,100),xlab='Sample size',ylab='% correctly assessed',pch=16,type='l',lty=1,cex.lab=1.25)
par(new=T)
plot(p.correct2$nparcels,p.correct2$est.correct2,xlim=c(0,2000), ylim=c(0,100),xlab='Sample size',ylab='% correctly assessed',pch=16,type='l',lty=2,cex.lab=1.25)
par(new=T)
plot(p.correct1$nparcels,p.correct1$est.correct1,xlim=c(0,2000), ylim=c(0,100),xlab='Sample size',ylab='% correctly assessed',pch=16,type='l',lty=3,cex.lab=1.25)
abline(h=95,col='red')
leg.text<-c('5% precision','2% precision','1% precision')
legend('bottomright',leg.text,lty=c(1,2,3))
dev.off()

#at what point does each line cross the 95% threshold
subset(p.correct5,est.correct5>=95 & est.correct5<=96)
subset(p.correct2,est.correct2>=95 & est.correct2<=96)
subset(p.correct1,est.correct1>=95 & est.correct1<=96)




