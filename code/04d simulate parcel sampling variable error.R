#simulate parcel sampling and different errors and produce contour plots of power
#Simon Gillings
#March 2016

#set working directory
setwd('X:\\Shared Projects\\Defra80 - EODIP5\\gis analysis\\')

#loop through different sample sizes and error rates with 10000 replicates
reps<-10000
nparcels<-seq(50,10000,by=50)
errors<-seq(0.10,0.9,by=0.1)
for(i in 1:length(nparcels)) {
  this.sample.size<-nparcels[i]
  for(e in 1:length(errors)) {
    this.error<-errors[e]
    event<-rbinom(reps,this.sample.size,this.error)
    result<-data.frame(nparcels=rep(this.sample.size,reps),error=rep(this.error,reps),events=event)
    if((e*i)==1) {results<-result}
    if((e*i)>1) {results<-rbind(results,result)}
  }
}



#calculate the error rate
results$error.rate<-results$events/results$nparcels
#assess whether error estimate is correct using different levels of precision
results$est.correct5<-ifelse(results$error.rate>=(results$error-0.05) & results$error.rate<=(results$error+0.05),100,0)
results$est.correct2<-ifelse(results$error.rate>=(results$error-0.02) & results$error.rate<=(results$error+0.02),100,0)
results$est.correct1<-ifelse(results$error.rate>=(results$error-0.01) & results$error.rate<=(results$error+0.01),100,0)

#aggregate to get the percentage of simulation runs where error is correctly estimated
p.correct5<-aggregate(data=results,est.correct5~nparcels+error,mean)
p.correct2<-aggregate(data=results,est.correct2~nparcels+error,mean)
p.correct1<-aggregate(data=results,est.correct1~nparcels+error,mean)


#create colour ramps - all colours <95 different to above
library(RColorBrewer)
cont.cols.p<-colorRampPalette(c('red','pink'),space="rgb")
zlevels<-seq(0,100,by=5)

#setup the values for the 3d surface plot; needs x and y as sequential values, and z as a matrix
p5<-p.correct5
p.correct5<-subset(p.correct5,nparcels<=500)
x<-unique(p.correct5$nparcels)
y<-unique(p.correct5$error)
z<-matrix(p.correct5$est.correct5,nrow =length(x) ,ncol = length(y))
cont.cols<-c(cont.cols.p(19),'grey')
loc.name<-paste0('sim_parcel_random_sampling\\how_often_correctly_assessed_multipc_precision5.png')
png(loc.name,width=600,height=400)
par(mar=c(5.1,4.1,1.5,0.2))
filled.contour(x,y,z,levels=zlevels,xlab='Sample size',ylab='Error rate',col=cont.cols,cex.lab=1.25,key.title=title('% true'))
dev.off()

#setup the values for the 3d surface plot; needs x and y as sequential values, and z as a matrix
p2<-p.correct2
p.correct2<-subset(p.correct2,nparcels<=3000)
x<-unique(p.correct2$nparcels)
y<-unique(p.correct2$error)
z<-matrix(p.correct2$est.correct2,nrow =length(x) ,ncol = length(y))
loc.name<-paste0('sim_parcel_random_sampling\\how_often_correctly_assessed_multipc_precision2.png')
png(loc.name,width=600,height=400)
par(mar=c(5.1,4.1,1.5,0.2))
filled.contour(x,y,z,levels=zlevels,xlab='Sample size',ylab='Error rate',col=cont.cols,cex.lab=1.25,key.title=title('% true'))
dev.off()

#setup the values for the 3d surface plot; needs x and y as sequential values, and z as a matrix
x<-unique(p.correct1$nparcels)
y<-unique(p.correct1$error)
z<-matrix(p.correct1$est.correct1,nrow =length(x) ,ncol = length(y))
loc.name<-paste0('sim_parcel_random_sampling\\how_often_correctly_assessed_multipc_precision1.png')
png(loc.name,width=600,height=400)
par(mar=c(5.1,4.1,1.5,0.2))
filled.contour(x,y,z,levels=zlevels,xlab='Sample size',ylab='Error rate',col=cont.cols,cex.lab=1.25,key.title=title('% true'))
dev.off()


