#make a graph for report - example of simulation output
#Simon Gillings
#March 2016


results<-readRDS(file='X:\\Shared Projects\\Defra80 - EODIP5\\gis analysis\\sim_random_sampling_15pc_error\\sim_random_sampling_results_3scales_100-2000sqrs_error15.rds')
head(results,n=35)
this.results.grid<-subset(results,gridres=='500m' & CLASS=='Dune Grassland')
loc.name<-paste0('X:\\Shared Projects\\Defra80 - EODIP5\\gis analysis\\sim_random_sampling_15pc_error\\sim_random_sampling_error15_',gsub(" ", "_", this.hab),'.png')
png(loc.name,width=1000,height=1500)
#create output for image for this habitat
#set image parameters for png output
#par(mfrow=c(3,2))
#par(mgp=c(0,2.5,0))
#par(tcl=-1)

loc.name<-paste0('X:\\Shared Projects\\Defra80 - EODIP5\\gis analysis\\sim_random_sampling_15pc_error\\example_graph_nparcels.png')
png(loc.name,width=400,height=400)
par(mar=c(5.1,4.1,0.2,0.2))
boxplot(data=this.results.grid,n.parcels.checked~nsqr,cex.lab=1.25,xlab='Sample size',ylab='N parcels checked',las=1)
dev.off()


loc.name<-paste0('X:\\Shared Projects\\Defra80 - EODIP5\\gis analysis\\sim_random_sampling_15pc_error\\example_graph_error.png')
png(loc.name,width=400,height=400)
par(mar=c(5.1,4.1,0.2,0.2))
boxplot(data=this.results.grid,error.rate~nsqr,cex.lab=1.25,xlab='Sample size',ylab='Error rate',ylim=c(0,1),las=1)
#add a line for where error is expected to be
abline(h=0.15,col='red')
abline(h=0.1,col='red',lty=2)
abline(h=0.2,col='red',lty=2)
dev.off()



}
