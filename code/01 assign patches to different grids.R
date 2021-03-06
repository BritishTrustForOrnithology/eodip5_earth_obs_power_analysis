#read in Norfolk Map parcels data; this has coordinates of centroid of each parcel; convert 
#these to give British National Grid reference for different grid resolutions
#Simon Gillings
#February 2016

#set working directory
setwd('X:\\Shared Projects\\Defra80 - EODIP5\\gis analysis\\')

#read a csv file containing all the 
df<-read.table('..\\gis data\\parcel_centroids.csv',sep=',',header=T,colClasses=c('numeric','character','numeric','numeric'))

head(df)
# FID                                                      CLASS  easting northing
# 1   0 Coastal and Floodplain Grazing Marsh (medium productivity) 552713.4 292678.5
# 2   1                                                     Arable 554910.0 292500.8
# 3   2                                                     Arable 556177.6 292882.3
# 4   3                                                     Arable 556910.6 292510.1
# 5   4                                                     Arable 557483.1 292942.8
# 6   5                                                     Arable 563098.1 290526.5

#check extent of eastings and northings before conversion
summary(df$easting)
summary(df$northing)

# convert eastings and northings to grids ------------------------------------------------------------------------------------
#split easting and northing into components
df$e100<-floor( df$easting/100000)
df$e10 <-floor( (df$easting-(df$e100*100000) ) /10000)
df$e1  <-floor( (df$easting-((df$e100*100000)+(df$e10*10000) ))  /1000)
df$e01 <-floor( (df$easting-((df$e100*100000)+(df$e10*10000)+(df$e1*1000) )) /100)
df$n100<-floor( df$northing/100000)
df$n10 <-floor((df$northing-(df$n100*100000) ) /10000)
df$n1  <-floor((df$northing-((df$n100*100000)+(df$n10*10000) )) /1000)
df$n01 <-floor((df$northing-((df$n100*100000)+(df$n10*10000)+(df$n1*1000) )) /100)

#code up 100-km, giving Northern Irish squares off the BNG a dummy XX before removal
df$hundref<-NA
df$hundref<-ifelse(df$e100==0 & df$n100==0,'SV',df$hundref)
df$hundref<-ifelse(df$e100==1 & df$n100==0,'SW',df$hundref)
df$hundref<-ifelse(df$e100==2 & df$n100==0,'SX',df$hundref)
df$hundref<-ifelse(df$e100==3 & df$n100==0,'SY',df$hundref)
df$hundref<-ifelse(df$e100==4 & df$n100==0,'SZ',df$hundref)
df$hundref<-ifelse(df$e100==5 & df$n100==0,'TV',df$hundref)
df$hundref<-ifelse(df$e100==1 & df$n100==1,'SR',df$hundref)
df$hundref<-ifelse(df$e100==2 & df$n100==1,'SS',df$hundref)
df$hundref<-ifelse(df$e100==3 & df$n100==1,'ST',df$hundref)
df$hundref<-ifelse(df$e100==4 & df$n100==1,'SU',df$hundref)
df$hundref<-ifelse(df$e100==5 & df$n100==1,'TQ',df$hundref)
df$hundref<-ifelse(df$e100==6 & df$n100==1,'TR',df$hundref)
df$hundref<-ifelse(df$e100==1 & df$n100==2,'SM',df$hundref)
df$hundref<-ifelse(df$e100==2 & df$n100==2,'SN',df$hundref)
df$hundref<-ifelse(df$e100==3 & df$n100==2,'SO',df$hundref)
df$hundref<-ifelse(df$e100==4 & df$n100==2,'SP',df$hundref)
df$hundref<-ifelse(df$e100==5 & df$n100==2,'TL',df$hundref)
df$hundref<-ifelse(df$e100==6 & df$n100==2,'TM',df$hundref)
df$hundref<-ifelse(df$e100==2 & df$n100==3,'SH',df$hundref)
df$hundref<-ifelse(df$e100==3 & df$n100==3,'SJ',df$hundref)
df$hundref<-ifelse(df$e100==4 & df$n100==3,'SK',df$hundref)
df$hundref<-ifelse(df$e100==5 & df$n100==3,'TF',df$hundref)
df$hundref<-ifelse(df$e100==6 & df$n100==3,'TG',df$hundref)
df$hundref<-ifelse(df$e100==0 & df$n100==4,'XX',df$hundref)
df$hundref<-ifelse(df$e100==1 & df$n100==4,'XX',df$hundref)
df$hundref<-ifelse(df$e100==2 & df$n100==4,'SC',df$hundref)
df$hundref<-ifelse(df$e100==3 & df$n100==4,'SD',df$hundref)
df$hundref<-ifelse(df$e100==4 & df$n100==4,'SE',df$hundref)
df$hundref<-ifelse(df$e100==5 & df$n100==4,'TA',df$hundref)
df$hundref<-ifelse(df$e100==0 & df$n100==5,'XX',df$hundref)
df$hundref<-ifelse(df$e100==1 & df$n100==5,'NW',df$hundref)
df$hundref<-ifelse(df$e100==2 & df$n100==5,'NX',df$hundref)
df$hundref<-ifelse(df$e100==3 & df$n100==5,'NY',df$hundref)
df$hundref<-ifelse(df$e100==4 & df$n100==5,'NZ',df$hundref)
df$hundref<-ifelse(df$e100==1 & df$n100==6,'NR',df$hundref)
df$hundref<-ifelse(df$e100==2 & df$n100==6,'NS',df$hundref)
df$hundref<-ifelse(df$e100==3 & df$n100==6,'NT',df$hundref)
df$hundref<-ifelse(df$e100==4 & df$n100==6,'NU',df$hundref)
df$hundref<-ifelse(df$e100==0 & df$n100==7,'NL',df$hundref)
df$hundref<-ifelse(df$e100==1 & df$n100==7,'NM',df$hundref)
df$hundref<-ifelse(df$e100==2 & df$n100==7,'NN',df$hundref)
df$hundref<-ifelse(df$e100==3 & df$n100==7,'NO',df$hundref)
df$hundref<-ifelse(df$e100==0 & df$n100==8,'NF',df$hundref)
df$hundref<-ifelse(df$e100==1 & df$n100==8,'NG',df$hundref)
df$hundref<-ifelse(df$e100==2 & df$n100==8,'NH',df$hundref)
df$hundref<-ifelse(df$e100==3 & df$n100==8,'NJ',df$hundref)
df$hundref<-ifelse(df$e100==4 & df$n100==8,'NK',df$hundref)
df$hundref<-ifelse(df$e100==0 & df$n100==9,'NA',df$hundref)
df$hundref<-ifelse(df$e100==1 & df$n100==9,'NB',df$hundref)
df$hundref<-ifelse(df$e100==2 & df$n100==9,'NC',df$hundref)
df$hundref<-ifelse(df$e100==3 & df$n100==9,'ND',df$hundref)
df$hundref<-ifelse(df$e100==3 & df$n100==10,'HY',df$hundref)
df$hundref<-ifelse(df$e100==4 & df$n100==10,'HZ',df$hundref)
df$hundref<-ifelse(df$e100==3 & df$n100==11,'HT',df$hundref)
df$hundref<-ifelse(df$e100==4 & df$n100==11,'HU',df$hundref)
df$hundref<-ifelse(df$e100==4 & df$n100==12,'HP',df$hundref)

#code 500-m grid
df$let500m<-NA
df$let500m<-ifelse(df$e01<=4 & df$n01<=4,'SW',df$let500m)
df$let500m<-ifelse(df$e01>=5 & df$n01<=4,'SE',df$let500m)
df$let500m<-ifelse(df$e01<=4 & df$n01>=5,'NW',df$let500m)
df$let500m<-ifelse(df$e01>=4 & df$n01>=5,'NE',df$let500m)

#code 200-m grid
df$let200m<-NA
df$let200m<-ifelse(df$e01==0 & df$n01==0,'A',df$let200m)
df$let200m<-ifelse(df$e01==0 & df$n01==1,'A',df$let200m)
df$let200m<-ifelse(df$e01==1 & df$n01==0,'A',df$let200m)
df$let200m<-ifelse(df$e01==1 & df$n01==1,'A',df$let200m)
df$let200m<-ifelse(df$e01==2 & df$n01==0,'F',df$let200m)
df$let200m<-ifelse(df$e01==2 & df$n01==1,'F',df$let200m)
df$let200m<-ifelse(df$e01==3 & df$n01==0,'F',df$let200m)
df$let200m<-ifelse(df$e01==3 & df$n01==1,'F',df$let200m)
df$let200m<-ifelse(df$e01==4 & df$n01==0,'K',df$let200m)
df$let200m<-ifelse(df$e01==4 & df$n01==1,'K',df$let200m)
df$let200m<-ifelse(df$e01==5 & df$n01==0,'K',df$let200m)
df$let200m<-ifelse(df$e01==5 & df$n01==1,'K',df$let200m)
df$let200m<-ifelse(df$e01==6 & df$n01==0,'Q',df$let200m)
df$let200m<-ifelse(df$e01==6 & df$n01==1,'Q',df$let200m)
df$let200m<-ifelse(df$e01==7 & df$n01==0,'Q',df$let200m)
df$let200m<-ifelse(df$e01==7 & df$n01==1,'Q',df$let200m)
df$let200m<-ifelse(df$e01==8 & df$n01==0,'V',df$let200m)
df$let200m<-ifelse(df$e01==8 & df$n01==1,'V',df$let200m)
df$let200m<-ifelse(df$e01==9 & df$n01==0,'V',df$let200m)
df$let200m<-ifelse(df$e01==9 & df$n01==1,'V',df$let200m)
df$let200m<-ifelse(df$e01==0 & df$n01==2,'B',df$let200m)
df$let200m<-ifelse(df$e01==0 & df$n01==3,'B',df$let200m)
df$let200m<-ifelse(df$e01==1 & df$n01==2,'B',df$let200m)
df$let200m<-ifelse(df$e01==1 & df$n01==3,'B',df$let200m)
df$let200m<-ifelse(df$e01==2 & df$n01==2,'G',df$let200m)
df$let200m<-ifelse(df$e01==2 & df$n01==3,'G',df$let200m)
df$let200m<-ifelse(df$e01==3 & df$n01==2,'G',df$let200m)
df$let200m<-ifelse(df$e01==3 & df$n01==3,'G',df$let200m)
df$let200m<-ifelse(df$e01==4 & df$n01==2,'L',df$let200m)
df$let200m<-ifelse(df$e01==4 & df$n01==3,'L',df$let200m)
df$let200m<-ifelse(df$e01==5 & df$n01==2,'L',df$let200m)
df$let200m<-ifelse(df$e01==5 & df$n01==3,'L',df$let200m)
df$let200m<-ifelse(df$e01==6 & df$n01==2,'R',df$let200m)
df$let200m<-ifelse(df$e01==6 & df$n01==3,'R',df$let200m)
df$let200m<-ifelse(df$e01==7 & df$n01==2,'R',df$let200m)
df$let200m<-ifelse(df$e01==7 & df$n01==3,'R',df$let200m)
df$let200m<-ifelse(df$e01==8 & df$n01==2,'W',df$let200m)
df$let200m<-ifelse(df$e01==8 & df$n01==3,'W',df$let200m)
df$let200m<-ifelse(df$e01==9 & df$n01==2,'W',df$let200m)
df$let200m<-ifelse(df$e01==9 & df$n01==3,'W',df$let200m)
df$let200m<-ifelse(df$e01==0 & df$n01==4,'C',df$let200m)
df$let200m<-ifelse(df$e01==0 & df$n01==5,'C',df$let200m)
df$let200m<-ifelse(df$e01==1 & df$n01==4,'C',df$let200m)
df$let200m<-ifelse(df$e01==1 & df$n01==5,'C',df$let200m)
df$let200m<-ifelse(df$e01==2 & df$n01==4,'H',df$let200m)
df$let200m<-ifelse(df$e01==2 & df$n01==5,'H',df$let200m)
df$let200m<-ifelse(df$e01==3 & df$n01==4,'H',df$let200m)
df$let200m<-ifelse(df$e01==3 & df$n01==5,'H',df$let200m)
df$let200m<-ifelse(df$e01==4 & df$n01==4,'M',df$let200m)
df$let200m<-ifelse(df$e01==4 & df$n01==5,'M',df$let200m)
df$let200m<-ifelse(df$e01==5 & df$n01==4,'M',df$let200m)
df$let200m<-ifelse(df$e01==5 & df$n01==5,'M',df$let200m)
df$let200m<-ifelse(df$e01==6 & df$n01==4,'S',df$let200m)
df$let200m<-ifelse(df$e01==6 & df$n01==5,'S',df$let200m)
df$let200m<-ifelse(df$e01==7 & df$n01==4,'S',df$let200m)
df$let200m<-ifelse(df$e01==7 & df$n01==5,'S',df$let200m)
df$let200m<-ifelse(df$e01==8 & df$n01==4,'X',df$let200m)
df$let200m<-ifelse(df$e01==8 & df$n01==5,'X',df$let200m)
df$let200m<-ifelse(df$e01==9 & df$n01==4,'X',df$let200m)
df$let200m<-ifelse(df$e01==9 & df$n01==5,'X',df$let200m)
df$let200m<-ifelse(df$e01==0 & df$n01==6,'D',df$let200m)
df$let200m<-ifelse(df$e01==0 & df$n01==7,'D',df$let200m)
df$let200m<-ifelse(df$e01==1 & df$n01==6,'D',df$let200m)
df$let200m<-ifelse(df$e01==1 & df$n01==7,'D',df$let200m)
df$let200m<-ifelse(df$e01==2 & df$n01==6,'I',df$let200m)
df$let200m<-ifelse(df$e01==2 & df$n01==7,'I',df$let200m)
df$let200m<-ifelse(df$e01==3 & df$n01==6,'I',df$let200m)
df$let200m<-ifelse(df$e01==3 & df$n01==7,'I',df$let200m)
df$let200m<-ifelse(df$e01==4 & df$n01==6,'N',df$let200m)
df$let200m<-ifelse(df$e01==4 & df$n01==7,'N',df$let200m)
df$let200m<-ifelse(df$e01==5 & df$n01==6,'N',df$let200m)
df$let200m<-ifelse(df$e01==5 & df$n01==7,'N',df$let200m)
df$let200m<-ifelse(df$e01==6 & df$n01==6,'T',df$let200m)
df$let200m<-ifelse(df$e01==6 & df$n01==7,'T',df$let200m)
df$let200m<-ifelse(df$e01==7 & df$n01==6,'T',df$let200m)
df$let200m<-ifelse(df$e01==7 & df$n01==7,'T',df$let200m)
df$let200m<-ifelse(df$e01==8 & df$n01==6,'Y',df$let200m)
df$let200m<-ifelse(df$e01==8 & df$n01==7,'Y',df$let200m)
df$let200m<-ifelse(df$e01==9 & df$n01==6,'Y',df$let200m)
df$let200m<-ifelse(df$e01==9 & df$n01==7,'Y',df$let200m)
df$let200m<-ifelse(df$e01==0 & df$n01==8,'E',df$let200m)
df$let200m<-ifelse(df$e01==0 & df$n01==9,'E',df$let200m)
df$let200m<-ifelse(df$e01==1 & df$n01==8,'E',df$let200m)
df$let200m<-ifelse(df$e01==1 & df$n01==9,'E',df$let200m)
df$let200m<-ifelse(df$e01==2 & df$n01==8,'J',df$let200m)
df$let200m<-ifelse(df$e01==2 & df$n01==9,'J',df$let200m)
df$let200m<-ifelse(df$e01==3 & df$n01==8,'J',df$let200m)
df$let200m<-ifelse(df$e01==3 & df$n01==9,'J',df$let200m)
df$let200m<-ifelse(df$e01==4 & df$n01==8,'P',df$let200m)
df$let200m<-ifelse(df$e01==4 & df$n01==9,'P',df$let200m)
df$let200m<-ifelse(df$e01==5 & df$n01==8,'P',df$let200m)
df$let200m<-ifelse(df$e01==5 & df$n01==9,'P',df$let200m)
df$let200m<-ifelse(df$e01==6 & df$n01==8,'U',df$let200m)
df$let200m<-ifelse(df$e01==6 & df$n01==9,'U',df$let200m)
df$let200m<-ifelse(df$e01==7 & df$n01==8,'U',df$let200m)
df$let200m<-ifelse(df$e01==7 & df$n01==9,'U',df$let200m)
df$let200m<-ifelse(df$e01==8 & df$n01==8,'Z',df$let200m)
df$let200m<-ifelse(df$e01==8 & df$n01==9,'Z',df$let200m)
df$let200m<-ifelse(df$e01==9 & df$n01==8,'Z',df$let200m)
df$let200m<-ifelse(df$e01==9 & df$n01==9,'Z',df$let200m)

#concatenate variables to produce grid refs of different resolutions
df$ref10km<-paste0(df$hundref,df$e10,df$n10)
df$ref01km<-paste0(df$hundref,df$e10,df$e1,df$n10,df$n1)
df$ref500m<-paste0(df$hundref,df$e10,df$e1,df$n10,df$n1,df$let500m)
df$ref200m<-paste0(df$hundref,df$e10,df$e1,df$n10,df$n1,df$let200m)
df$ref100m<-paste0(df$hundref,df$e10,df$e1,df$e01,df$n10,df$n1,df$n01)

#add a count column for aggregate functions in future progs
df$count<-1
#remove working columns
df<-subset(df,select=c('FID','CLASS','easting','northing','ref10km','ref01km','ref500m','ref200m','ref100m','count'))

#export as RDS file for easy access by other progs
saveRDS(df,file='parcels_with_grids.rds')


