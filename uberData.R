#####
## Uber Data-
######
setwd("/Users/mutononyamai/Dropbox/Uber paper/Travels_Waiyaki Way")
library(raster)
library(tmap)
library(tmaptools)
library(rgdal)
library(base)
library(haven)
library(epiDisplay)
library(dplyr)
library(plyr)
## January 01/52 data for 2016-2018 during the Early Morning(EM) Hours (12am-7am)
uber<- read.csv(file.choose())
names(uber)
head(uber, 2)

## Separate the 2016 and 2017 data
uberJan01_2016EM<- uber[,1:8]
uberJan01_2017EM<- uber[,c(1:4,9:12)]
uberJan01_2018EM<- read.csv(file.choose())
names(uberJan01_2016EM) <- c("originid","originname","destmovtid","destmovtname","datetime","meantravel","lowerci","upperci") 
names(uberJan01_2017EM) <- c("originid","originname","destmovtid","destmovtname","datetime","meantravel","lowerci","upperci")
names(uberJan01_2018EM) <- c("originid","originname","destmovtid","destmovtname","datetime","meantravel","lowerci","upperci")
uberJan01_2016EM$datetime<- c("2016") 
uberJan01_2017EM$datetime<- c("2017")
uberJan01_2018EM$datetime<- c("2018")
uberJan01_2017EM
## Combine the data for the three years
uberJan01_EMCombined <- rbind(uberJan01_2016EM, uberJan01_2017EM, uberJan01_2018EM)
uberJan01_EMCombined                  
##Add the shapefile
Nairobi<- read_shape(file.choose(), as.sf=TRUE, stringsAsFactors=FALSE)

load(file.choose())


#uberJan01_EMCombined <- append_data(Nairobi, uberJan01_EMCombined1, key.shp="MOVEMENT_I", key.data="destmovtid")
tm1<- qtm(uberJan01_2016EM, fill="meantravel", text.size=0.5, fill.title="Mean Travel- Sec", text.root=5, bg.color="white", borders="white")+ tm_layout(title="January 2016", title.size=0.9, legend.show=FALSE)
tm2<- qtm(uberJan01_2017EM, fill="meantravel", text.size=0.5, fill.title="Mean Travel- Sec", text.root=5, bg.color="white", borders="white")+ tm_layout(title="January 2017", title.size=0.9, legend.show=FALSE)
tm3<- qtm(uberJan01_2018EM, fill="meantravel", text.size=0.5, fill.title="Mean Travel- Sec", text.root=5, bg.color="white", borders="white")+ tm_layout(title="January 2018", title.size=0.9, legend.show=FALSE)


                  
 ##Merge the shapefile and the datafile
uberJan01_2016EMSF<- append_data(Nairobi, uberJan01_2016EM, key.shp="MOVEMENT_I", key.data="destmovtid")
uberJan01_2017EMSF<- append_data(Nairobi, uberJan01_2017EM, key.shp="MOVEMENT_I", key.data="destmovtid")
uberJan01_2018EMSF<- append_data(Nairobi, uberJan01_2018EM, key.shp="MOVEMENT_I", key.data="destmovtid")

                  
tm1<- qtm(uberJan01_2016EMSF, fill="meantravel", text.size=0.5, fill.title="Mean Travel- Sec", text.root=5, bg.color="white", borders="white")+ tm_layout(title="February 2016", title.size=0.9, legend.show=FALSE)

tm2<- qtm(uberJan01_2017EMSF, fill="meantravel", text.size=0.5, fill.title="Mean Travel- Sec", text.root=5, bg.color="white", borders="white")+ tm_layout(title="February 2017", title.size=0.9, legend.show=FALSE)

tm3<- qtm(uberJan01_2018EMSF, fill="meantravel", text.size=0.5, fill.title="Mean Travel- Sec", text.root=5, bg.color="white", borders="white")+ tm_layout(title="February 2018", title.size=0.9, legend.outside=TRUE)

current.mode<- tmap_mode("plot")
tmap_arrange(tm1,tm2, tm3, asp=NA)
tmap_mode(current.mode)
head(uberJan01_EMCombined)

## Cumulative data for mean time used for travelling
cumulativeJan01<- factor(cut(uberJan01_2016EM$meantravel, breaks=nclass.Sturges(uberJan01_2016EM$meantravel)))

cumulativeOut<- as.data.frame(table(cumulativeJan01))
cumulativeOut1<- transform(cumulativeOut, cumFreq=cumsum(Freq), relative=prop.table(Freq))
cumulativeOut1
x<-cbind(Freq=table(uberJan01_2016EM$meantravel),Cumul=cumsum(table(uberJan01_2016EM$meantravel)),
                                                               relative=prop.table(uberJan01_2016EM$meantravel))
x
tab1(cumulativeOut1, sort.group = "decreasing")
#   works best
try<- graph.freq(uberJan01_2016EM$meantravel, col=c("yellow", "red"), frequency = 1, las=2, xlab = " Seconds",
                ylab = "Frequency", main="Time used for travelling from Waiyaki way January 2016 (Early Morning)" )
try<- graph.freq(uberJan01_2017EM$meantravel, col=c("yellow", "red"), frequency = 1, las=2, xlab = " Seconds",
                 ylab = "Frequency", main="Time used for travelling from Waiyaki way January 2017 (Early Morning)" )
try<- graph.freq(uberJan01_2018EM$meantravel, col=c("yellow", "red"), frequency = 1, las=2, xlab = " Seconds",
                 ylab = "Frequency", main="Time used for travelling from Waiyaki way January 2018 (Early Morning)" )
barplot(c(uberJan01_2016EM$meantravel, uberJan01_2017EM$meantravel, uberJan01_2018EM$meantravel), col=c("red", "yellow", "darkblue"), las=2)
print(summary(try), row.names=FALSE)

uberJan01_2016EM
counts<- table(uberJan01_EMCombined$meantravel)
barplot(counts, main= "Mean time travels for Jan 2016-2018", xlab="Seconds", col=c("darkblue", "red", "yellow"),
        legend=rownames(counts), beside=TRUE)
xz<- data.frame(uberJan01_EMCombined$datetime)
by1<- c(uberJan01_EMCombined=="2016")
by2<- c(uberJan01_EMCombined=="2017")
by3<- c(uberJan01_EMCombined=="2018")
aggregate(x=xz, by=list(by1, by2, by3), FUN="mean")

transform(uberJan01_2016EM, Q=cut(uberJan01_2016EM$meantravel,
                                  breaks=quantile(uberJan01_2016EM$meantravel),
                                  include.lowest=TRUE))
transform(uberJan01_2017EM, Q=cut(uberJan01_2017EM$meantravel,
                                  breaks=quantile(uberJan01_2017EM$meantravel),
                                  include.lowest=TRUE))
countz<- table(uberJan01_2016EM$meantravel,uberJan01_2017EM$meantravel)
barplot(countz, main="Test", xlab="Number", col=c("darkblue", "red"),
        legend=rownames(countz))

##Trying to create quanitles using the meantravel column
set.seed(1)
counts<- data.frame(uberJan01_2016EM$meantravel = rnorm(10) )
ha<-transform(uberJan01_2016EM$meantravel, quint=ave(uberJan01_2016EM$meantravel, FUN = function(x){
  quintiles<- quantile(x, seq(0,1,.2))
  cuts<- cut(x, quintiles, include.lowest=TRUE)
  quintVal<- quintiles[match(cuts, levels(cuts))+1]
  return(quintVal)
}))



