#
# including the required libraries
#

if (!require("pacman")) install.packages("pacman")
pacman::p_load(pacman,psych,readxl, rworldmap, rworldxtra,readr, plotrix) 

#
# loading the data 
#

data <- read_excel("C:/Users/brian/OneDrive/College/Software Project/August Submission/Project/Ireland_Crime_Data.xls")
latlong <- read_csv("C:/Users/brian/OneDrive/College/Software Project/August Submission/Project/latlong.csv")

#
# changing into dataframe format
#

data <- data.frame(data) 
Timepoints <- 2003:2019
ireland.ylim = c(51.373707,55.312147)
ireland.xlim = c(-11.231506,-4.387024)

#
# overview of data to get prelimnary insights of dataset
#

head(data)
cols <-  colnames(data)
summary(data)
desc_data <- describe(data)
unique.data = list(region = unique(data$REGION),grada.division = unique(data$GARDA.DIVISION), offence = unique(data$OFFENCE), type.offence = unique(data$TYPE.OF.OFFENCE))

#
# cleaning data and removing redundant column
#

data <- subset(data, select = -c(OFFENCE.CODE)) 

#
# plotting data
#

agg.type.offence <- aggregate(.~TYPE.OF.OFFENCE,data[,c(4:length(data))],sum)
agg.type.offence <- data.frame(agg.type.offence[-1], row.names = agg.type.offence$TYPE.OF.OFFENCE)

#
# piechart for Crime occured from 2003 to 2019 , Based on Type of Offence
#

pie.agg.type.offence <- data.frame(Crime.Sum = rowSums(agg.type.offence),TypeOfOffence = row.names(agg.type.offence), row.names = c(1:nrow(agg.type.offence)))
pie.agg.type.offence <- pie.agg.type.offence[order(pie.agg.type.offence$Crime.Sum),]
pie(pie.agg.type.offence$Crime.Sum, labels = pie.agg.type.offence$TypeOfOffence, radius = 1, main="Total Crimes based on Type of Offences", cex = 0.5)

#
# line plot for Crime occured from 2003 to 2019 , Based on Type of Offence
#

agg.type.offence = t(agg.type.offence)
par(xaxt="n")
matplot(x = agg.type.offence,
        type = "o",
        pch = ncol(agg.type.offence),
        xlab = "Time from 2003 to 2019",
        ylab = "No. Of Crimes occured", 
        main = "Crime occured from 2003 to 2019 , Based on Type of Offence",
        cex=0.3, cex.main = 0.8)
legend("topright", inset=0.01, legend=colnames(agg.type.offence), col=c(1:5),pch=15:19,
       bg= ("white"), horiz=F, cex=0.25)
par(xaxt="s")
axis(side=1,at=1:(length(cols)-5),labels=cols[6:length(cols)], cex.axis=0.7)

#
# pie chart for Crime occured from 2003 to 2019 , Based on Type of Region
#

agg.type.region <- aggregate(.~REGION,data[,c(1,5:length(data))],sum)
agg.type.region <- data.frame(agg.type.region[-1], row.names = agg.type.region$REGION)

pie.agg.type.region <- data.frame(Crime.Sum = rowSums(agg.type.region),REGION = row.names(agg.type.region), row.names = c(1:nrow(agg.type.region)))
pie.agg.type.region <- pie.agg.type.region[order(pie.agg.type.region$Crime.Sum),]
pie3D(pie.agg.type.region$Crime.Sum,labels = pie.agg.type.region$REGION, explode = 0.1, labelcex = 0.55, main = "Total no. of Crime Occured from 2003 to 2019 based on Region")

#
# line plot for Crime occured from 2003 to 2019 , Based on Type of Region
#

agg.type.region = t(agg.type.region)
par(xaxt="n")
matplot(x = agg.type.region,
        type = "o",
        pch = ncol(agg.type.region),
        xlab = "Time from 2003 to 2019",
        ylab = "No. Of Crimes occured", 
        main = "Crime occured from 2003 to 2019 , Based on Type of Region",
        cex=0.3, cex.main = 0.8)
legend("topright", inset=0.01, legend=colnames(agg.type.region), col=c(1:5),pch=15:19,
       bg= ("white"), horiz=F, cex=.25)
par(xaxt="s")
axis(side=1,at=1:(length(cols)-5),labels=cols[6:length(cols)], cex.axis=0.7)

#
# plot heatmap for garda divison wise and quater wise crime rate over ireland
#

agg.garda <- aggregate(.~GARDA.DIVISION,data[,c(2,5:length(data))],sum)
agg.garda <- data.frame(agg.garda[-1], row.names = agg.garda$GARDA.DIVISION)
heatmap(as.matrix(agg.garda),Colv = NA, scale="column", main = "Heatmap for Garda Divsion vs Crime Over years", cexCol = 0.5, cexRow = 0.5)

#
# region pointers on map
#

newmap <- getMap(resolution = "high")
plot(newmap, xlim = ireland.xlim, ylim = ireland.ylim, asp = 1)
points(latlong$long, latlong$lat, col="red",cex=0.6)