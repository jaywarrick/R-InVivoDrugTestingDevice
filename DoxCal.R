library(foreign)
library(data.table)
fileTable <- read.arff('/Volumes/MMB Sackmann/Jay/JEX Databases/In Vivo/temp/JEXData0000004643.arff')
data <- data.table(reorganizeTable(read.arff(fileTable[1,4]), convertToNumeric=FALSE))
data <- data[,c(1,2,7,8),with=F]
data$Location <- as.numeric(as.character(data$Location))
concentrations <- setNames(c(0,1e-8,1e-7,1e-6,1e-5,1e-4,1e-3,1e-2), unique(data$Location))
data$Conc <- concentrations[as.character(data$Location)]
exposureTimes <- setNames(c(20,350,300,10,4000,100,1000), unique(data$Channel))
data$Exposure <- exposureTimes[as.character(data$Channel)]
data[,AdjMean:=mean - mean[Conc==0],by=Channel]
data$AdjMean <- data$AdjMean/data$Exposure
data[,Crossover:=AdjMean/(AdjMean[Channel=='DoxLong']),by=Conc]
eps <- 1e-12
plot(c(), c(), xlab='Dox Concentration [fraction of stock]', ylab='Intensity [au]', xlim=range(data$Conc+eps), ylim=range(data[data$AdjMean > 0,]$AdjMean), log='xy')
i <- 1
for(c in unique(data$Channel))
{
     temp <- subset(data, Channel==c)
     lines(temp$Conc+eps, temp$AdjMean, col=i)
     points(temp$Conc+eps, temp$AdjMean, col=i, pch=i)
     i <- i + 1
     print(temp)
}
legend('topleft', legend=unique(data$Channel), lty=1, pch=1:8, col=1:8)

duh <- subset(data, Conc==1e-3)
duh
duh <- subset(data, Channel=='740 X 809 M')
