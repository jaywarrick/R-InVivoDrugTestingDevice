rm(list=ls())
library(data.table)
library(Hmisc)

myInside <- data.frame(day=c(1,3,10),IL8=c(9892,14691.7,9844.1),MCP1=c(992.5,192,442.2),VEGF=c(18020.3,20773.8,16145.3))
myOutside <- data.frame(day=c(1,3,10),IL8=c(84.3,707,953.5),MCP1=c(6.75,8.5,42),VEGF=c(280.25,2169.375,3175.5))
myInsideSDs <- data.frame(day=c(1,3,10),IL8=c(4080.7,4008.7,8078.4),MCP1=c(1555.6,220.9,422.2),VEGF=c(698.2,1177.9,8508.7))
myInsideSDs <- data.frame(day=c(1,3,10),IL8=c(42.0,1174.1,124.3),MCP1=c(0.5,3,0),VEGF=c(364.2,3037.7,329.9))

plot(myInside$day,myInside$IL8, xlim=c(1,10), ylim=c(1,21000), pch=20, col='green', bg='green', type='o', ylab='Median Bead Intensity [au]', xlab='Days After Implantation [days]')
lines(myInside$day,myInside$MCP1, pch=20, col='red', bg='red', type='o')
lines(myInside$day,myInside$VEGF, pch=20, col='blue', bg='blue', type='o')
legend('topright', legend=c('IL8    ','MCP1    ','VEGF    '), col=c('green','red','blue'), lty=c(1,1,1), pch=c(20,20,20))

plot(myOutside$day,myOutside$IL8, xlim=c(1,10), ylim=c(1,21000), pch=20, col='green', bg='green', type='o', ylab='Median Bead Intensity [au]', xlab='Days After Implantation [days]')
lines(myOutside$day,myOutside$MCP1, pch=20, col='red', bg='red', type='o')
lines(myOutside$day,myOutside$VEGF, pch=20, col='blue', bg='blue', type='o')
legend('topright', legend=c('IL8    ','MCP1    ','VEGF    '), col=c('green','red','blue'), lty=c(1,1,1), pch=c(20,20,20))

data <- read.csv(file='/Users/jaywarrick/Documents/MMB/Projects/Sampling Device/20151110 - Small Volume Standards.csv', stringsAsFactors=FALSE)
data$Concentration <- (1/3)^(data$Dilution-1)
results <- subset(data, Dilution > 0)
results <- data.table(results)
results <- results[order(Condition,Dilution)]
aCols <- names(results)[grep("Analyte",names(results))]
resultsMean <- results[,lapply(.SD, mean), by=.(Concentration,Condition), .SDcols=aCols]
resultsSD <- results[,lapply(.SD, sd), by=.(Concentration,Condition), .SDcols=aCols]


plotData <- function(data, dataErr, analyte, conc=1)
{
     col <- rgb(0,0,0,0)
     bg1 <- rgb(0,0,1,1)
     bg2 <- rgb(1,0,0,1)

     #analyte <- paste0('Analyte.', analyteNum)
     x <- data$Concentration[data$Condition == 'Control']*conc
     y <- data[,get(analyte)][data$Condition == 'Control']
     yerr <- dataErr[,get(analyte)][dataErr$Condition == 'Control']
     plot(x, y, main='', pch=21, cex=0.75, col=bg1, bg=col, xlab='Conc [pg/mL]', ylab='MFI [au]', log='xy', ylim=c(30,max(y)))
     lines(x, y, col='blue', lty=2)
     errbar(x, y, y+yerr, y-yerr, type='n', add=T)

     x <- data$Concentration[data$Condition == 'Expt']*conc
     y <- data[,get(analyte)][data$Condition == 'Expt']
     yerr <- dataErr[,get(analyte)][dataErr$Condition == 'Expt']
     points(x, y, pch=21, cex=0.75, col=bg2, bg=col)
     lines(x, y, col='red', lty=2)
     errbar(x, y, y+yerr, y-yerr, type='n', add=T)

     legend('topleft', legend=c('Large Vol Sample','Small Vol Sample'), pch=21, cex=0.75, col=c(bg1, bg2), pt.bg=col)
}

# Apply concentration of standards
standards <- list(Analyte.13=list(name='IL-6', conc=4880), Analyte.18=list(name='IL-8',conc=4310), Analyte.26=list(name='VEGF',conc=1970), Analyte.57=list(name='VEGF-R3',conc=36300), Analyte.73=list(name='VEGF-D',conc=17100), Analyte.14=list(name='MMP-9',conc=37370), Analyte.20=list(name='CSCL12',conc=3200), Analyte.38=list(name='VEGF C',conc=26000), Analyte.66=list(name='HGF',conc=9320))

for(analyte in grep('Analyte', names(results)))
{
     analyteName <- names(data)[analyte]

     #pdf(file=file.path('/Users/jaywarrick/Documents/MMB/Projects/Sampling Device/20151110 Plots',paste0(analyteName,'.pdf')), width=6, height=4)
     plotData(resultsMean, resultsSD, analyte=analyteName, conc=standards[[analyteName]]$conc)
     title(main=standards[[analyteName]]$name)
     #dev.off()
}

# Get the ratio
resultsRatios <- resultsMean[,.SD[1]/.SD[2],by=.(Concentration), .SDcols=aCols]
resultsRatios <- data.table(analyte=aCols, meanRatio=as.vector(unlist(resultsRatios[,lapply(.SD,mean), .SDcols=aCols])), sdRatio=as.vector(unlist(resultsRatios[,lapply(.SD,sd), .SDcols=aCols])))
resultsRatios
