#install.packages('chron')
#install.packages('gtools')
library(chron)
library(gtools)
library(outliers)
library(foreign)
library(Hmisc)
library(pastecs)

# 400 microliters was in each tube. The tube volumes were

source('/Users/jaywarrick/Public/DropBox/GitHub/R-InVivoDrugTestingDevice/InVitroData_HelperFunctions.R')
source('~/.Rprofile')

# Define key words or patterns for parsing the data file.
calPatterns <- c("^[B]","^[.][0]{3}[1][%]","^[.][0]{2}[1][%]","^[.][0]{1}[1][%]","^[.][1][%]","^[1][%]","^[1][0][%]","^[1][0][0][%]")
calNames <- c("Blank", ".0001", ".001", ".01", ".1", "1", "10", "100")
calNames2 <- c("Blank", ".0001%", ".001%", ".01%", ".1%", "1%", "10%", "100%")
calConc <- c(0,0.0001,0.001,0.01,0.1,1,10,100)
calPattern <- paste('(',calNames, ')', sep=')|(')
blankPattern <- "Blank"
measPattern <- "Meas"
resPattern <- "Res"
deliveryRatePattern <- "E"

# Loop over days, readinging in the data files for tube concentration measurements.
day <- 29
data <- data.frame()
for(i in c(0:16,18:day))
{
     print(i)
     temp <- read.delim(paste('/Users/jaywarrick/Public/DropBox/GitHub/R-InVivoDrugTestingDevice/John - In Vivo - In Vitro Study/Day ', i, '.txt', sep=''))
     temp <- cbind(temp, Day=i)
     data <- rbind(data, temp)
}

# Read in capsule data
capData <- read.delim('/Users/jaywarrick/Public/DropBox/GitHub/R-InVivoDrugTestingDevice/John - In Vivo - In Vitro Study/Day 29 Reservoir Measurements - Revised.txt', fill=TRUE)
capData$Day <- 29
# capData[capData$User.ID=='Too High','RFU'] <- NA
# capData[capData$User.ID=='Too High','Date'] <- '4/21/2015'
# capData[capData$User.ID=='Too High','Time'] <- '6:00 AM'
capData <- capData[!grepl('No', capData$Sample.ID),]
capData <- capData[!capData$User.ID=='Too High',]

# Convert the Date txt to a real date.
data$TimeStamp <- as.chron(strptime(paste(data$Date, data$Time, sep=' '), format="%m/%d/%Y %I:%M %p"))
data$relTimeStamp <- data$TimeStamp-data$TimeStamp[1]
capData$TimeStamp <- as.chron(strptime(paste(capData$Date, capData$Time, sep=' '), format="%m/%d/%Y %I:%M %p"))
capData$relTimeStamp <- capData$TimeStamp-capData$TimeStamp[1]

# Make hypenation consistent
data$Sample.ID <- gsub(" - ", " ", data$Sample.ID, fixed=TRUE)
data$Sample.ID <- gsub("-", " ", data$Sample.ID, fixed=TRUE)
data$Sample.ID <- gsub(" ", "-", data$Sample.ID, fixed=TRUE)
capData$Sample.ID <- gsub(" - ", " ", capData$Sample.ID, fixed=TRUE)
capData$Sample.ID <- gsub("-", " ", capData$Sample.ID, fixed=TRUE)
capData$Sample.ID <- gsub(" ", "-", capData$Sample.ID, fixed=TRUE)

# Make % signs consistent
data$Sample.ID <- sub(" %", "%", data$Sample.ID)
capData$Sample.ID <- sub(" %", "%", capData$Sample.ID)

# Make repeat nomenclature and numbers consistent
data$Sample.ID <- sub("redo", "2", data$Sample.ID, ignore.case=TRUE)
data[!grepl("[-][[:digit:]]$", data$Sample.ID),'Sample.ID'] <- paste(data[!grepl("[-][[:digit:]]$", data$Sample.ID), 'Sample.ID'],'-1',sep='')
data$Sample.ID <- sub(" %", "%", data$Sample.ID)
data$BaseName <- sub("[-][[:digit:]]$", "", data$Sample.ID)
data$BaseName2 <- sub("Meas-", "", data$BaseName, ignore.case=T)
capData$Sample.ID <- sub("redo", "2", capData$Sample.ID, ignore.case=TRUE)
capData[!grepl("[-][[:digit:]]$", capData$Sample.ID),'Sample.ID'] <- paste(capData[!grepl("[-][[:digit:]]$", capData$Sample.ID), 'Sample.ID'],'-1',sep='')
capData$Sample.ID <- sub(" %", "%", capData$Sample.ID)
capData$BaseName <- sub("[-][[:digit:]]$", "", capData$Sample.ID)
capData$BaseName2 <- sub("Res-", "", capData$BaseName, ignore.case=T)
capData$BaseName2 <- sub("Meas-", "", capData$BaseName2, ignore.case=T)

# Create a numeric concentration column\
data$Conc <- as.numeric(sub("%", "", data$BaseName2, fixed=TRUE))
capData$Conc <- as.numeric(sub("%", "", capData$BaseName2, fixed=TRUE))

# Sort the table
data <- data[with(data, multi.mixedorder(Day, Conc, Sample.ID, relTimeStamp)), ]
capData <- capData[with(capData, multi.mixedorder(Day, Conc, Sample.ID, relTimeStamp)), ]

# Filter repeats, keeping keeping only last repeat for now
temp <- data.frame()
for(d in unique(data$Day))
{
     for(n in unique(data$BaseName))
     {
          subTable <- data[data$Day==d & data$BaseName==n,]
          if(nrow(subTable) != 0)
          {
               subTable <- subTable[nrow(subTable),]
               temp <- rbind(temp, subTable)
          }
     }
}
originalData <- data
data <- temp
# do it for cap data now
temp <- data.frame()
for(d in unique(capData$Day))
{
     for(n in unique(capData$BaseName))
     {
          subTable <- capData[capData$Day==d & capData$BaseName==n,]
          if(nrow(subTable) != 0)
          {
               subTable <- subTable[nrow(subTable),]
               temp <- rbind(temp, subTable)
          }
     }
}
originalCapData <- capData
capData <- temp

# Remove outliners in calibration data
calData2 <- getValidCalData(data)

# Specify different important subsets of the data
outliers <- data$BaseName2 %in% c('E11','E13','E14','E15','E16') | (data$BaseName2=='E1' & data$Day %in% c(2,3)) | (data$BaseName2=='E2' & data$Day==2)
blankData <- grepl(blankPattern, data$BaseName2) & !outliers
leakData <- grepl(measPattern, data$BaseName, ignore.case=T) & !grepl(deliveryRatePattern, data$BaseName2) & !outliers
deliveryData <- grepl(measPattern, data$BaseName, ignore.case=T) & grepl(deliveryRatePattern, data$BaseName2) & !outliers
capOutliers <- capData$BaseName2 %in% c('E11','E13','E14','E15','E16')
capDeliveryData <- grepl(deliveryRatePattern, capData$BaseName2) & !capOutliers


# Plot Calibration
#pdf('/Users/jaywarrick/Desktop/John - In Vivo - In Vitro Study/CalibrationPlot.pdf', width=7, height=5)
temp <- calData2
plot(x=c(), y=c(), main='Calibration', xlab='Time [days]', ylab='RFU', xlim=range(temp$relTimeStamp), ylim=range(temp$RFU + 0.1), log='y')
i <- 1
for(name in calPatterns)
{
     temp2 <- temp[grepl(name,temp$Sample.ID),]
     print(temp2)
     points(temp2$relTimeStamp, temp2$RFU + 0.1, col=i)
     lines(temp2$relTimeStamp, temp2$RFU + 0.1, col=i)
     i <- i + 1
}
legend('topright', legend=unique(calNames), col=1:length(unique(calNames)), lty=1, cex=0.7, bg=rgb(1,1,1))
#dev.off()


# Plot Leak Data that ends with less that 200 units of intensity
#pdf('/Users/jaywarrick/Desktop/John - In Vivo - In Vitro Study/DeliveryRatePlot.pdf', width=7, height=5)
temp <- data[leakData,]
filter <- unique(temp[temp$RFU < 2000 & temp$relTimeStamp > 25,'BaseName2'])
plot(x=c(), y=c(), main='Leak Devices', xlab='Time [days]', ylab='RFU', xlim=range(temp$relTimeStamp), ylim=c(0.1,max(temp$RFU)), log='y')
i <- 1
for(name in filter)
{
     points(temp[temp$BaseName2==name,]$relTimeStamp, temp[temp$BaseName2==name,]$RFU, col=i)
     lines(temp[temp$BaseName2==name,]$relTimeStamp, temp[temp$BaseName2==name,]$RFU, col=i)
     i <- i + 1
}
legend('topleft', legend=filter, col=1:length(unique(temp$BaseName2)), lty=1, cex=0.7, bg=rgb(1,1,1))
#dev.off()

# Determine which degree polynomial fit to use.
# use getCalForConc to calculate the standard deviation for each conc (or log10(conc)).
w <- data.frame(conc=c(), w=c())
for(conc in getValidConc(data))
{
     conc
     w <- rbind(w, data.frame(conc=conc, weight=1/((mad(log10(getCalForConc(conc,data)$RFU))^2))))
}
w$weight <- w$weight/sum(w$weight)
x <- log10(calData2$Conc)
y <- log10(calData2$RFU)
ws <- 0*x
for(conc in getValidConc(data))
{
     ws[calData2$Conc == conc] <- w[w$conc==conc,'weight']
}
ws
lm.Cal1 <- lm(y ~ poly(x, 1, raw=TRUE), weights=ws, na.action=na.omit)
lm.Cal2 <- lm(y ~ poly(x, 2, raw=TRUE), weights=ws, na.action=na.omit)
lm.Cal3 <- lm(y ~ poly(x, 3, raw=TRUE), weights=ws, na.action=na.omit)
lm.Cal4 <- lm(y ~ poly(x, 4, raw=TRUE), weights=ws, na.action=na.omit)
summary(lm.Cal1)
summary(lm.Cal2)
summary(lm.Cal3)
summary(lm.Cal4)

# Check which degree of polynomial is justified (i.e., continues to improve fit significantly)
anova(lm.Cal1,lm.Cal2,lm.Cal3,lm.Cal4,test='F')

# Results suggest a 2nd degree polynomial is justified so getCurves(data, isLog=T) will fit with 2nd degree polynomials
plot(x,y, xlab="log(% Conc.)", ylab="log(RFU)")
lines(-3:1, predict(lm.Cal2, newdata=data.frame(x=-3:1)))

# Fit a 2nd order polynomial to each day of calibration data and store the models in a list
models <- getModels(calData2, w)

# Use the models for each day to adjust the measured RFU
data$AdjConc2 <- NA
for(day in data$Day)
{
     tempLogConc <- seq(-3,1,length.out=100)
     tempLogRFU <- predict(models[[as.character(day)]], newdata=data.frame(x=tempLogConc))
     flags <- data$Day==day & data$RFU>0 & !is.na(data$RFU)
     oldLogRFU <- log10(data[flags,]$RFU)
     newConc <- 10^(approx(x=tempLogRFU, y=tempLogConc, xout=oldLogRFU, yleft=NA)$y)
     data[flags,]$AdjConc2 <- newConc
}

# Use the models for each day to adjust the measured RFU FOR CAPDATA
capData$AdjConc2 <- NA
for(day in capData$Day)
{
     tempLogConc <- seq(-3,2,length.out=100)
     tempLogRFU <- predict(models[[as.character(day)]], newdata=data.frame(x=tempLogConc))
     flags <- capData$Day==day & capData$RFU>0 & !is.na(capData$RFU)
     oldLogRFU <- log10(capData[flags,]$RFU)
     newConc <- 10^(approx(x=tempLogRFU, y=tempLogConc, xout=oldLogRFU, yleft=NA)$y)
     capData[flags,]$AdjConc2 <- newConc
}

# Use the models for one day to adjust the measured RFU
data$AdjConc <- NA
for(day in data$Day)
{
     tempLogConc <- seq(-3,1,length.out=100)
     tempLogRFU <- predict(lm.Cal2, newdata=data.frame(x=tempLogConc))
     flags <- data$Day==day & data$RFU>0 & !is.na(data$RFU)
     oldLogRFU <- log10(data[flags,]$RFU)
     newConc <- 10^(approx(x=tempLogRFU, y=tempLogConc, xout=oldLogRFU, yleft=NA)$y)
     data[flags,]$AdjConc <- newConc
}

# Plot RAW Delivery Rate Data
#pdf('/Users/jaywarrick/Desktop/John - In Vivo - In Vitro Study/DeliveryRatePlot.pdf', width=7, height=5)
temp <- data[deliveryData,]
plot(x=c(), y=c(), main='Raw Delivery Intensities', xlab='Time [days]', ylab='RFU [au]', xlim=range(temp$relTimeStamp), ylim=range(temp$RFU))
i <- 1
for(name in unique(temp$BaseName2))
{
     points(temp[temp$BaseName2==name,]$relTimeStamp, temp[temp$BaseName2==name,]$RFU, col=i)
     lines(temp[temp$BaseName2==name,]$relTimeStamp, temp[temp$BaseName2==name,]$RFU, col=i)
     i <- i + 1
}
legend('topleft', legend=unique(temp$BaseName2), col=1:length(unique(temp$BaseName2)), lty=1, cex=0.7, bg=rgb(1,1,1))
#dev.off()


# Plot Adjusted Delivery Rate Data (one day adjustment)
#pdf('/Users/jaywarrick/Desktop/John - In Vivo - In Vitro Study/DeliveryRatePlot.pdf', width=7, height=5)
temp <- data[deliveryData,]
plot(x=c(), y=c(), main='Calibrated Delivery Concentrations (one cal curve)', xlab='Time [days]', ylab='Conc [%]', xlim=range(temp$relTimeStamp, na.rm=T), ylim=range(temp$AdjConc, na.rm=T))
i <- 1
for(name in unique(temp$BaseName2))
{
     points(temp[temp$BaseName2==name,]$relTimeStamp, temp[temp$BaseName2==name,]$AdjConc, col=i)
     lines(temp[temp$BaseName2==name,]$relTimeStamp, temp[temp$BaseName2==name,]$AdjConc, col=i)
     i <- i + 1
}
legend('topleft', legend=unique(temp$BaseName2), col=1:length(unique(temp$BaseName2)), lty=1, cex=0.7, bg=rgb(1,1,1))
#dev.off()

# Plot Adjusted Delivery Rate Data (per day adjustment)
#pdf('/Users/jaywarrick/Desktop/John - In Vivo - In Vitro Study/DeliveryRatePlot.pdf', width=7, height=5)
temp <- data[deliveryData,]
plot(x=c(), y=c(), main='Calibrated Delivery Concentrations (cal curve per day)', xlab='Time [days]', ylab='Conc [%]', xlim=range(temp$relTimeStamp, na.rm=T), ylim=range(temp$AdjConc2, na.rm=T))
i <- 1
for(name in unique(temp$BaseName2))
{
     points(temp[temp$BaseName2==name,]$relTimeStamp, temp[temp$BaseName2==name,]$AdjConc2, col=i)
     lines(temp[temp$BaseName2==name,]$relTimeStamp, temp[temp$BaseName2==name,]$AdjConc2, col=i)
     i <- i + 1
}
legend('topleft', legend=unique(temp$BaseName2), col=1:length(unique(temp$BaseName2)), lty=1, cex=0.7, bg=rgb(1,1,1))
#dev.off()

temp <- data[deliveryData,]
i <- 1
temp$AvgTime <- -1
for(day in temp$Day)
{
     temp[temp$Day == day,]$AvgTime <- mean(temp[temp$Day==day,]$relTimeStamp)
}
tempResults <- data.frame(x=c(), y=c())
for(day in unique(temp$Day))
{
     temp2 <- subset(temp, Day==day)
     x <- temp2$AvgTime[1]
     y <- mean(temp2$AdjConc)
     yerr <- sd(temp2$AdjConc)
     tempResults <- rbind(tempResults, data.frame(day=day, x=x, y=y, yerr=yerr))
}

# Define various equations to fit the trajectory data with
# Define y = e^(alpha*(x-tau))
fun <- function(par, tempResults)
{
     # par = c(capsuleCoef=D*A/(L*V1), tubeCoef=D*A/L)
     out <- getModel(times=tempResults$x*(24*3600), V1=par[1])#, tubeCoef=par[2])
     #return(sum(tempResults$w*((tempResults$y-out$C2)^2)))
     return(sum((tempResults$y-out$C2)^2))
}

# Self-starter function for determining initial conditions.
ssfctA <- function(data)
{
     out <- getModel()
     return(c(V1=out$VALD$V1))#, tubeCoef=out$params$tubeCoef))
}

# fit <- optim(c(V1=1.87e-9, ssfctA(0)[2]), fun, tempResults=tempResults)
fit <- optim(c(V1=1.87e-9), fun, tempResults=tempResults)
normalizationFactor <- 100*fit$par['V1']/((400e-6)/1000)

# Plot Adjusted Delivery Rate Data
pdf('/Users/jaywarrick/Public/DropBox/GitHub/R-InVivoDrugTestingDevice/Figures/DeliveryRatePlot.pdf', width=7, height=5)

plot(x=c(), y=c(), main='', xlab='Time [days]', ylab='Normalized Concentration', xlim=c(min(temp$relTimeStamp, na.rm=T),30), ylim=range(0,1.05))
lines(tempResults$x, tempResults$y/normalizationFactor, type='l')
# points(tempResults$x, tempResults$y, pch=20, cex=0.3, col='black')
errbar(tempResults$x, tempResults$y/normalizationFactor, yplus=tempResults$y/normalizationFactor+tempResults$yerr/normalizationFactor, yminus=tempResults$y/normalizationFactor-tempResults$yerr/normalizationFactor, add=T, pch=20, col='black', cex=0.8)
#legend('topleft', legend=unique(temp$BaseName2), col=1:length(unique(temp$BaseName2)), lty=1, cex=0.7, bg=rgb(1,1,1))
#dev.off()

# Assign wieghts to all delivery data.
tempResults$w <- 1/(tempResults$yerr)^2
tempResults$w <- tempResults$w/sum(tempResults$w)

print('Final Capsule Concentration Data for Delivery Rate Capsules')
stat.desc(capData[capDeliveryData,]$AdjConc)
