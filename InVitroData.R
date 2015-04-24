#install.packages('chron')
#install.packages('gtools')
library(chron)
library(gtools)
library(outliers)

calPatterns <- c("^[B]","^[.][0]{3}[1][%]","^[.][0]{2}[1][%]","^[.][0]{1}[1][%]","^[.][1][%]","^[1][%]","^[1][0][%]","^[1][0][0][%]")
calNames <- c("Blank", ".0001", ".001", ".01", ".1", "1", "10", "100")
calNames2 <- c("Blank", ".0001%", ".001%", ".01%", ".1%", "1%", "10%", "100%")
calConc <- c(0,0.0001,0.001,0.01,0.1,1,10,100)
calPattern <- paste('(',calNames, ')', sep=')|(')
blankPattern <- "Blank"
measPattern <- "Meas"
deliveryRatePattern <- "E"

day <- 29
data <- data.frame()
for(i in c(0:16,18:day))
{
     temp <- read.delim(paste('/Users/jaywarrick/Desktop/John - In Vivo - In Vitro Study/Day ', i, '.txt', sep=''))
     temp <- cbind(temp, Day=i)
     data <- rbind(data, temp)
}

# Convert the Date txt to a real date.
data$TimeStamp <- as.chron(strptime(paste(data$Date, data$Time, sep=' '), format="%m/%d/%Y %I:%M %p"))
data$relTimeStamp <- data$TimeStamp-data$TimeStamp[1]

# Make hypenation consistent
data$Sample.ID <- gsub(" - ", " ", data$Sample.ID, fixed=TRUE)
data$Sample.ID <- gsub("-", " ", data$Sample.ID, fixed=TRUE)
data$Sample.ID <- gsub(" ", "-", data$Sample.ID, fixed=TRUE)

print(data)

# Make % signs consistent
data$Sample.ID <- sub(" %", "%", data$Sample.ID)

# Make repeat nomenclature and numbers consistent
data$Sample.ID <- sub("redo", "2", data$Sample.ID, ignore.case=TRUE)
data[!grepl("[-][[:digit:]]$", data$Sample.ID),'Sample.ID'] <- paste(data[!grepl("[-][[:digit:]]$", data$Sample.ID), 'Sample.ID'],'-1',sep='')
data$Sample.ID <- sub(" %", "%", data$Sample.ID)
data$BaseName <- sub("[-][[:digit:]]$", "", data$Sample.ID)
data$BaseName2 <- sub("Meas-", "", data$BaseName, fixed=TRUE)

# Sort the table
data <- data[with(data, multi.mixedorder(Day, Sample.ID, relTimeStamp)), ]

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

# Specify different important subsets of the data
calData <- data$BaseName2 %in% calNames2
blankData <- grepl(blankPattern, data$Sample.ID)
leakData <- grepl(measPattern, data$Sample.ID) & !grepl(deliveryRatePattern, data$Sample.ID)
deliveryData <- grepl(measPattern, data$Sample.ID) & grepl(deliveryRatePattern, data$Sample.ID)

# Create a numeric concentration column
#data$Conc <- as.numeric(sub("%", "", sub("Blank", "0", data$BaseName2), fixed=TRUE))
data$Conc <- as.numeric(sub("%", "", data$BaseName2, fixed=TRUE))

# Plot Delivery Rate Data
#pdf('/Users/jaywarrick/Desktop/John - In Vivo - In Vitro Study/DeliveryRatePlot.pdf', width=7, height=5)
temp <- data[deliveryData,]
plot(x=c(), y=c(), main='Delivery Rate Devices', xlab='Time [days]', ylab='RFU', xlim=range(temp$relTimeStamp), ylim=range(temp$RFU))
i <- 1
for(name in unique(temp$BaseName2))
{
     points(temp[temp$BaseName2==name,]$relTimeStamp, temp[temp$BaseName2==name,]$RFU, col=i)
     lines(temp[temp$BaseName2==name,]$relTimeStamp, temp[temp$BaseName2==name,]$RFU, col=i)
     i <- i + 1
}
legend('topleft', legend=unique(temp$BaseName2), col=1:length(unique(temp$BaseName2)), lty=1, cex=0.7, bg=rgb(1,1,1))
#dev.off()

# Plot Calibration
#pdf('/Users/jaywarrick/Desktop/John - In Vivo - In Vitro Study/CalibrationPlot.pdf', width=7, height=5)
temp <- data[calData,]
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


# Plot Delivery Rate Data
#pdf('/Users/jaywarrick/Desktop/John - In Vivo - In Vitro Study/DeliveryRatePlot.pdf', width=7, height=5)
temp <- data[leakData,]
filter <- unique(temp[temp$RFU < 2000 & temp$relTimeStamp > 25,'BaseName2'])
plot(x=c(), y=c(), main='Leak Devices', xlab='Time [days]', ylab='RFU', xlim=range(temp$relTimeStamp), ylim=range(temp$RFU))
i <- 1
for(name in filter)
{
     points(temp[temp$BaseName2==name,]$relTimeStamp, temp[temp$BaseName2==name,]$RFU, col=i)
     lines(temp[temp$BaseName2==name,]$relTimeStamp, temp[temp$BaseName2==name,]$RFU, col=i)
     i <- i + 1
}
legend('topleft', legend=filter, col=1:length(unique(temp$BaseName2)), lty=1, cex=0.7, bg=rgb(1,1,1))
#dev.off()

#pdf('/Users/jaywarrick/Desktop/John - In Vivo - In Vitro Study/DeliveryRatePlot.pdf', width=7, height=5)
temp <- data[leakData,]
filter <- unique(temp[temp$RFU > -1 & temp$relTimeStamp > 25 & grepl('C', temp$BaseName2),'BaseName2'])
plot(x=c(), y=c(), main='Leak Devices', xlab='Time [days]', ylab='RFU', xlim=range(temp$relTimeStamp), ylim=range(temp$RFU))
i <- 1
for(name in filter)
{
     points(temp[temp$BaseName2==name,]$relTimeStamp, temp[temp$BaseName2==name,]$RFU, col=i)
     lines(temp[temp$BaseName2==name,]$relTimeStamp, temp[temp$BaseName2==name,]$RFU, col=i)
     i <- i + 1
}
legend('topleft', legend=filter, col=1:length(unique(temp$BaseName2)), lty=1, cex=0.7, bg=rgb(1,1,1))
#dev.off()

getMe <- function(name)
{
     duh <- data[data$BaseName2 == name,]
     plot(duh$relTimeStamp, duh$RFU, type='l')
}

temp <- data.frame(x=data$Conc, y=data$RFU)
temp <- temp[calData & temp$x>0 & temp$y>0 & !is.na(temp$x) & !is.na(temp$y),]
dates <- unique(temp$Day)[order(unique(temp$Day))]
temp$x
x <- log(temp$x)
y <- log(temp$y)
plot(x,y)
lm.Cal <- lm(y ~ x, na.action=na.omit)
abline(lm.Cal, untf=FALSE)
fit <- coef(summary(lm.Cal))
curve(fit[1,1] + x*fit[2,1], from=min(x), to=max(x), add=TRUE)
min(x)
x
