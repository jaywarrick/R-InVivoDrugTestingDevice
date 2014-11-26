rm(list=ls())


library(foreign)
source('~/.Rprofile')
fileTable <- read.arff('/Users/jaywarrick/Documents/JEX/In Vivo Drug Testing Device/temp/JEXData0000000007.arff')
data <- read.arff(fileTable$Value[1])
data <- reorganizeTable(data)
data[data$Day==1,'Day'] <- 7
data[data$Day==2,'Day'] <- 27
data$Device <- data$Device + 1
data[data$Location==0,'Location'] <- 'PDMS'
data[data$Location==1,'Location'] <- 'Tube'
duh <- reorganizeTable(data, baseName=NA, convertToNumeric=TRUE, nameCol='Location', valueCol='mean')
duh <- duh[sort(duh$Day, index.return=TRUE)$ix,]


# Subtract background
duh$PDMS <- duh$PDMS-200
duh$Tube <- duh$Tube-200

#Calculate Ratio
duh$PTRatio <- duh$PDMS/duh$Tube


# Cube Volumes
cubeVol <- mean(c(6.31*6.25*4.92, 6.39*6.26*4.96, 6.33*6.30*4.90, 6.31*6.35*4.94, 6.28*6.37*4.93))

# Tube Volumes
tubeVols <- c(4.80, 4.80, 5.50, 5.89)*pi*0.3333333^2

# Total Vol to Tube Vol Ratios and add them to dataset
volRatios <- (tubeVols + cubeVol)/tubeVols
for(i in seq(volRatios))
{
     duh$volRatio[duh$Device==i] <- volRatios[i]
}


# Governing equation
decay <- function(alpha=100, tau=7, t=1:10, volRatio=40)
{
     return(alpha*( 1-exp(-t/tau) )/((volRatio - 1)*exp(-t/tau) + 1 ))
}

# error function
sseDecay <- function(alpha=100, tau=60, data)
{
     t <- data$Day
     predictedRatios <- decay(alpha=alpha, tau=tau, t=data$Day, volRatio=data$volRatio)
     print(predictedRatios)
     actualRatios <- data$PDMS/data$Tube
     print(actualRatios)
     print((predictedRatios-actualRatios)^2)
     return(sum((predictedRatios-actualRatios)^2))
}

# Get Delivery Rate Fit
getBestFit <- function(data)
{
     guess <- c(alpha=80, tau=10)
     alphaLimits <- c(10, 1000)
     tauLimits <- c(1, 100)
     bestFit <- optim(par=guess,
                      function(par, data){sseDecay(alpha=par['alpha'], tau=par['tau'], data=data)},
                      method='L-BFGS-B',
                      lower=c(min(alphaLimits), min(tauLimits)),
                      upper=c(max(alphaLimits), max(tauLimits)),
                      control=list(trace=0),
                      data=data)
     return(list(par=c(alpha=as.numeric(bestFit$par['alpha']), tau=as.numeric(bestFit$par['tau'])), fit=bestFit))
}

#duh <- subset(duh, Device!=4)
results <- getBestFit(duh)
plot(duh$Day,duh$PTRatio, xlab='Time [days]', ylab='Fluorescence Ratio [au]')
predicted <- decay(alpha=results$par['alpha'], tau=results$par['tau'], t=1:27, volRatio=mean(duh$volRatio))
lines(1:27, predicted)


plot(duh$Day,duh$PTRatio, xlab='Time [days]', ylab='Fluorescence Ratio [au]')
predicted <- decay(alpha=75, tau=10, t=1:27, volRatio=mean(duh$volRatio))
lines(1:27, predicted)

# results
#
#
# alpha=100
# tau=32
# sseDecay(alpha=alpha, tau=tau, duh)
# lines(1:27, decay(alpha=alpha, tau=tau, t=1:27, volRatio=40))
