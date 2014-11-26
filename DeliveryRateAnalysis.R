data <- data.frame(device=rep(1:4,3), day=rep(c(0,7,27),each=4), reservoir=c(1,1,1,1,1,1,1,1,1,1,1,1), tube=c(2,2,2,2,2,2,2,2,2,2,2,2))

library(foreign)
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
duh$PTRatio <- duh$PDMS/duh$Tube


# Cube Volumes
cubeVol <- mean(c(6.31*6.25*4.92, 6.39*6.26*4.96, 6.33*6.30*4.90, 6.31*6.35*4.94, 6.28*6.37*4.93))

# Tube Volumes
tubeVols <- c(4.80, 4.80, 5.50, 5.89)

# Tube to Cube Vol Ratios
volRatio <- (tubeVols + cubeVol)/tubeVols

# Governing equatio: par=c(alpha, tau) [the fitted parameters], volRatio = (Vt+Vp)/Vt
decay <- function(par=c(1000, 60*60*24*7), volRatio=40)

