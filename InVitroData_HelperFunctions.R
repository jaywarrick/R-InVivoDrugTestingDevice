getMe <- function(name)
{
     duh <- data[data$BaseName2 == name,]
     plot(duh$relTimeStamp, duh$RFU, type='l')
}

getCalForDay <- function(day, data)
{
     temp <- subset(data, Conc %in% getValidConc(data) & Day==day & RFU > 0)
     if(day == 9)
     {
          # Remove errant calibration point
          temp <- subset(temp, Conc != 0.01)
     }
     return(temp)
}

getCalForConc <- function(conc, data)
{
     temp <- subset(data, Conc==conc)
     if(conc == 0.01)
     {
          # Remove errant calibration point
          temp <- subset(temp, Day != 9)
     }
     else if(conc == 0.001)
     {
          # Remove errant calibration point
          temp <- subset(temp, Day != 16)
     }
     return(temp)
}

getValidConc <- function(data)
{
     return(sort(unique(data$Conc[data$Conc > 0.0001 & !is.na(data$Conc)])))
}

getValidCalData <- function(data)
{
     ret <- data.frame()
     for(day in unique(data$Day))
     {
          ret <- rbind(ret, getCalForDay(day, data))
     }
     return(ret)
}

getModels<- function(data, w)
{
     # use getCalForDay to get cal curve.
     # fit and return set of curves stored by day
     results <- list()
     for(day in unique(data$Day))
     {
          temp <- getCalForDay(day, data)
          x <- log10(temp$Conc)
          y <- log10(temp$RFU)
          results[[as.character(day)]] <- lm(y ~ poly(x,2), weights=w[w$conc %in% temp$Conc,'weight'], na.action=na.omit)
     }
     return(results)
}

getNeedleData <- function()
{
     require(foreign)
     myShortData <- data.frame()
     myTemp <- read.arff('/Users/jaywarrick/Public/DropBox/GitHub/R-InVivoDrugTestingDevice/NeedleData/Measurements S1 - x0_y0.arff')
     myShortData <- rbind(myShortData, reorganizeTable(myTemp))
     myTemp <- read.arff('/Users/jaywarrick/Public/DropBox/GitHub/R-InVivoDrugTestingDevice/NeedleData/Measurements S2 - x0_y0.arff')
     myShortData <- rbind(myShortData, reorganizeTable(myTemp))
     myTemp <- read.arff('/Users/jaywarrick/Public/DropBox/GitHub/R-InVivoDrugTestingDevice/NeedleData/Measurements S3 - x0_y0.arff')
     myShortData <- rbind(myShortData, reorganizeTable(myTemp))
     myTemp <- read.arff('/Users/jaywarrick/Public/DropBox/GitHub/R-InVivoDrugTestingDevice/NeedleData/Measurements S4 - x0_y0.arff')
     myShortData <- rbind(myShortData, reorganizeTable(myTemp))
     myShortData$length

     myLongData <- data.frame()
     myTemp <- read.arff('/Users/jaywarrick/Public/DropBox/GitHub/R-InVivoDrugTestingDevice/NeedleData/Measurements L1 - x0_y0.arff')
     myLongData <- rbind(myLongData, reorganizeTable(myTemp))
     myTemp <- read.arff('/Users/jaywarrick/Public/DropBox/GitHub/R-InVivoDrugTestingDevice/NeedleData/Measurements L2 - x0_y0.arff')
     myLongData <- rbind(myLongData, reorganizeTable(myTemp))
     myTemp <- read.arff('/Users/jaywarrick/Public/DropBox/GitHub/R-InVivoDrugTestingDevice/NeedleData/Measurements L3 - x0_y0.arff')
     myLongData <- rbind(myLongData, reorganizeTable(myTemp))
     myTemp <- read.arff('/Users/jaywarrick/Public/DropBox/GitHub/R-InVivoDrugTestingDevice/NeedleData/Measurements L4 - x0_y0.arff')
     myLongData <- rbind(myLongData, reorganizeTable(myTemp))

     myRulerData <- data.frame()
     myTemp <- read.arff('/Users/jaywarrick/Public/DropBox/GitHub/R-InVivoDrugTestingDevice/NeedleData/Measurements R - x0_y0.arff')
     myRulerData <- rbind(myRulerData, reorganizeTable(myTemp))

     # pixel measurement of rule is 8 mm long.
     conversionFactor <- 8e-3/myRulerData$length

     myTemp <- data.frame(short=myShortData$length, long=myLongData$length, diff=myLongData$length-myShortData$length, lengthAdjustment=(myLongData$length-myShortData$length)*comsolCalcs()$ratio, equPixLength=myShortData$length +
                               (myLongData$length-myShortData$length)*comsolCalcs()$ratio, equMetricLength=conversionFactor*(myShortData$length +
                                                                                                                                  (myLongData$length-myShortData$length)*comsolCalcs()$ratio))

     return(list(short=myShortData, long=myLongData, ruler=myRulerData, conversionFactor=conversionFactor, calcs=myTemp))
}

getModel <- function(times=seq(from = 0, to = 29*24*3600, by = 3600), V1=(1.8e-6)/1000, V2=(400e-6)/1000, D=414e-12, r=(0.006*24.5e-3)/2, A=pi*(r)^2, L=3.34e-3, dV2=((1.5e-6)/1000)/(24*3600), yini=c(C1=100, C2=0), capsuleCoef=D*A/(L*V1), tubeCoef=D*A/L)
#getModel <- function(times=seq(from = 0, to = 29*24*3600, by = 3600), V1=(2.136e-6)/1000, V2=(400e-6)/1000, D=414e-12, r=(0.006*24.5e-3)/2, A=pi*(r)^2, L=3.34e-3, dV2=((1.5e-6)/1000)/(24*3600), yini=c(C1=100, C2=0), tubeCoef=D*A/L, capsuleCoef=tubeCoef/V1)
{
     for(i in 1:length(capsuleCoef))
     {

     }
     require(deSolve)
     derivs <- function (t, y, parms) {
          with(as.list(y), {
               dC1 <- (capsuleCoef)*(C2-C1)
               dC2 <- -(tubeCoef/(V2-dV2*t))*(C2-C1)
               list(c(dC1, dC2))
          }) }

     out <- ode(y = yini, times = times, func = derivs, parms = NULL)
     return(list(times=times, C1=out[,'C1'], C2=out[,'C2'], params=list(capsuleCoef=capsuleCoef, tubeCoef=tubeCoef, dV2=dV2, yini=yini), VALD=list(V1=V1, V2=V2, r=r, A=A, L=L, D=D)))
}

plotModel <- function(add=T, capsule=F, lty=1, col='black', ...)
{
     out <- getModel(...)
     if(add & !capsule)
     {
          lines(out$time/(24*3600), out$C2 + 0.02151873, type='l', lty=lty, col=col)
     }
     if(!add & !capsule)
     {
          plot(out$time/(24*3600), out$C2 + 0.02151873, type='l', lty=lty, col=col)
     }
     if(add & capsule)
     {
          lines(out$time/(24*3600), out$C1, type='l', lty=lty, col=col)
     }
     if(!add & capsule)
     {
          plot(out$time/(24*3600), out$C1, type='l', lty=lty, col=col)
     }
     return(out)
}

getTotalUncertainty <- function(dA, dL, dV, V, A, L, D=414e-12)
{
     # based on (D*A)/(L*V)
     if(dV!=0)
     {
          return(sqrt((-(A*D*dV)/(L*V^2))^2+(-(A*D*dL)/(L^2*V))^2+((D*dA)/(L*V))^2))
     }
     else
     {
          return(sqrt((-(A*D*dL)/L^2)^2+((D*dA)/L)^2))
     }
}

getAreaUncertainty <- function(dR, r=0.32e-3)
{
     # based on A = pi*r^2
     return(sqrt((2*pi*r*dR)^2))
}

comsolCalcs <- function()
{
     #      Base opening
     #      Coordinates: (1.55575e-4,0,-0.002995), Point: 29
     #      Coordinates: (-7.9375e-5,0,3.627355e-4), Point: 10
     #
     #      Top opening
     #      Coordinates: (7.9375e-5,0,7.988976e-4), Point: 24
     #      Coordinates: (1.55575e-4,0,-0.002995), Point: 29

     D <- 100e-12
     C1 <- 1
     C2 <- 0
     L_long <- 7.988976e-4 - (-0.002995)
     L_short <- 3.627355e-4 - (-0.002995)

     analytical_long <- pi*(0.5*0.00625*25.4e-3)^2*D*(C1-C2)/(L_long)
     analytical_short <- pi*(0.5*0.00625*25.4e-3)^2*D*(C1-C2)/(L_short)

     ret <- list()
     ret$lengths <- c(L_short, L_long)
     ret$analytical_long <- analytical_long
     ret$analytical_short <- analytical_short
     ret$comsol <- 5.679279e-16 # [mol/s]
     ret$comsolEqLength <- pi*(0.5*0.00625*25.4e-3)^2*D*(C1-C2)/(5.679279e-16)
     ret$ratio <- (ret$comsolEqLength-L_short)/(L_long-L_short)

     return(ret)
}
