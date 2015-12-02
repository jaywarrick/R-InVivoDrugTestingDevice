needleData <- getNeedleData()
require(pastecs)
require(deSolve)
stat.desc(needleData$calcs)

out <- plotModel(add=T, V1=fit$par[1], col='blue') # Parameters defined by best fit (defined at bottom of this file)
#out <- plotModel(add=T, col='red') # Parameters defined by mesaured values
# Calculate / define uncertainties
dRPlus = (0.001*25.4e-3)/3 # i.e. 1/3 of stated limits (re: 3 sigma = 99%)
dRMinus = (0.0005*25.4e-3)/3
dAPlus <- getAreaUncertainty(dR=dRPlus, r=out$VALD$r)
dAMinus <- getAreaUncertainty(dR=dRMinus, r=out$VALD$r)
dL <- mad(needleData$calcs$equMetricLength) #0.02*out$VALD$L/3
dV <- (0.109e-6)/1000 # based on experimental images (1 sigma value)
dCapsulePlus <- getTotalUncertainty(dA=dAPlus, dL=dL, dV=dV, V=out$VALD$V1, A=out$VALD$A, L=out$VALD$L, D=out$VALD$D)
dCapsuleMinus <- getTotalUncertainty(dA=dAMinus, dL=dL, dV=dV, V=out$VALD$V1, A=out$VALD$A, L=out$VALD$L, D=out$VALD$D)
dTubePlus <- getTotalUncertainty(dA=dAPlus, dL=dL, dV=0, V=out$VALD$V2, A=out$VALD$A, L=out$VALD$L, D=out$VALD$D)
dTubeMinus <- getTotalUncertainty(dA=dAMinus, dL=dL, dV=0, V=out$VALD$V2, A=out$VALD$A, L=out$VALD$L, D=out$VALD$D)

params <- out$params
paramsHi <- params
paramsHi$capsuleCoef <- params$capsuleCoef + dCapsulePlus
paramsHi$tubeCoef <- params$tubeCoef + 1.96*dTubePlus
paramsHi$col='red'
paramsHi$capsule=F
outHi <- do.call(plotModel, paramsHi)
paramsLo <- params
paramsLo$capsuleCoef <- params$capsuleCoef - dCapsuleMinus
paramsLo$tubeCoef <- params$tubeCoef - 1.96*dTubeMinus
paramsLo$col='red'
paramsLo$capsule=F
outLo <- do.call(plotModel, paramsLo)

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
plotModel(add=T, V1=fit$par[1], col='blue')

