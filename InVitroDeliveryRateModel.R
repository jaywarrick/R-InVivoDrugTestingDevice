needleData <- getNeedleData()
source('/Users/jaywarrick/Public/DropBox/GitHub/R-InVivoDrugTestingDevice/InVitroData_HelperFunctions.R')
require(pastecs)
require(deSolve)
stat.desc(needleData$calcs)

times <- seq(from = 0, to = 29*24*3600, by = 3600)

out <- plotModel(add=T, V1=fit$par[1], normalizationFactor=normalizationFactor, col='blue') # Parameters defined by best fit (defined at bottom of this file)
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
paramsHi$normalizationFactor=normalizationFactor
paramsHi$V1 = as.numeric(fit$par['V1'])
outHi <- do.call(plotModel, paramsHi)
paramsLo <- params
paramsLo$capsuleCoef <- params$capsuleCoef - dCapsuleMinus
paramsLo$tubeCoef <- params$tubeCoef - 1.96*dTubeMinus
paramsLo$col='red'
paramsLo$capsule=F
paramsLo$normalizationFactor=normalizationFactor
paramsLo$V1 = as.numeric(fit$par['V1'])
outLo <- do.call(plotModel, paramsLo)

duh <- plotModel(add=T, capsule=F, V1=fit$par[1], col='blue', normalizationFactor=normalizationFactor, times=seq(from = 0, to = 30*24*3600, by = 3600))
par(new=T)
plotModel(add=T, capsule=T, V1=fit$par[1], col='green', normalizationFactor=100, times=seq(from = 0, to = 30*24*3600, by = 3600))

dev.off()

