library(drc)
library(drm)
globalFctDefA <- list(name='global',fct=fctA, ssfct=ssfctA, names=c('alpha','tau'));

cont <- drmc(constr = FALSE, errorm = TRUE, maxIt = 1000, method="SANN", noMessage = FALSE, relTol = 1e-07, rmNA=FALSE, useD = FALSE, trace = TRUE, otrace = TRUE, warnVal = -1, dscaleThres = 1e-15, rscaleThres = 1e-15)


# Define various equations to fit the trajectory data with
# Define y = e^(alpha*(x-tau))
fctA <- function(x, par)
{
     V1 <- (2.25e-6)/1000 # m^3
     V2 <- (400e-6)/1000 # m^3
     D <- (400e-12) # m^2/s
     r <- (0.006*24.5e-3)/2
     A <- pi*(r)^2 # m^2
     L <- 3.5e-3 # m
     dV2 <- ((1.5e-6)/1000)/(24*3600) # meters^3/s
     yini <- c(C1=100, C2=0)
     derivs <- function (t, y, parms) {
          with(as.list(y), {
               dC1 <- ((D*A)/(V1*L))*(C2-C1)
               dC2 <- -((D*A)/((V2-dV2*t)*L))*(C2-C1)
               list(c(dC1, dC2))
          }) }
     times <- seq(from = 0, to = 29*24*3600, by = 3600)
}

# Self-starter function for determining initial conditions.
ssfctA <- function(data)
{
     # Just define because system behavior is almost matched
     V1 <- (2.25e-6)/1000 # m^3
     V2 <- (400e-6)/1000 # m^3
     D <- (400e-12) # m^2/s
     r <- (0.006*24.5e-3)/2
     A <- pi*(r)^2 # m^2
     L <- 3.5e-3 # m
     dV2 <- ((1.5e-6)/1000)/(24*3600) # meters^3/s
     modelo <- lm(log(data$y)~data$x)
     #     print(summary(modelo))
     alpha <- coef(modelo)[2]
     tau <- -1*coef(modelo)[1]/alpha
     return(c(alpha, tau))
}

print('fitting modA.R')
modA.R <- drm(R~time,data=subsetDataR,ID,pmodels=data.frame(ID,1),fct=globalFctDefA, weights=subsetDataR$w, na.action=na.omit, separate=FALSE, lowerl=c(0,0), upperl=c(4,50))
summary(modA.R)
plot(modA.R, ylim=c(1,1000), log='y')

fct <- function(L=3.5e-3)
{

}

getModel <- function(add=T, V1=(2.3e-6)/1000, V2=(400e-6)/1000, D=414e-12, r=(0.006*24.5e-3)/2, L=4e-3, dV2=((1.5e-6)/1000)/(24*3600), yini=c(C1=100, C2=0))
{
     #      V1 <- (2.6e-6)/1000 # m^3
     V2 <- (400e-6)/1000 # m^3
     D <- (414e-12) # m^2/s
     r <- (0.006*24.5e-3)/2
     A <- pi*(r)^2 # m^2
     L <- 4e-3 # m
     dV2 <- ((1.5e-6)/1000)/(24*3600) # meters^3/s
     yini <- c(C1=100, C2=0)
     derivs <- function (t, y, parms) {
          with(as.list(y), {
               dC1 <- ((D*A)/(V1*L))*(C2-C1)
               dC2 <- -((D*A)/((V2)*L))*(C2-C1)
               list(c(dC1, dC2))
          }) }
     times <- seq(from = 0, to = 29*24*3600, by = 3600)
     out   <- ode(y = yini, times = times, func = derivs, parms = NULL)
     return(out)
}


#plot(out)
if(add)
{

}else
{
     lines(times/(24*3600), out[,'C2'] + 0.02151873, type='l', col='red')
}


V1 <- (2.1e-6)/1000 # m^3
r <- (0.006*24.5e-3)/2
derivs <- function (t, y, parms) {
     with(as.list(y), {
          dC1 <- ((D*A)/(V1*L))*(C2-C1)
          dC2 <- -((D*A)/((V2)*L))*(C2-C1)
          list(c(dC1, dC2))
     }) }
times <- seq(from = 0, to = 29*24*3600, by = 3600)
out   <- ode(y = yini, times = times, func = derivs, parms = NULL, method='rk4')
lines(times/(24*3600), out[,'C2'] + 0.02151873, type='l', col='red')

V1 <- (2.3e-6)/1000 # m^3
r <- (0.006*24.5e-3)/2 # + 25 microns - 12.5 microns (diameter)
derivs <- function (t, y, parms) {
     with(as.list(y), {
          dC1 <- ((D*A)/(V1*L))*(C2-C1)
          dC2 <- -((D*A)/((V2)*L))*(C2-C1)
          list(c(dC1, dC2))
     }) }
times <- seq(from = 0, to = 29*24*3600, by = 3600)
out <- ode(y = yini, times = times, func = derivs, parms = NULL, method='rk4')
lines(times/(24*3600), out[,'C2'] + 0.02151873, type='l', col='green')

x
y
z
d
