plotModel()

lines(times/(24*3600), out[,'C2'] + 0.024732, type='l', col='red')

V1 <- (2.1e-6)/1000 # m^3 # LO
V2 <- (400e-6)/1000 # m^3
D <- (414e-12) # m^2/s
r <- (0.006*24.5e-3)/2 - (0.00000625) # LO
A <- pi*(r)^2 # m^2
L <- 0.135*25.4e-3 # 4e-3 # m
dV2 <- ((1.5e-6)/1000)/(24*3600) # meters^3/s
yini <- c(C1=100, C2=0)
derivs <- function (t, y, parms) {
     with(as.list(y), {
          dC1 <- ((D*A)/(V1*L))*(C2-C1)
          dC2 <- -((D*A)/((V2)*L))*(C2-C1)
          list(c(dC1, dC2))
     }) }
times <- seq(from = 0, to = 29*24*3600, by = 3600)
out   <- ode(y = yini, times = times, func = derivs, parms = NULL, method='rk4')
#plot(out)

lines(times/(24*3600), out[,'C2'] + 0.024732, type='l', col='red')

V1 <- (2.3e-6)/1000 # m^3
V2 <- (400e-6)/1000 # m^3
D <- (414e-12) # m^2/s
r <- (0.006*24.5e-3)/2
A <- pi*(r)^2 # m^2
L <- 0.135*25.4e-3 # 4e-3 # m
dV2 <- ((1.5e-6)/1000)/(24*3600) # meters^3/s
yini <- c(C1=100, C2=0)
derivs <- function (t, y, parms) {
     with(as.list(y), {
          dC1 <- ((D*A)/(V1*L))*(C2-C1)
          dC2 <- -((D*A)/((V2)*L))*(C2-C1)
          list(c(dC1, dC2))
     }) }
times <- seq(from = 0, to = 29*24*3600, by = 3600)
out   <- ode(y = yini, times = times, func = derivs, parms = NULL, method='rk4')
#plot(out)

lines(times/(24*3600), out[,'C2'] + 0.024732, type='l', col='green')

