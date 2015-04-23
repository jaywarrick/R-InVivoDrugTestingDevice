# library(foreign)
# fileTable <- read.arff('/Volumes/MMB Sackmann/Jay/JEX Databases/In Vivo/temp/JEXData0000000001.arff')
# data <- read.arff(fileTable[1,4])
# data <- reorganizeTable(data)
# data <- data[order(data$Point),]
# write.table(data, file='/Users/jaywarrick/Documents/MMB/Presentations/20150417 - IVDTD Update/Mouse5_Side4B.txt')

Mode <- function(x) {
     ux <- unique(x)
     tab <- tabulate(match(x, ux)); ux[tab == max(tab)]
}

data <- read.table('/Users/jaywarrick/Documents/MMB/Presentations/20150417 - IVDTD Update/Mouse5_Side4B.txt')

hist(data$R)
hist(data$G)
hist(data$B)

X <- data$R
Y <- data$G

library(RColorBrewer)
g = 10
my.cols <- rev(brewer.pal(g, "RdYlBu"))

XThresh <- exp(Mode(log(X)) + (Mode(log(X))-min(log(X))))
YThresh <- exp(Mode(log(Y)) + (Mode(log(Y))-min(log(Y))))

# kernel density using MASS
library(MASS)
z <- kde2d(X, Y, n=300)
plot(X, Y, xlab="X", ylab="Y", pch=19, cex=.3, col =rgb(0,0,0,0.1), log='xy')
contour(z, drawlabels=FALSE, nlevels=g, col=my.cols, add=TRUE, lwd = 2)
abline(h=YThresh)
abline(v=XThresh)
XPos <- length(which(X > XThresh))/length(X)
YPos <- length(which(Y > YThresh))/length(Y)
XYPos <- length(which(X > XThresh & Y > YThresh))/length(X)
# abline(h=mean(Y), v=mean(X), lwd=2, col = "black")
# legend("topleft", paste("r=", round(cor(X, Y),2)), bty="n")

r <- hist(log(X), freq=TRUE, breaks=50, plot=FALSE)
plot(exp(r$mids),r$density, type='l', log='x', lwd=2, col='red')
points(exp(r$mids),r$density, col='red')
r <- hist(log(Y), freq=TRUE, breaks=50, plot=FALSE)
lines(exp(r$mids),r$density, type='l', lwd=2, col='green')
points(exp(r$mids),r$density, col='green')
abline(v=YThresh, col='green')
abline(v=XThresh, col='red')

# or alternatively:
barplot(r$density, col="white")
