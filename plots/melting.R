#Kalorimetrei Versuch Schmelzenthalpie von Eis

#rm(list =ls())
setwd("/Users/kainebel/Desktop/Studium/1. Semester/R")
source("Constants2.R")

#read data
iceone <- read.table("ice_1_samuel_wechsler.dat", sep = "")
icetwo <- read.table("ice_2_samuel_wechsler.dat", sep = "")
icethree <- read.table("ice_1_samuel_wechsler.dat", sep = "")
iWone <- read.table("ice_water_1_samuel_wechsler.dat", sep = "")
iWtwo <- read.table("icewater_2_samuel_wechsler.dat", sep = "")
iWthree <- read.table("ice_water_3_samuel_wechsler.dat", sep = "")



cpW <- 4.182
mEW <- c(13.05, 17.11, 16.98)
mE  <- c(16.63, 16.91, 17.11)

par(mfrow = c(2,3))

#Ice Water 1

time <- iWone[,1]
temperature <- iWone[,2]
plot(time, temperature, type="l", lwd = 2,
     xlab=expression("time "*italic(t)*"/s"),
     ylab=expression("temperature  " *theta* "/" *degree*"C"),
     ylim=c(6,20)  ,yaxs="i"
)




Temp.Mean <- function() { # Temp Mean Function
  tw <- 20.0
  p <- locator(1)$x #choose PT in graph
  idx <- which(abs(time-p) <= tw)
  Temp.mw <- mean(temperature[idx]) # Mean in intervall
  lines(time[idx], temperature[idx], lwd=2, col="green")
  abline(h=Temp.mw, lty=3)
  points(p, Temp.mw, pch=21, bg="white", cex=1.0)
  return(Temp.mw)
}


wTi1 <- Temp.Mean()
wTf1 <- Temp.Mean()

mtext(side=3, line=1, sprintf("temperature differnce = %.3f K", wTf1-wTi1))


#Ice Water 2
time <- iWtwo[,1]
temperature <- iWtwo[,2]
plot(time, temperature, type="l", lwd = 2,
     xlab=expression("time "*italic(t)*"/s"),
     ylab=expression("temperature  " *theta* "/" *degree*"C"),ylim=c(6,20),yaxs="i"
)




Temp.Mean <- function() { # Temp Mean Function
  tw <- 20.0
  p <- locator(1)$x #choose PT in graph
  idx <- which(abs(time-p) <= tw)
  Temp.mw <- mean(temperature[idx]) # Mean in intervall
  lines(time[idx], temperature[idx], lwd=2, col="green")
  abline(h=Temp.mw, lty=3)
  points(p, Temp.mw, pch=21, bg="white", cex=1.0)
  return(Temp.mw)
}


wTi2 <- Temp.Mean()
wTf2 <- Temp.Mean()
mtext(side=3, line=1, sprintf("temperature differnce = %.3f K", wTf2-wTi2))

#Ice Water 3
time <- iWthree[,1]
temperature <- iWthree[,2]
plot(time, temperature, type="l", lwd = 2,
     xlab=expression("time "*italic(t)*"/s"),
     ylab=expression("temperature  " *theta* "/" *degree*"C"),ylim=c(6,20) ,yaxs="i"
)




Temp.Mean <- function() { # Temp Mean Function
  tw <- 20.0
  p <- locator(1)$x #choose PT in graph
  idx <- which(abs(time-p) <= tw)
  Temp.mw <- mean(temperature[idx]) # Mean in intervall
  lines(time[idx], temperature[idx], lwd=2, col="green")
  abline(h=Temp.mw, lty=3)
  points(p, Temp.mw, pch=21, bg="white", cex=1.0)
  return(Temp.mw)
}


wTi3 <- Temp.Mean()
wTf3 <- Temp.Mean()
mtext(side=3, line=1, sprintf("temperature differnce = %.3f K", wTf3-wTi3))

#iceone
time <- iceone[,1]
temperature <- iceone[,2]
plot(time, temperature, type="l", lwd = 2,
     xlab=expression("time "*italic(t)*"/s"),
     ylab=expression("temperature  " *theta* "/" *degree*"C"),ylim=c(6,20) ,yaxs="i"
     )




Temp.Mean <- function() { # Temp Mean Function
  tw <- 20.0
  p <- locator(1)$x #choose PT in graph
  idx <- which(abs(time-p) <= tw)
  Temp.mw <- mean(temperature[idx]) # Mean in intervall
  lines(time[idx], temperature[idx], lwd=2, col="green")
  abline(h=Temp.mw, lty=3)
  points(p, Temp.mw, pch=21, bg="white", cex=1.0)
  return(Temp.mw)
}


iTi1 <- Temp.Mean()
iTf1<- Temp.Mean()
mtext(side=3, line=1, sprintf("temperature differnce = %.3f K", iTf1-iTi1))

#icetwo


time <- icetwo[,1]
temperature <- icetwo[,2]
plot(time, temperature, type="l", lwd = 2,
     xlab=expression("time "*italic(t)*"/s"),
     ylab=expression("temperature  " *theta* "/" *degree*"C"),ylim=c(6,20) ,yaxs="i"
)




Temp.Mean <- function() { # Temp Mean Function
  tw <- 20.0
  p <- locator(1)$x #choose PT in graph
  idx <- which(abs(time-p) <= tw)
  Temp.mw <- mean(temperature[idx]) # Mean in intervall
  lines(time[idx], temperature[idx], lwd=2, col="green")
  abline(h=Temp.mw, lty=3)
  points(p, Temp.mw, pch=21, bg="white", cex=1.0)
  return(Temp.mw)
}


iTi2 <- Temp.Mean()
iTf2 <- Temp.Mean()
mtext(side=3, line=1, sprintf("temperature differnce = %.3f K", iTf2-iTi2))

#icethree
time <- iceone[,1]
temperature <- iceone[,2]
plot(time, temperature, type="l", lwd = 2,
     xlab=expression("time "*italic(t)*"/s"),
     ylab=expression("temperature  " *theta* "/" *degree*"C"),ylim=c(6,20) ,yaxs="i"
)




Temp.Mean <- function() { # Temp Mean Function
  tw <- 20.0
  p <- locator(1)$x #choose PT in graph
  idx <- which(abs(time-p) <= tw)
  Temp.mw <- mean(temperature[idx]) # Mean in intervall
  lines(time[idx], temperature[idx], lwd=2, col="green")
  abline(h=Temp.mw, lty=3)
  points(p, Temp.mw, pch=21, bg="white", cex=1.0)
  return(Temp.mw)
}


iTi3 <- Temp.Mean()
iTf3 <- Temp.Mean()
mtext(side=3, line=1, sprintf("temperature differnce = %.3f K", iTf3-iTi3))


#dev.copy2pdf(file="Schmelzenthalpie3-plot.pdf", width=8,height= 7)

wthetai <- c(wTi1,wTi2,wTi3)
wthetaf <- c(wTf1,wTf2,wTf3)
ithetai <- c(iTi1,iTi2,iTi3)
ithetaf <- c(iTf1,iTf2,iTf3)
wDtheta <- wthetaf - wthetai
iDtheta <- ithetaf - ithetai
DmHE <- NULL
for (k in 1:3){
  for(j in 1:3) {
    DmHE <- c(DmHE,
    cpW*((mEW[k]/mE[j])*(iDtheta[j]/wDtheta[k])*wthetaf[k]-ithetaf[j]))
  }
}
#Melting enthropy of ice water
DmHE_mean <- mean(DmHE)
DmHE_vi <- qt(0.975,df=8)*sd(DmHE)/sqrt(9)


masses <- mEW
tempi <- c(wTi1,wTi2,wTi3)
tempf <- c(wTf1,wTf2,wTf3)
deltemp <- c(wTf1-wTi1,wTf2-wTi2,wTf3-wTi3)
restab <- data.frame(masses,tempi,tempf,deltemp, col.names = FALSE)
#names <- c("Ice Water",expression("m"[iw] * "/g"),expression(theta[iwi] *"/" *degree*"C"),expression(theta[iwf]*"/"*degree*"C"),expression(Delta * theta[iw]*"/" *degree*"C")


#colnames(restab) <- names
print(restab)

#Melting enthropy of ice
DmHE_mean <- mean(DmHE)
DmHE_vi <- qt(0.975,df=8)*sd(DmHE)/sqrt(9)


xmasses <- mE
xtempi <- c(iTi1,iTi2,iTi3)
xtempf <- c(iTf1,iTf2,iTf3)
xdeltemp <- c(iTf1-iTi1,iTf2-iTi2,iTf3-iTi3)
xrestab <- data.frame(xmasses,xtempi,xtempf,xdeltemp)
#names <- c("Ice Water",expression("m"[iw] * "/g"),expression(theta[iwi] *"/" *degree*"C"),expression(theta[iwf]*"/"*degree*"C"),expression(Delta * theta[iw]*"/" *degree*"C")


#colnames(restab) <- names
print(xrestab)

write.table(restab, file="Tabelle_Eiswasser.dat")
write.table(xrestab, file="Tabelle_Eis.dat")

read.table("Tabelle_Eis.dat")
