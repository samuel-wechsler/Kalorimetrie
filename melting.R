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
  temp <-temperature[idx]
  Temp.mw <- mean(temp) # Mean in intervall
  lines(time[idx], temp, lwd=2, col="green")
  abline(h=Temp.mw, lty=3)
  points(p, Temp.mw, pch=21, bg="white", cex=1.0)
  abw <- sd(temp)
  abw.mw <- abw /sqrt(345)
  out <- list(Temp.mw,abw.mw)
  return(out)
  
}


wTi1 <- Temp.Mean()
wTf1 <- Temp.Mean()
wdT1 <- sprintf("%f \u00B1 %f",wTf1[[1]]-wTi1[[1]],  wTf1[[2]]+wTi1[[2]])
#mtext(side=3, line=1, sprintf("temperature differnce = %s",wdT1))
mtext("A",cex=1.5, at=0.03, side="3", outer=TRUE,line=-3)

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
  temp <-temperature[idx]
  Temp.mw <- mean(temp) # Mean in intervall
  lines(time[idx], temp, lwd=2, col="green")
  abline(h=Temp.mw, lty=3)
  points(p, Temp.mw, pch=21, bg="white", cex=1.0)
  abw <- sd(temp)
  abw.mw <- abw /sqrt(345)
  out <- list(Temp.mw,abw.mw)
  return(out)
  
}


wTi2 <- Temp.Mean()
wTf2 <- Temp.Mean()
wdT2 <- sprintf("%f \u00B1 %f",wTf2[[1]]-wTi2[[1]],  wTf2[[2]]+wTi2[[2]])
#mtext(side=3, line=1, sprintf("temperature differnce = %s",wdT2))

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
  temp <-temperature[idx]
  Temp.mw <- mean(temp) # Mean in intervall
  lines(time[idx], temp, lwd=2, col="green")
  abline(h=Temp.mw, lty=3)
  points(p, Temp.mw, pch=21, bg="white", cex=1.0)
  abw <- sd(temp)
  abw.mw <- abw /sqrt(345)
  out <- list(Temp.mw,abw.mw)
  return(out)
  

}


wTi3 <- Temp.Mean()
wTf3 <- Temp.Mean()
wdT3 <- sprintf("%f \u00B1 %f",wTf3[[1]]-wTi3[[1]],  wTf3[[2]]+wTi3[[2]])
#mtext(side=3, line=1, sprintf("temperature differnce = %s",wdT3))

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
  temp <-temperature[idx]
  Temp.mw <- mean(temp) # Mean in intervall
  lines(time[idx], temp, lwd=2, col="green")
  abline(h=Temp.mw, lty=3)
  points(p, Temp.mw, pch=21, bg="white", cex=1.0)
  abw <- sd(temp)
  abw.mw <- abw /sqrt(345)
  out <- list(Temp.mw,abw.mw)
  return(out)
  
}


iTi1 <- Temp.Mean()
iTf1<- Temp.Mean()
idT1 <- sprintf("%f \u00B1 %f",iTf1[[1]]-iTi1[[1]],  iTf1[[2]]+iTi1[[2]])
#mtext(side=3, line=1, sprintf("temperature differnce = %s",idT1))
mtext("B",cex=1.5, at=0.03, side="3", outer=TRUE,line=-30)

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
  temp <-temperature[idx]
  Temp.mw <- mean(temp) # Mean in intervall
  lines(time[idx], temp, lwd=2, col="green")
  abline(h=Temp.mw, lty=3)
  points(p, Temp.mw, pch=21, bg="white", cex=1.0)
  abw <- sd(temp)
  abw.mw <- abw /sqrt(345)
  out <- list(Temp.mw,abw.mw)
  return(out)
  
}


iTi2 <- Temp.Mean()
iTf2 <- Temp.Mean()
idT2 <- sprintf("%f \u00B1 %f",iTf2[[1]]-iTi2[[1]],  iTf2[[2]]+iTi2[[2]])
#mtext(side=3, line=1, sprintf("temperature differnce = %.3f K", iTf2-iTi2))

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
  temp <-temperature[idx]
  Temp.mw <- mean(temp) # Mean in intervall
  lines(time[idx], temp, lwd=2, col="green")
  abline(h=Temp.mw, lty=3)
  points(p, Temp.mw, pch=21, bg="white", cex=1.0)
  abw <- sd(temp)
  abw.mw <- abw /sqrt(345)
  out <- list(Temp.mw,abw.mw)
  return(out)
  
}


iTi3 <- Temp.Mean()
iTf3 <- Temp.Mean()
idT3 <- sprintf("%f \u00B1 %f",iTf3[[1]]-iTi3[[1]],  iTf3[[2]]+iTi3[[2]])
#mtext(side=3, line=1, sprintf("temperature differnce = %.3f K", iTf3-iTi3))


dev.copy2pdf(file="Schmelzenthalpie4-plot.pdf", width=8,height= 7)

wthetai <- c(wTi1[[1]],wTi2[[1]],wTi3[[1]])
wthetaf <- c(wTf1[[1]],wTf2[[1]],wTf3[[1]])
ithetai <- c(iTi1[[1]],iTi2[[1]],iTi3[[1]])
ithetaf <- c(iTf1[[1]],iTf2[[1]],iTf3[[1]])
wDtheta <- wthetaf - wthetai
iDtheta <- ithetaf - ithetai
DmHE <- NULL
for (k in 1:3){
  for(j in 1:3) {
    DmHE <- c(DmHE,
    cpW*((mEW[k]/mE[j])*(iDtheta[j]/wDtheta[k])*wthetaf[k]-ithetaf[j]))
  }
}

#Standard deviations
sdwthetai <- c(wTi1[[2]],wTi2[[2]],wTi3[[2]])
sdwthetaf <- c(wTf1[[2]],wTf2[[2]],wTf3[[2]])
sdithetai <- c(iTi1[[2]],iTi2[[2]],iTi3[[2]])
sdithetaf <- c(iTf1[[2]],iTf2[[2]],iTf3[[2]])






#Melting enthalpy of ice
DmHE_mean <- mean(DmHE)
DmHE_vi <- qt(0.975,df=8)*(sd(DmHE)/sqrt(9))




#Tables Final
fintab <- data.frame(mE,ithetai, sdithetai ,ithetaf, sdithetaf, iDtheta, sdithetai+sdithetaf)
wfintab <- data.frame(mEW,wthetai,sdwthetai, wthetaf,sdwthetaf, wDtheta, sdwthetai+sdwthetaf)

#colnames(restab) <- names
print(xrestab)

write.table(wfintab, file="Tabelle_Eiswasser1.dat")
write.table(fintab, file="Tabelle_Eis1.dat")

read.table("Tabelle_Eis1.dat")
read.table("Tabelle_Eiswasser1.dat")


wthetai +273.15
wthetaf +273.15
ithetai+ 273.15
ithetaf +273.15

asdf <- c(0.311,0.311,0.308)
mean(asdf)
sd(asdf)/sqrt(3)
