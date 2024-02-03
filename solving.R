# tablua rase
rm(list=ls())

# import functions
source("../functions.R")

# import data
solve1 <- parseData("solving_enthalpy_1_samuel_wechsler.dat")
solve2 <- parseData("solving_enthalpy_2_samuel_wechsler.dat")
solve3 <- parseData("solving_enthalpy_3_samuel_wechsler.dat")



# plot data and determine temperature differences
a <- 0.8
par(mfrow=c(1,3), mai = c(1, a, 1, a))
deltaT1 <- plotTempSolv(solve1$zeit, solve1$temp, c(50, 100, 200, 307), c(220, 292))
deltaT2 <- plotTempSolv(solve2$zeit, solve2$temp, c(50, 100, 230, 340), c(248, 328))
deltaT3 <- plotTempSolv(solve3$zeit, solve3$temp, c(50, 105, 220, 322), c(248, 310))
deltaT <- c(deltaT1, deltaT2, deltaT3)
dev.copy2pdf(file = "../plots/solving.pdf", width = 24, height = 8)
dev.off()

print(deltaT3)

# mass of NH4NO3
M <- 80.04
m1 <- 0.5045
m2 <- 0.5053
m3 <- 0.5060

# calculate solving entahlpy
Hsolv1 <- solvEnthalpy(M, m1, deltaT1)
Hsolv2 <- solvEnthalpy(M, m2, deltaT2)
Hsolv3 <- solvEnthalpy(M, m3, deltaT3)

# measure duration of calibration
Hsolv <- c(Hsolv1, Hsolv2 , Hsolv3) / 1000
Hsolv <- c(Hsolv1, Hsolv2) / 1000
Hsolv.mw <- mean(Hsolv)
Hsolv.sd <- sd(Hsolv)
Hsolv.se <- Hsolv.sd / sqrt(2)
Hsolv.ci <- Hsolv.se * qt(0.975, df=2)

# print results
cat("molar solution enthalpy of NH4NO3\n")
cat(paste(round(Hsolv.mw, 4), "±", round(Hsolv.ci, 4)))