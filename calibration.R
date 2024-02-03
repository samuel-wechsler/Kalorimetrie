# Tablua rasa
rm(list=ls())

# import functions
source("../functions.R")

## Part I
# parse data
calib_data_1 <- parseData("calibration_1_samuel_wechsler.dat")
calib_data_2 <- parseData("calibration_2_samuel_wechsler.dat")
calib_data_3 <- parseData("calibration_3_samuel_wechsler.dat")

# # plot data to find temperature differences
a <- 0.5
par(mfrow=c(1,3), mai = c(1, a, 1, a)) 


ylims <- c(19, 22.5)
slope1 <- plotTemp(calib_data_1$zeit, calib_data_1$temp, 50, 150, ylims)
slope2 <- plotTemp(calib_data_2$zeit, calib_data_2$temp, 50, 150, ylims)
slope3 <- plotTemp(calib_data_3$zeit, calib_data_3$temp, 50, 150, ylims)
dewar.slopes <- c(slope1$slope, slope2$slope, slope3$slope)

dewar.slope.mean <- mean(dewar.slopes)
dewar.slope.sd <- sd(dewar.slopes)
dewar.slope.se <- dewar.slope.sd/sqrt(2)

U <- 9.6
I <- 1.37

water.c.sp <- 4182  # J / K / kg
water.rho <- 0.9982  # kg / L
water.m <- 0.1 * 0.9982

system.C <- U * I / dewar.slope.mean
water.C <- water.m * water.c.sp
dewar.C <- system.C - water.C

error_prop_dewarC <- function(U, I, m, a, sa) {
  return (U * I / a**2 * sa)
}
dewar.se <- error_prop_dewarC(U, I, water.m, dewar.slope.mean, dewar.slope.se)
dewar.CI <- qt(0.975, df=2) * dewar.se

print(dewar.se)
print(dewar.C)
print(dewar.CI)

# Save the plot to a PDF file
dev.copy2pdf(file = "../plots/calibration.pdf", width = 24, height = 8)
dev.off()
