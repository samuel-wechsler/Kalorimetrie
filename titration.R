# Tabula rase
rm(list=ls())

# import functions
source("../functions.R")

# parse Titration data
cont_titration.1 <- parseData("titration_continuous_1_samuel_wechsler.dat")
cont_titration.2 <- parseData("titration_continuous_2_samuel_wechsler.dat")
disc_titration <- parseData("titration_discrete_1_samuel_wechsler.dat")

# # plot data
plotContTitration(cont_titration.2, t=c(60, 150, 150, 250))
dev.copy2pdf(file = "../plots/continuous_titration.pdf", width = 8, height = 8)
dev.off()  # Close the PDF device
plotDiscTitration(disc_titration, t=256)
dev.copy2pdf(file = "../plots/discrete_titration.pdf", width = 8, height = 8)
dev.off()  # Close the PDF device
