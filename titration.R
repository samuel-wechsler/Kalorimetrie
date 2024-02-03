# Tabula rase
rm(list=ls())

# import functions
source("../functions.R")

# parse Titration data
cont_titration.1 <- parseData("titration_continuous_1_samuel_wechsler.dat")
cont_titration.2 <- parseData("titration_continuous_2_samuel_wechsler.dat")
disc_titration <- parseData("titration_discrete_1_samuel_wechsler.dat")

# # plot data
a <- 1
par(mfrow=c(1,2), mai = c(1, a, 1, a))  # Adjusted margin values
V1 <- plotContTitration(cont_titration.2, t=c(60, 150, 150, 250), t_0 = 70.31771)
V2 <- plotDiscTitration(disc_titration, t=256, t_0=68.90918-2)

c1 <- titration_concentration(V1, 1, 25)
c2 <- titration_concentration(V2, 1, 25)

mtext("A", side = 3, line = -3, outer = TRUE, adj = 0, cex=3)
mtext("B", side=3,at=0.5, line=-3, outer=TRUE, adj=0, cex=3)

dev.copy2pdf(file = "../plots/titration.pdf", width = 24, height = 8)
dev.off()

cat("Molar concentartions\n")
cat(paste("Continuous: ", c1, "mol/L\n"))
cat(paste("Discrete: ", c2, "mol/L\n"))


M <-60.05 # obtained from https://pubchem.ncbi.nlm.nih.gov/compound/Acetic-Acid
cat("Concentrations in g/L\n")
cat(paste("Continuous: ", c1 * M, "mol/L\n"))
cat(paste("Discrete: ", c2 * M, "mol/L\n"))

