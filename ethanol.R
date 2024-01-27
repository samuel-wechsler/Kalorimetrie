# tabula rase
rm(list=ls())

# import custom functions
source("../functions.R")

## Part II: heat capacity of various liquids
# parse results from calibration
source("../calibration.R")

# parse data
data20 <- parseData("heatcapacity_20.dat")
data25 <- parseData("heatcapacity_25.dat")
data30 <- parseData("heatcapacity_30r.dat")

# literature data
lit_data <- read.table("KLM_CpWasserEthanolLitdat.txt")
m_per <- lit_data[,3]
csp <- lit_data[,6]

# plot data
a <- 0.7
par(mfrow=c(1,3), mai = c(1, a, 1, a)) 
ylims <- c(23.5, 28) # enforce identical scaling of y axis

slope20 <- plotTemp(data20$zeit, data20$temp, 50, 150, ylims)
slope25 <- plotTemp(data25$zeit, data25$temp, 50, 150, ylims)
slope30 <- plotTemp(data30$zeit, data30$temp, 50, 150, ylims)
ethanol.slopes <- c(slope20, slope25, slope30)

dev.copy2pdf(file = "../plots/ethanol.pdf", width = 24, height = 8)
dev.off()  # Close the PDF device

# masses measured in experiment
m.20 <- c(51.67, 71.28, 149.27)
m.25 <- c(52.07, 76.98, 149.19)
m.30 <- c(52.20, 89.04, 174.80)

# calculated mass percentage
p.20 <- mass_percentage(m.20)
p.25 <- mass_percentage(m.25)
p.30 <- mass_percentage(m.30)
p <- c(p.20, p.25, p.30)

m.20 <- 0.96850 * 0.1 
m.25 <- (0.96094+0.96080)/2 * 0.1
m.30 <- (0.95382+0.95365)/2 * 0.1

# calculate specific heat capacity of liquids
c.20 <- specificHeatCapacity(slope20$slope, dewar.C, m.20)
c.25 <- specificHeatCapacity(slope25$slope, dewar.C, m.25)
c.30 <- specificHeatCapacity(slope30$slope, dewar.C, m.30)

U <- 9.6
I <- 1.37

slope_b <- list(slope=dewar.slope.mean, slope_se=dewar.slope.se)

error_prop_specificHeatCapacity <- function(U, I, m, slope_a, slope_b) {
  a <- slope_a$slope
  sa <- slope_a$slope_se
  b <- slope_b$slope
  sb <- slope_b$slope_se
  
  # See section in appendix for derivation of below formula
  return (U * I / m * sqrt(1/a**4 * sa**2 + 1/b**4 * sb**2))
}

CI.20 <- error_prop_specificHeatCapacity(U, I, m.20, slope20, slope_b)
CI.25 <- error_prop_specificHeatCapacity(U, I, m.25, slope25, slope_b)
CI.30 <- error_prop_specificHeatCapacity(U, I, m.30, slope30, slope_b)

mean_ci_ethanol20 <- list(mean=c.20, ci=CI.20)
mean_ci_ethanol25 <- list(mean=c.25, ci=CI.25)
mean_ci_ethanol30 <- list(mean=c.30, ci=CI.30)

cat("Specific heat capacity of ethanol: \n")
cat(paste("20% ethanol: ", format_mean_ci(mean_ci_ethanol20)))
cat(paste("25% ethanol: ", format_mean_ci(mean_ci_ethanol25)))
cat(paste("30% ethanol: ", format_mean_ci(mean_ci_ethanol30)))

