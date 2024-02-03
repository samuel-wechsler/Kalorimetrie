# tabula rase
rm(list=ls())

# import custom functions
source("../functions.R")

## Part II: heat capacity of various liquids
# parse results from calibration
source("../calibration.R")

dewar.C <- dewar.C + 20

# parse data
data20 <- parseData("heatcapacity_20.dat")
data25 <- parseData("heatcapacity_25.dat")
data30 <- parseData("heatcapacity_30r.dat")

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

m.20 <- 96.8500 / 1000 
m.25 <- 96.0870 / 1000
m.30 <- 95.3735 / 1000

# calculate specific heat capacity of liquids
c.20 <- specificHeatCapacity(slope20$slope, dewar.C, m.20)
c.25 <- specificHeatCapacity(slope25$slope, dewar.C, m.25)
c.30 <- specificHeatCapacity(slope30$slope, dewar.C, m.30)

U <- 9.6
I <- 1.37

slope_b <- list(slope=dewar.slope.mean, slope_se=dewar.slope.se)


CI.20 <- error_prop_specificHeatCapacity(U, I, m.20, slope20, slope_b) * qt(0.975, df=2)
CI.25 <- error_prop_specificHeatCapacity(U, I, m.25, slope25, slope_b) * qt(0.975, df=2)
CI.30 <- error_prop_specificHeatCapacity(U, I, m.30, slope30, slope_b) * qt(0.975, df=2)

mean_ci_ethanol20 <- list(mean=c.20, ci=CI.20)
mean_ci_ethanol25 <- list(mean=c.25, ci=CI.25)
mean_ci_ethanol30 <- list(mean=c.30, ci=CI.30)

# literature data provided by Dr. Meister
lit_data <- read.table("KLM_CpWasserEthanolLitdat.txt")
m_per <- (1 - lit_data[,3])*100  # convert to mass percentage of ethanol
csp <- lit_data[,6] * 1000 # convert to J/kg

find_closest_idx <- function(number, vector) {
  idx <- which(abs(vector - number) == min(abs(vector - number)))
  return (idx)
}

c.20.lit <- csp[find_closest_idx(p.20, m_per)]
p.20.lit <- m_per[find_closest_idx(p.20, m_per)]
c.25.lit <- csp[find_closest_idx(p.25, m_per)]
p.25.lit <- m_per[find_closest_idx(p.25, m_per)]
c.30.lit <- csp[find_closest_idx(p.30, m_per)]
p.30.lit <- m_per[find_closest_idx(p.30, m_per)]

cat("Specific heat capacity of ethanol: \n")
cat(paste(p.20,"% ethanol: ", format_mean_ci(mean_ci_ethanol20)))
cat(paste(p.25,"% ethanol: ", format_mean_ci(mean_ci_ethanol25)))
cat(paste(p.30,"% ethanol: ", format_mean_ci(mean_ci_ethanol30)))


cat("Comparison to literature values:\n")
cat(paste(p.20.lit,"% ethanol: ", c.20.lit, "\n"))
cat(paste(p.25.lit,"% ethanol: ", c.25.lit, "\n"))
cat(paste(p.30.lit,"% ethanol: ", c.30.lit, "\n"))

cat("Deviations from literature values\n")
cat(paste(p.20.lit,"% ethanol: ", c.20 - c.20.lit, "\n"))
cat(paste(p.25.lit,"% ethanol: ", c.25 - c.25.lit, "\n"))
cat(paste(p.30.lit,"% ethanol: ", c.30 - c.30.lit, "\n"))
