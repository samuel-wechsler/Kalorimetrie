library(shape)

# Script containing reused functions

#------------------
# function to wait for user to press [enter]
readkey <- function()
{
  cat("[press [enter] to continue]")
  number <- scan(n=1,what='character')
  return (number)
}
#------------------

# ------------------ 
# Function to parse tempeature-time data
parseData <- function(filename) {
  data <- read.table(filename)
  zeit <- data[,1]
  temp <- data[,2]
  return (list(zeit=zeit, temp=temp))
}
# ------------------

#' Calculate mean and confidence interval
#'
#' This function takes a numeric vector as input and computes the mean
#' along with the confidence interval using a t-distribution.
#'
#' @param data A numeric vector for which the mean and confidence interval
#' need to be calculated.
#'
#' @return A list with two elements: mean and confidence interval.
#' @seealso \code{\link{qt}}
#'
#' @examples
#' data <- c(1, 2, 3, 4, 5)
#' result <- calculate_mean_ci(data)
#' result$mean # contains the mean value
#' result$ci # contains the confidence interval
calculate_mean_ci <- function(data) {
  mean_value <- mean(data)
  sd_value <- sd(data)
  n <- length(data)
  se <- sd_value / sqrt(n)
  
  t_s = qt(0.975, df = n - 1)
  
  ci <- t_s * se  # Using the t-distribution multiplier
  
  return(list(mean = mean_value, ci = ci))
}


#' Format the mean and confidence interval
#'
#' This function takes a list containing mean and confidence interval values
#' and formats them into a string for easy display.
#'
#' @param mean_ci A list with mean and confidence interval values,
#' typically the output of \code{\link{calculate_mean_ci}}.
#'
#' @param r The number of decimal places to round the mean and confidence interval values.
#'
#' @return A character string representing the formatted mean and
#' confidence interval.
#'
#' @examples
#' data <- c(1, 2, 3, 4, 5)
#' result <- calculate_mean_ci(data)
#' formatted_result <- format_mean_ci(result, 3)
#' formatted_result # contains the formatted string for mean and confidence interval
format_mean_ci <- function(mean_ci, r) {
  return(paste(round(mean_ci$mean, r), "\u00B1", round(mean_ci$ci, r), "\n"))
}


# ------------------
# Function to calculat mean of tempeature values over a manually selected interval
Temperatur.Mittelwert <- function(Zeit, Temperatur, p) {
  tw <- 10.0  # Zeitfenster (s) für Mittelung
  
  # Find indices of data points within the time window
  idx <- which(abs(Zeit - p) <= tw)
  
  # Extract data within the time window
  selectedZeit <- Zeit[idx]
  selectedTemperatur <- Temperatur[idx]
  
  # Calculate Temperatur-Mittelwert within the time window
  Temp.mw <- mean(selectedTemperatur)
  
  # Plot the selected data points
  lines(selectedZeit, selectedTemperatur, lwd = 2, col = "black")
  points(p, Temp.mw, pch = 21, bg = "white", cex = 1.0)
  
  return(list(Temp.mw = Temp.mw, p = p))  # Return Temperatur-Mittelwert
}
# ------------------

#-------------------
calculate_slope <- function(x, y, t1, t2) {
  # Find the index where the slope starts to increase
  start_idx <- which(x==t1)
  end_idx <- which(x==t2)
  
  # Subset the data to the identified range
  x_sub <- x[start_idx:end_idx]
  y_sub <- y[start_idx:end_idx]
  
  lm_model <- lm(y_sub ~ x_sub)
  return (lm_model)
}

#-------------------
# function to plot temperatur data of calibration experiment
# see calibration.R, ethanol.R
plotTemp <- function(Zeit, Temperatur, t1, t2, ylims) {
  # Plot temperature data
  par(cex.lab = 2.0, cex.axis = 2)
  plot(Zeit, Temperatur, type = "l", lwd = 2, col = "gray",
       ylim = c(ylims[1], ylims[2]),
       xlab = expression(italic(t) * " / " * s),
       ylab = expression(theta * " / " * degree * C)
  )
  
  # Calculate slope before and after the jump
  lm_model <- calculate_slope(Zeit, Temperatur, t1, t2)
  T1 <- predict(lm_model, newdata = data.frame(x_sub = t1))
  T2 <- predict(lm_model, newdata = data.frame(x_sub = t2))
  
  # Add linear regression line to the plot
  abline(lm_model, col = "blue", lwd = 2)
  
  # Draw arrows to indicate temperature jumps
  segments(x0 = t2, y0 = T1, x1 =t2, y1 = T2, lty=2, lwd = 2, col = "black")
  
  # Annotate arrows
  text(x = t2 + 30, y = (T1 + T2) / 2, labels = bquote(Delta * italic(T) == .(round(T2 - T1, 3)) * italic(K)), cex=2.5)
  
  # Draw arrow to indicate calibration time
  segments(x0=t1, y0=T1, x1=t2, y1=T1, lwd = 2, lty=2, col = "black")
  
  # Annotate arrows
  text(x = (t1 + t2) / 2, y = T1-0.2, labels = bquote(Delta * italic(t) == .(round(t2-t1, 3)) * italic(s)), cex=3)
  
  summary_lm <- summary(lm_model)
  slope <- coef(lm_model)[2]
  slope_se <- summary_lm$coefficients["x_sub", "Std. Error"]
  
  return (list(slope=slope, slope_se=slope_se)) # return slope and standard error
}
#-------------------

#-------------------
# function used to plot temperature-time date of solvation enthalpy data
# see solving.R
plotTempSolv <- function(Zeit, Temperatur, idx, t) {
  par(cex.lab = 2.0, cex.axis = 2)
  plot(Zeit, Temperatur, type = "l", lwd = 2, col = "gray",
       ylim = c(19, 21.7),
       xlab = expression(italic(t) * " / " * s),
       ylab = expression(theta * " / " * degree * C)
  )
  
  # Measure temperature jumps and calibration time
  point1 <- Temperatur.Mittelwert(Zeit, Temperatur, idx[1])  # Temperatur vor Lösung
  T1 <- point1$Temp.mw
  t1 <- point1$p
  
  point2 <- Temperatur.Mittelwert(Zeit, Temperatur, idx[2])  # Temperatur nach Lösung
  T2 <- point2$Temp.mw
  t2 <- point2$p
  
  point3 <- Temperatur.Mittelwert(Zeit, Temperatur, idx[3])  # Temperatur vor Kalib
  T3 <- point3$Temp.mw
  t3 <- point3$p
  
  point4 <- Temperatur.Mittelwert(Zeit, Temperatur, idx[4])  # Temperatur nach Kalib
  T4 <- point4$Temp.mw
  t4 <- point4$p
  
  # Tcalib <- measureCalibrationTime(Zeit, Temperatur)  # Measure calibration time
  # 
  abline(v=t[1], lty=3)
  abline(v=t[2], lty=3) 
  Tcalib <- t[2] - t[1]
  
  # Draw dotted line segments to indicate mean temperature on given intervals
  segments(x0 = (t1 + t2) / 2 - 50, y0 = T1, x1 = (t1 + t2) / 2 + 50, y1 = T1, lty = 3)
  segments(x0 = (t1 + t2) / 2 - 50, y0 = T2, x1 = (t1 + t2) / 2 + 50, y1 = T2, lty = 3)
  segments(x0 = (t3 + t4) / 2 - 50, y0 = T3, x1 = (t3 + t4) / 2 + 50, y1 = T3, lty = 3)
  segments(x0 = (t3 + t4) / 2 - 50, y0 = T4, x1 = (t3 + t4) / 2 + 50, y1 = T4, lty = 3)
  
  # Draw arrows to indicate temperature jumps
  arrows(x0 = (t1 + t2) / 2, y0 = T1, x1 = (t1 + t2) / 2, y1 = T2, angle = 30, length = 0.1, lwd = 2, code = 2, col = "black")
  arrows(x0 = (t3 + t4) / 2, y0 = T3, x1 = (t3 + t4) / 2, y1 = T4, angle = 30, length = 0.1, lwd = 2, code = 2, col = "black")
  
  # Annotate arrows
  text(x = (t1 + t2) / 2 + 70, y = (T1 + T2) / 2, labels = bquote(Delta * italic(T[S]) == .(round(T2 - T1, 3)) * italic(K)), cex=3)
  text(x = (t3 + t4) / 2 - 70, y = (T3 + T4) / 2, labels = bquote(Delta * italic(T[C]) == .(round(T4 - T3, 3)) * italic(K)), cex=3)
  
  
  return(list(deltaTsolv = (T2 - T1), deltaTC = (T4 - T3), Tcalib = Tcalib))
}
#-------------------


# ------------------
# Experimental measurements
m_w <- 0.1          # Mass of water
t <- 120            # Time
U <- 9.6            # Voltage
I <- 1.37           # Current
c_w_sp <- 4182      # Specific heat capacity of water

# Function to calculate heat capacity of calorimeter
getCp <- function(slope) {
  return (U * I / slope)
}

# Function to calculate uncertainty of heat capacity of calorimeter
sHeatCapacity <- function(deltaT, sdeltaT) {
  return ((U * I * t / deltaT^2)^2 * sdeltaT^2)
}

# Function to calculate specific heat capacity of ethanol-water mixture
specificHeatCapacity <- function(slope, C.dewar, mass) {
  Cp <- U * I / slope
  return ((Cp - C.dewar) / mass)
}

# Function to calculate the standard error of the specific heat capcity
# using gaussian error propagation.
# See appendix for a derivation of the formula used.
error_prop_specificHeatCapacity <- function(U, I, m, slope_a, slope_b) {
  a <- slope_a$slope
  sa <- slope_a$slope_se
  b <- slope_b$slope
  sb <- slope_b$slope_se
  
  # See section in appendix for derivation of below formula
  return (U * I / m * sqrt(1/a**4 * sa**2 + 1/b**4 * sb**2))
}


# Function to calculate mass percentage given mass measurements of liquid
mass_percentage <- function(m) {
  return(100 * (m[2] - m[1]) / (m[3] - m[1]))
}

# Function to calculate solvation enthalpy
solvEnthalpy <- function(Mb, mb, deltaT) {
  return (-U * I * deltaT$Tcalib * Mb / mb * deltaT$deltaTsolv / deltaT$deltaTC)
}
# ------------------

# ------------------
get_intersection <- function(quadmodel, linmodel, rate) {
  # Generate a sequence of volume values
  V.c <- seq(from = 50, to = 400, by = 0.1)
  
  # Predictions for the linear model
  th1 <- predict(linmodel, newdata = list(x1 = V.c), interval = "confidence")
  
  # Predictions for the quadratic model
  th2 <- predict(quadmodel, newdata = list(x2 = V.c), interval = "confidence")
  
  # Interpolate the intersection point (= equivalence point)
  Vx <- approx(th1[, 1] - th2[, 1], V.c, 0)$y
  
  # Interpolate confidence intervals
  Vx1 <- approx(th1[, 2] - th2[, 2], V.c, 0)$y
  Vx2 <- approx(th1[, 3] - th2[, 3], V.c, 0)$y
  
  # Draw a dashed vertical line at the intersection point
  abline(v = Vx, lty = 2)
  abline(v = Vx, lty=2)
  abline(v = Vx, lty=2)
  
  # Calculate and display the result
  result <- Vx * rate
  return(list(Vx = Vx, Vx1 = Vx1, Vx2 = Vx2))
}
# ------------------


# ------------------
findEquivPoint <- function(Zeit, Temperatur) {
  p <- identify(Zeit,Temperatur, n=1, plot=FALSE)
  return (p[1])
}
# ------------------

# ------------------
timeToVolume <- function(time, rate, t_start) {
  return ((time - t_start) * rate)
}
# ------------------

#--------------------------
plotContTitration <- function(data, t, t_0) {
  Zeit <- data$zeit
  Temperatur <- data$temp
  par(cex.lab = 2.0, cex.axis = 2)
  plot(Zeit, Temperatur, type="l", lwd=2, col="grey65",
       xlab=expression(italic(t)~"/"~s),
       ylab=expression(theta~"/"~degree*"C")
  )
  
  # Add second x-axis
  top_labels <- c(0, 10, 20, 30, 40)
  top_pos <- (top_labels) * 18 / 4 + t_0
  axis(side = 3, at = top_pos, labels = top_labels)
  mtext(expression(Delta * "V / mL"), side = 3, line = 2.5, cex=2)
  
  t1 <- t[1]
  t2 <- t[2]
  t3 <- t[3]
  t4 <- t[4]
  
  idx2 <- which(Zeit > t1 & Zeit < t2)
  idx1 <- which(Zeit > t3 & Zeit < t4)
  
  # Create models
  x1 <- Zeit[idx1]
  y1 <- Temperatur[idx1]
  linmodel <- lm(y1 ~ I(x1))
  
  x2 <- Zeit[idx2]
  y2 <- Temperatur[idx2]
  quadmodel <- lm(y2 ~ I(x2) + I(x2^2))
  
  # Plot the first model
  x1 <- Zeit[(idx1[1]-10):(idx1[length(idx1)]+10)]
  x2 <- Zeit[(idx2[1]-10):(idx2[length(idx2)]+10)]
  
  # plot model curves and confidence intervall
  matlines(x1, predict(linmodel, list(x1 = x1), interval="confidence"), col = "black", lwd=c(2,1,1))
  matlines(x2, predict(quadmodel, list(x2 = x2), interval="confidence"), col = "black", lwd=c(2,1,1))
  
  # calculate intersection to obtain equivalent volume of titer
  deltat <- get_intersection(quadmodel, linmodel, 4/18)
  deltaV <- timeToVolume(deltat$Vx, 4/18, t_0)
  text(x = deltaV + 180, y = 21, label = substitute(Delta * V == value *mL, list(value = round(deltaV, 3))), cex=2.5)
  
  return (deltaV)
}
# ------------------

# ------------------
plotDiscTitration <- function(data, t, t_0) {
  # Extracting time and temperature from the data
  Zeit <- data$zeit
  Temperatur <- data$temp
  
  par(cex.lab = 2.0, cex.axis = 2)
  # Plot temperature line
  plot(Zeit, Temperatur, type = "l", lwd = 2, col = "black",
       xlim = c(), ylim = c(),
       xlab = expression(italic(t) ~ "/" ~ s),
       ylab = expression(theta ~ "/" ~ degree * "C"))
  
  # Plot selected points
  n <- 16
  Zeit.P <- 69.0 + seq(0, n) * 20.0
  Temp.P <- approx(Zeit, Temperatur, Zeit.P)$y
  points(Zeit.P, Temp.P, pch = 21, bg = "white", cex=1.5)
  
  idx2 <- which(Zeit.P > 0 & Zeit.P < 280)
  idx1 <- which(Zeit.P > 300 & Zeit.P < 400)
  
  # Create models
  x1 <- Zeit.P[idx1]
  y1 <- Temp.P[idx1]
  linmodel <- lm(y1 ~ I(x1))
  
  x2 <- Zeit.P[idx2]
  y2 <- Temp.P[idx2]
  quadmodel <- lm(y2 ~ I(x2) + I(x2^2))
  
  # Plot the first model
  x1 <- Zeit.P[(idx1[1]-3):(idx1[length(idx1)])]
  x2 <- Zeit.P[(idx2[1]):(idx2[length(idx2)]+1)]
  
  # plot model curves and confidence intervall
  lines(x1, predict(linmodel, list(x1 = x1)), col = "black", lwd=c(2,1,1))
  lines(x2, predict(quadmodel, list(x2 = x2)), col = "black", lwd=c(2,1,1))
  

  # Add top labels and axis
  top_labels <- seq(0, 100, 10)
  top_pos <- (top_labels) * 20 / 2 + t_0
  axis(side = 3, at = top_pos, labels = top_labels)
  mtext(expression(Delta * "V / mL"), side = 3, line = 2.5, cex=2)
  
  # Find equivalence point
  t_eq <- get_intersection(quadmodel, linmodel, 2/10)
  
  # Calculate volume change at equivalence point
  deltaV <- timeToVolume(t_eq$Vx, 2/20, t_0)
  
  # Draw line at equivalence point and add text
  text(x = t_eq$Vx + 75, y = 21.25, label = substitute(Delta * V == value * mL, list(value = round(deltaV, 3))), cex=2.5)
  
  return (deltaV)
}
# ------------------

# ------------------
titration_concentration <- function(V_eq, c_tit, V_init)
  return ((V_eq * c_tit) / V_init)

# ------------------
