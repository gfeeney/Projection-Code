linear.5to1 <- function(series, period) {
  # Linear interpolation on midpoints
  xin <- 5 * 1:length(series) - 2.5
  yin <- series
  xout <- 1:(5*length(series)) - 0.5
  yout <- approx(xin, yin, xout)
  changeleft <- yout$y[4] - yout$y[3]
  yout$y[1:2] <- yout$y[3] - 2:1 * changeleft
  n <- length(yout$y)
  changeright <- yout$y[n - 3] - yout$y[n - 4]
  yout$y[(n - 1): n] <- yout$y[n - 2] + 1:2 * changeright
  return(yout)
}
rates  # Mombase female age 0-4
out <- linear.5to1(series = rates, period = 5)
plot(out$x, out$y, pch = 16)
points(5 * 1:14 - 2.5, rates, cex = 2)
sum(out$y)      # 5.132580
sum(5 * rates)  # 5.124049

moving.average <- function(series, period) {
  #stop(floor(period / 2) == period / 2, "period must be odd number")
  nlead <- (period - 1) / 2
  series.out <- series
  first <- (period + 1) / 2
  last  <- length(series) - (period + 1) / 2
  offsets <- c(-(1:nlead), 0, 1:nlead)
  for (i in first:last) {
    series.out[i] <- mean(series[i + offsets])
  }
  return(series.out)
}
y <- moving.average(series= out$y, period = 5)
plot(out$x, y, pch = 16)
lines(out$x, y)
points(5 * 1:14 - 2.5, rates, cex = 2)

sum(rates)  # 1.02481
sum(out$y)
x
smooth.series <- function(series.in, period, plotting = TRUE) {
  y <- moving.average(out$y, 5)
  # y[25:length(y)] <- moving.average(y[25:length(y)], 7)
  # y[35:length(y)] <- moving.average(y[35:length(y)], 11)
  return(y)
}

ad5 <- BaseASD[1:(dim(BaseASD)[1]/2), 1]
ad1 <- linear.5to1(ad5, period)
x <- ad1$x
y <- ad1$y
y <- smooth.ad1(y)
plot(x, y, pch = 16)
points(xin, yin, cex = 2)



