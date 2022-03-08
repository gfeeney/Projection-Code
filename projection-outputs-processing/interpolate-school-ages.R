interpolate.school.ages <- function(ad5.in) {
  # ad5.in = numbers of persons in 0-4, 5-9, . . . age groups to at least age 15-19
  y <- ad5.in[1:4] / 5
  x <- 5 * 1:4 - 2.5
  xout <- 1:20 - 0.5
  yout <- approx(x, y, xout)
  changeleft <- yout$y[4] - yout$y[3]
  yout$y[1:2] <- yout$y[3] - 2:1 * changeleft
  n <- length(yout$y)
  changeright <- yout$y[n - 3] - yout$y[n - 4]
  yout$y[(n - 1): n] <- yout$y[n - 2] + 1:2 * changeright
  plot(yout$x, yout$y)
  sch.age <- c(sum(yout$y[1 + 3:5]), sum(yout$y[1 + 6:13]), sum(yout$y[1 + 14:17]))
  names(sch.age) <- c("Age 3-5", "Age 6-13", "Age 14-17")
  return(sch.age)
}
