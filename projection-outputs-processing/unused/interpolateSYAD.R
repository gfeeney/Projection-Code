# Linear interpolation on 5 year groups 0-4, 5-9, 10-14, 15-19
# to get numbers at single years of age 0, 1, ..., 19

# Develop code to interpolate to single years of age (linear) and
# calculate numbers in school age groups 3-5, 6-13, snf 14-17. The
# approx() function does all the work of interpolation, but it 
# doesn't extrapolate
x <- 5 * 1:4 - 2.5
y <- c(298548, 308445, 313615, 259991)
xout <- 1:20 - 0.5
yout <- approx(x, y, xout)
# approx() doesn't extrapolate to the end points, so we need to write
# code to do this, unfortunately
changeleft <- yout$y[4] - yout$y[3]
yout$y[1:2] <- yout$y[3] - 2:1 * changeleft
n <- length(yout$y)
changeright <- yout$y[n - 3] - yout$y[n - 4]
yout$y[(n - 1): n] <- yout$y[n - 2] + 1:2 * changeright
plot(yout$x, yout$y)
sch.age <- c(sum(yout$y[1 + 3:5]), sum(yout$y[1 + 6:13]), sum(yout$y[1 + 14:17]))
names(sch.age) <- c("3-5", "6-13", "14-17")

# Now use this code to define a function to calculate the school age numbers;
# write it to accept a 5 year age group distribution of any length > 4
interpolate.school.ages <- function(ad5.in) {
  # ad5.in = numbers of persons in 0-4, 5-9, . . . age groups to at least age 15-19
  y <- ad5.in[1:4]
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
  names(sch.age) <- c("3-5", "6-13", "14-17")
  return(sch.age)
}

# Now we need to pull in age-sex distributions for all 47 counties and
# calculate the age distribution for both sexes
ad.in <- read.csv("../../projections/kenya1/inputs/BaseASD.csv")
rownames(ad.in) <- ad.in[, 1]
ad.in <- ad.in[, -1]
# ad.in <- as.matrix(ad.in)
View(ad.in)
source("../../projections/R/projection-utilities.R")
fAG <- paste0("f", getAG(5, 95))
mAG <- paste0("m", getAG(5, 95))
ad.both <- matrix(0, nrow=dim(ad.in)[1]/2, ncol = dim(ad.in)[2])
ad.both <- ad.in[fAG, ] + ad.in[mAG, ]
rownames(ad.both) <- getAG(5, 95)
# At this point we've got the inputs we need to calculate school age population
# for Kenya and the 47 countries

# We use a loop to apply interpolate.school.ages() to every column of
# ad.both. But first we set up a data frame to receive the results
sch.age <- ad.both[1:3, ]
rownames(sch.age) <- c("3-5", "6-13", "14-17")
sch.age[ , ] <- 0
for (i in 1:dim(sch.age)[2]) {
  sch.age[, i] <- interpolate.school.ages(ad.both[, i])
}
round(sch.age, 0)

