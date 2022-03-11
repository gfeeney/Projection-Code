school <- readRDS("../projection-outputs-data/annex-tables3/school.rds")
j <- 1  # Year 2020, 2021, . . . 2035
i <- 1  # 00Kenya, . . . 48Nairobi City
# Five year age distributions from which school ages are interpolated
y1 <- school[[i]][1:10, j] / 5
x1 <- 5 * 1:10 - 2.5
plot(x1, y1)
lines(x1, y1)  # This plots the input 5 year age distribution for area i and year j
filenames <- paste0(md$place$codes, names(school), ".csv")
pathfile <- paste0("../projection-outputs-data/annex-tables3/", filenames)
y2 <- read.csv(pathfile[i])[j, 2:4] / c(3, 8, 4)
x2 <- c(4.5, 10, 16)
points(x2, y2, pch = 18)
lines(x2, y2, col = "red")
text(mean(x1), mean(y1), paste0(md$place$codes[i], names(school)[i]))
i <- i + 1

# Use this to plot the input 5 year age distribution and the interpolated school 
# age distributions together. In each case, the average number in the age group 
# plotted at the midpoint of the age group. There should be reasonable coincidence
# between the two plots. I've looked at all of them, no reason to doubt the
# interpolated school age numbers.