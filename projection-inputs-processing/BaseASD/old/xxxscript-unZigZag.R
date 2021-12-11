rm(list = ls())
library(xlsx)
dir("../projection-inputs/BaseASD/")
x <- read.xlsx("../projection-inputs/BaseASD/unZigZag.xlsx", 
               sheetIndex = 1, 
               startRow = 8, endRow = 49, colIndex = 1:4)
x
adf <- x[1:20, 4]
names(adf) <- x[1:20, 2]
adf
adm <- x[21:40, 4]
names(adm) <- x[1:20, 2]
adm

x <- adf
i <- 7 + 2 * 0:6
plotout <- TRUE
unZigZag <- function(x, i = 7 + 2 * 0:6, plotout = FALSE) {
  # x - age distribution, five year groups for index min(i) and above
  # i indexes of age groups beginning with ages ending in '0' (e.g.
  # 30-34, 40-44, . . . ) from which some fraction is to be distributed
  # to surrounding age groups
  out <- matrix(0, nrow = length(x), ncol = 3)
  rownames(out) <- names(x)
  colnames(out) <- c("x", "xfer", "y")
  out[, "x"] <- x
  out[, "xfer"] <- NA
  xfer0 <- rep(0, times = length(i))
  out[i, "xfer"] <- xfer0
  adjustx <- function(x, i, xfer) {
    y <- x
    y[i] <- x[i] - xfer
    y[i - 1] <- x[i - 1] + xfer / 2
    y[i + 1] <- x[i + 1] + xfer /2
    return(y)
  }  # out[, "y"] <- adjustx(x, i, xfer0)
  yrough <- function(y, i) {
    y[i] - (y[i - 1] + y[i + 1])/2
  }
  
  rough <- function(yrough) {
    sum(yrough^2)
  }
  xfer <- optim(xfer0, rough)$par
  out[i, "xfer"] <- round(xfer, 0)
  out[, "y"] <- round(adjustx(x, i, xfer), 0)
  if (plotout == TRUE) {
    plot.unZigZag <- function(uZZ) {
      x <- 5 * 1:20 - 2.5
      y1 <- uZZ[, "x"]
      y2 <- uZZ[, "y"]
      plot(x, y1, cex = 2)
      lines(x, y1)
      points(x, y2, pch = 16)
      lines(x, y2)
    }
  plot.unZigZag(uZZ)
  }
  out
}

unZigZag(x, i = 7 + 2 * 0:6, plotout = FALSE) 
