rm(list = ls())
ATP <- readRDS("../projection-inputs/AgeTimePlace.rds")
ASDrows <- ATP$Age$ASDrows
BaseTime <- ATP$Time$Base
source("../projections2/R/projection.R")
pfmatrix0 <- readRDS("../projections2/kenya0/pframe-matrix0.rds")

ASDlist <- lapply(pfmatrix0, asd.from.ppframes, ASDrows, BaseTime)
Totals <- do.call(rbind, lapply(ASDlist, function(x){apply(x, 2, sum)}))
Totals <- round(Totals / 1000,  0)
Totals
CountyTotals <- Totals[-1, ]
CountyTotals <- CountyTotals[order(CountyTotals[, 1]), ]
CountyTotalsLog2 <- round(log2(CountyTotals), 1)
is.matrix(CountyTotalsLog2)
max(CountyTotalsLog2)

catvalues2020 <- CountyTotalsLog2[, 1]
catvalues2025 <- CountyTotalsLog2[, 2]
catvalues2030 <- CountyTotalsLog2[, 3]
catvalues2035 <- CountyTotalsLog2[, 4]
catvalues2040 <- CountyTotalsLog2[, 5]
catvalues2045 <- CountyTotalsLog2[, 6]
xlab <- "County Population (log base 2)"
xlim <- c(7, 13)
xatv <- 7:13

par(mai=c(0.6, 1.3, 0.6, 0.2))
draw.dotplot <- function(xlim, xlab, xatv, catvalues) {
  # xvalues must be named with category names!
  ylim <- c(0, 1 + length(catvalues))
  plot(xlim, ylim, type="n", xlab="", xaxt="n", xaxs="i", ylab="", yaxt="n", yaxs="i")
  axis(1, at = xatv)
  mtext(xlab, side = 1, line = 2.5)
  axis(3, at = xatv)
  mtext(xlab, side = 3, line = 2.5)
  for (i in 2:(length(xatv) - 1)) {
    lines(c(xatv[i], xatv[i]), ylim, lwd = 0.25, lty = "solid", col = gray(0.40))
  }
  
  axis(2, at = 1:length(catvalues), labels = names(catvalues), tick = FALSE, las = 2, line = -0.5, cex = 0.5)
  for (i in 1:length(catvalues)) {
    lines(xlim, c(i,i), lwd = 0.75, lty = "dashed", col = gray(0.25))
  }
  
  points(catvalues, 1:length(catvalues), pch = 16, cex = 1.5)
}

par(mai=c(0.6, 1.3, 0.6, 0.2))
draw.dotplot(xlim, xlab, xatv, catvalues)
points(catvalues[28], 28, pch = 15, cex = 2.0)

County2045Totals <- Totals[-1, 6]
catvalues2045 <- CountyBaseTotals[order(County2045Totals)]
catvalues2045 <- round(log2(catvalues2045), 1)
xlab <- "County 2045 Population (log base 2)"

pdf(file="CountyBaseTotalsLog2.pdf", paper="a4", width=7.27, height=10.69, pointsize=9)
par(mai=c(0.6, 1.3, 0.6, 0.2))
draw.dotplot(xlim, xlab, xatv, catvalues)

dev.off()

pdf(file="CountyBaseTotalsLog2.pdf", paper="a4", width=7.27, height=10.69, pointsize=9)
par(mai=c(0.6, 1.3, 0.6, 0.2))
draw.dotplot(xlim, xlab, xatv, catvalues2020)
points(catvalues2045, 1:length(catvalues2045), pch = 1, cex = 1.5)
dev.off()
