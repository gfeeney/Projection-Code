metadata <- readRDS("metadata.rds")
path2inputs <- metadata$paths$path2inputs
path2R <- metadata$paths$path2R
source(paste0(path2R, "projection.R"))

BaseASD1 <- readRDS(paste0(path2inputs, "BaseASD/BaseASD1.rds"))
# View(BaseASD1)
BaseASD2 <- BaseASD1
BaseASD2[ , ] <- 0
# View(BaseASD2)

unZigZag.BaseASD <- function(BaseASD, i) {
  out <- BaseASD
  out[ , ] <- 0
  for (j in 1:dim(BaseASD)[2]) {
    out[1:20, j] <- unZigZag.ad5(BaseASD[1:20, j], i)[, "y"]
    out[21:40, j] <- unZigZag.ad5(BaseASD[21:40, j], i)[, "y"]
  }
  return(out)
}
i <- 7 + 2 * 0:6
BaseASD2 <- unZigZag.BaseASD(BaseASD1, i)
pathfile <- paste0(path2inputs, "BaseASD/BaseASD2.rds")
saveRDS(BaseASD2, pathfile)
# View(readRDS(pathfile))

plot.unZigZag <- function(out) {
  x <- 5 * 1:20 - 2.5
  y1 <- out[, "x"]
  y2 <- out[, "y"]
  plot(x, y1, cex = 2)
  lines(x, y1)
  points(x, y2, pch = 16)
  lines(x, y2)
}

pathfile <- paste0(path2inputs, "BaseASD/unZigZagPlots.pdf")
pdf(file = pathfile, onefile = TRUE)
for (j in 1:dim(BaseASD)[2]) {
  place <- colnames(BaseASD)[j]
  sex <- "Female"
  out <- unZigZag.ad5(BaseASD[1:20, j], i)
  plot.unZigZag(out)
  text(0, paste(place, sex), adj = 0)
  sex <- "Male"
  out <- unZigZag.ad5(BaseASD[21:40, j], i)
  plot.unZigZag(out)
  text(0, paste(place, sex), adj = 0)
}
dev.off()
