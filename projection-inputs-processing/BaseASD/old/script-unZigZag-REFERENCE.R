unZigZag.REF1 <- read.xlsx("../projection-inputs-data/BaseASD/unZigZag-REF2.xlsx", sheetIndex = 1,
                           stringsAsFactors = FALSE)
source("R/projection.R")
rownames(unZigZag.REF1) <- getAG(5, 95)
unZigZag.REF1

x <- unZigZag.REF1[, "x"]
i <- 7 + 2 * 0:6
out <- unZigZag.ad5(x, i)

plot.unZigZag(out)

















BaseASD <- readRDS("../projection-inputs-data/BaseASD/BaseASD.rds")

unZigZab.BaseASD <- function(BaseASD, i) {
  out <- BaseASD
  out[ , ] <- 0
  for (j in 1:dim(BaseASD)[2]) {
    out[1:20, j] <- unZigZag(BaseASD[1:20, j], i)[, "y"]
    out[21:40, j] <- unZigZag(BaseASD[21:40, j], i)[, "y"]
  }
  return(out)
}
BaseASD.unZigZagged <- adjustBaseASD(BaseASD, i)
View(BaseASD.unZigZagged)

# The following does not work, error opening pdf
pdf("../projection-inputs/BaseASD/unZigZagPlots.pdf", onefile = TRUE)
BaseASD.unZigZagged <- unZigZag.BaseASD(BaseASD, i)
dev.off()




outlist <- list()

# female loop
for (j in 1:dim(BaseASD)[2]) {
  x <- unZigZag(BaseASD[1:20, j], i)
  BaseASD[1:20, j] <- x[, "y"]
  outlist[[length(outlist) + 1]] <- x
}

# male loop
for (j in 1:dim(BaseASD)[2]) {
  x <- unZigZag(BaseASD[21:40, j], i)
  BaseASD[21:40, j] <- x[, "y"]
  outlist[[length(outlist) + 1]] <- x
}

names(outlist) <- c(colnames(BaseASD), colnames(BaseASD))
outlist

ASDadj <- do.call(cbind, lapply(outlist, function(x){x[, "y"]}))
rownames(ASDadj) <- rownames(BaseASD)
        