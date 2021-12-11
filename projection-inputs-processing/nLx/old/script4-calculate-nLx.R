rm(list = ls())
metadata <- readRDS("metadata.rds")
path2inputs <- metadata$paths$path2inputs

path <- paste0(path2inputs, "nLx/")
LEBplist <- readRDS(paste0(path, "LEB.rds"))
nLxBase.plist    <- readRDS(paste0(path, "nLxBase.rds"))
nLxLimit         <- readRDS(paste0(path, "nLxLimit.rds"))
LEBpcycles       <- LEBplist[[1]]         # For testing
nLxBase          <- nLxBase.plist[[1]]     # For testing

calculate.nLxTable <- function(LEBpcycles, nLxBase, nLxLimit) {
  interpolate.nLx <- function(LEB, nLx0, nLx1) {
    LEB0 <- sum(nLx0)
    LEB1 <- sum(nLx1)
    for (i in 1:length(LEB)) {
      p <- (LEB[i] - LEB0) / (LEB1 - LEB0)
      nLx <- nLx0 + p * (nLx1 - nLx0)
    }
    return(nLx)
  }
  nLxTablef <- matrix(0, nrow = dim(nLxLimit)[1], ncol = dim(LEBpcycles)[2])
  rownames(nLxTablef) <- rownames(nLxLimit)
  colnames(nLxTablef) <- colnames(LEBpcycles)
  nLxTablem <- nLxTablef
  for (j in 1:dim(nLxTablef)[2]) {
    nLxTablef[, j] <- interpolate.nLx(LEBpcycles["Female",], nLxBase[, "Female"], nLxLimit[, "Female"])
    nLxTablem[, j] <- interpolate.nLx(LEBpcycles["Male",  ], nLxBase[,   "Male"], nLxLimit[,   "Male"])
  }
  nLxTable <- rbind(nLxTablef, nLxTablem)
  rownames(nLxTable) <- c(paste0("f", rownames(nLxTablef)), paste0("m", rownames(nLxTablem)))
  return(nLxTable)
}
# calculate.nLxTable(LEBpcycles, nLxBase, nLxLimit)
nLx <- vector(mode = "list", length = length(LEBplist))
names(nLx) <- names(LEBplist)
for (i in 1:length(nLx)) {
  nLx[[i]] <- calculate.nLxTable(LEBplist[[i]], nLxBase.plist[[i]], nLxLimit)
}
pathfile <- paste0(path, "nLx.rds")
saveRDS(nLx, pathfile)
readRDS(pathfile)
