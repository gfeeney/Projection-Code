rm(list = ls())
metadata <- readRDS("metadata.rds")
path2inputs <- metadata$paths$path2inputs

path <- paste0(path2inputs, "nLx/")
pathfile <- paste0(path, "census2019-nLx-estimates.xlsx")
library(xlsx)
x <- read.xlsx(pathfile, sheetName = "main")
rownames(x) <- x[, 1]
y <- x[, -1]
n <- dim(y)[1] / 2
nLxf <- y[1:n, ]      # View(nLxf)
nLxm <- y[n + 1:n, ]  # View(nLxm)

nLxBase <- vector(mode = "list", length = dim(y)[2])
names(nLxBase) <- dimnames(y)[[2]]
for (j in 1:length(nLxBase)) {
  nLx <- matrix(c(nLxf[, j], nLxm[, j]), ncol = 2)
  rownames(nLx) <- substr(rownames(nLxf), start = 2, stop = nchar(rownames(nLxf)))
  colnames(nLx) <- c("Female", "Male")
  nLxBase[[j]] <- nLx
}
pathfile <- paste0(path, "nLxBase.rds")
saveRDS(nLxBase, pathfile)
readRDS(pathfile)
