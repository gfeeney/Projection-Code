rm(list = ls())
metadata <- readRDS("metadata.rds")
path2inputs <- metadata$paths$path2inputs

path <- paste0(path2inputs, "nLx/")
pathfile <- paste0(path, "limit-model.xlsx")
library(xlsx)
x <- read.xlsx(pathfile, sheetName = "limit-model", 
               startRow = 1, endRow = 22, colIndex = c(1,3:6))
rownames(x) <- as.character(x[, 1])
y <- x[, -1]
z <- y[, c("nLxf1", "nLxm1")]
colnames(z) <- c("Female", "Male")

pathfile <- paste(path, "nLxLimit.rds")
saveRDS(z, pathfile)
readRDS(pathfile)
