rm(list = ls())
metadata <- readRDS("metadata.rds")
path2inputs <- metadata$paths$path2inputs

path <- paste0(path2inputs, "/nLx/LEB/")
filenames <- dir(path)
filenames
LEB <- vector(mode = "list", length = length(filenames))
area <- metadata$place$area
subareas <- metadata$place$subareas
names(LEB) <- c(area, subareas)

library(xlsx)
for (i in 1:length(LEB)) {
  filename <- paste0(path, filenames[i])
  x <- read.xlsx(filename, sheetName = "E0LGST", startRow = 10, endRow = 40, colIndex = 6:8)
  colnames(x) <- c("Year", "Male", "Female")
  rownames(x) <- x[, "Year"]
  x <- x[, -1]
  x[, c("Female", "Male")]
  LEB[[i]] <- as.matrix(t(x[, c("Female", "Male")]))
}

LEBmid <- function(LEB, pcycles) {
  lo <- LEB[, c("2022", "2027", "2032", "2037", "2042")]
  hi <- LEB[, c("2023", "2028", "2033", "2038", "2043")]
  mid <- (lo + hi) / 2
  colnames(mid) <- pcycles   
  return(mid)
}
pcycles <- metadata$time$pcycles
LEB <- lapply(LEB, LEBmid, pcycles)
pathfile <- paste0(path2inputs, "nLx/LEB.rds")
saveRDS(LEB, pathfile)
readRDS(pathfile)
