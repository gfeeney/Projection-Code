metadata <- readRDS("metadata.rds")
places <- metadata$place$places
pcycles <- metadata$time$pcycles
path2inputs <- metadata$paths$path2inputs
path2R <- metadata$paths$path2R
source(paste0(path2R, "projection.R"))
library(xlsx)

# INPUT FILES
# paste0(path2inputs, "/nLx/LEB/") (1 file for each place)
# UPDATED 27-SEP-2021 PER SCHOLA
# paste0(path2inputs, "nLx/census2019-nLx-estimates.xlsx")
# paste0(path2inputs, "nLx/limit-model.xlsx")

# OUTPUT FILES
# STEP 1: paste0(path2inputs, "nLx/LEBlist.rds")
# STEP 2: paste0(path2inputs, "nLx/nLxCensusList.rds")
# STEP 3: paste0(path2inputs, "nLx/nLxLimit.rds")
# STEP 4: paste0(path2inputs, "nLx/nLx.rds")  # THIS IS THE OBJECTIVE!

# QUESTION Has Schola redone the extrapolated LEBs for each place and pcycle
# based on the final 2019 census nLx values? I don't have these spreadsheets,
# only an LEBlist.rds file. that is why the code below is commented out.

# STEP 1 STEP 1 STEP 1 STEP 1 STEP 1 STEP 1 STEP 1 STEP 1 STEP 1 STEP 1 STEP 1
# Read in extrapolated Life Expectancy at Birth for each place and pcycle
# OUTPUT LEBlist
# path <- paste0(path2inputs, "nLx/LEB/")
# filenames <- dir(path)
# LEB <- vector(mode = "list", length = length(filenames))
# names(LEB) <- places
# for (i in 1:length(LEB)) {
#   filename <- paste0(path, filenames[i])
#   x <- read.xlsx(filename, sheetName = "E0LGST", startRow = 10, endRow = 40, 
#                  colIndex = 6:8)
#   colnames(x) <- c("Year", "Male", "Female")
#   rownames(x) <- x[, "Year"]
#   x <- x[, -1]
#   x[, c("Female", "Male")]
#   LEB[[i]] <- as.matrix(t(x[, c("Female", "Male")]))
# }
# rm(x)
# 
# LEBmid <- function(LEB, pcycles) {
#   lo <- LEB[, c("2022", "2027", "2032", "2037", "2042")]
#   hi <- LEB[, c("2023", "2028", "2033", "2038", "2043")]
#   mid <- (lo + hi) / 2
#   colnames(mid) <- pcycles   
#   return(mid)
# }
# LEBlist <- lapply(LEB, LEBmid, pcycles)
# LEBlist
# saveRDS(LEBlist, paste0(path2inputs, "nLx/LEBlist.rds"))

LEBlist <- readRDS(paste0(path2inputs, "nLx/LEBlist.rds"))
# LEBlist is list of 2x5 matrices (Sex x pcycle) for each place, Kenya and the
# 47 counties

# STEP 2 STEP 2 STEP 2 STEP 2 STEP 2 STEP 2 STEP 2 STEP 2 STEP 2 STEP 2 STEP 2
# Read in 2019 census nLx estimates
# OUTPUT nLxCensusList
pathfile <- paste0(path2inputs, "nLx/census2019-nLx-estimates.xlsx")
x <- read.xlsx(pathfile, sheetName = "main")
rownames(x) <- x[, 1]
nLxDF <- x[, -1]
rm(x)
nLxDF
# View(nLxDF)
# WARNING!!! Column names are not standard! Should be rectified in source file

nLxCensusList <- vector(mode = "list", length = dim(nLxDF)[2])
names(nLxCensusList) <- colnames(nLxDF)
n <- dim(nLxDF)[1] / 2
for (j in 1:length(nLxCensusList)) {
  x <- matrix(c(nLxDF[1:n, j], nLxDF[n + 1:n, j]), ncol = 2)
  rownames(x) <- rownames(nLxDF)[1:n]
  colnames(x) <- c("Female", "Male")
  nLxCensusList[[j]] <- x
}
rm(x)
nLxCensusList
saveRDS(nLxCensusList, paste0(path2inputs, "nLx/nLxCensusList.rds"))

# STEP 3 STEP 3 STEP 3 STEP 3 STEP 3 STEP 3 STEP 3 STEP 3 STEP 3 STEP 3 STEP 3
# Read in nLx Limit values
pathfile <- paste0(path2inputs, "nLx/Method/limit-model.xlsx")
x <- read.xlsx(pathfile, sheetName = "limit-model", 
               startRow = 1, endRow = 22, colIndex = c(1,3:6))
rownames(x) <- as.character(x[, 1])
x <- x[, -1]
x <- x[, c("nLxf1", "nLxm1")]
colnames(x) <- c("Female", "Male")
nLxLimit <- x
rm(x)
nLxLimit
saveRDS(nLxLimit, paste0(path2inputs, "nLx/nLxLimit.rds"))
apply(nLxLimit, 2, sum)  # Limit nLx values for females and males
# write.csv(nLxLimit, paste0(path2inputs, "nLx/nLxLimit.csv"))

# STEP 4 STEP 4 STEP 4 STEP 4 STEP 4 STEP 4 STEP 4 STEP 4 STEP 4 STEP 4 STEP 4
# Calculate nLx list input to projection function by interpolation between 
# Census nLx and Limit nLx
interpolate.nLx <- function(LEB, nLx0, nLx1) {
  # LEB <- LEBlist[[1]]["Female", pcycles[1]]
  # nLx0 <- nLxCensusList[[1]][, "Female"]
  # nLx1 <- nLxLimit[, "Female"]
  LEB0 <- sum(nLx0)
  LEB1 <- sum(nLx1)
  p <- (LEB - LEB0) / (LEB1 - LEB0)
  nLx <- nLx0 + p * (nLx1 - nLx0)
  if (abs(LEB - sum(nLx)) > 0.0000001) {
    stop("abs(LEB - sum(nLx)) > 0.0000001)")
  }
  return(nLx)
}

AG <- getAG(5, 100)
ASG <- c(paste0("f", AG), paste0("m", AG))
M <- matrix(0, nrow = length(ASG), ncol = length(pcycles))
rownames(M) <- ASG
colnames(M) <- pcycles
n <- dim(M)[1] / 2
nLx <- lapply(LEBlist, function(x){M})
for (i in 1:length(nLx)) {
  LEBf <- LEBlist[[i]]["Female", ]
  nLx0f <- nLxCensusList[[i]][, "Female"]
  nLx1f <- nLxLimit[, "Female"]  
  LEBm <- LEBlist[[i]]["Male", ]
  nLx0m <- nLxCensusList[[i]][, "Male"]
  nLx1m <- nLxLimit[, "Male"]
  for (j in 1:length(pcycles)) {
    nLx[[i]][1:n, j]     <- interpolate.nLx(LEBf[j], nLx0f, nLx1f)
    nLx[[i]][n + 1:n, j] <- interpolate.nLx(LEBm[j], nLx0m, nLx1m)
  }
}
  
nLx <- lapply(nLx, round, 6)
nLx
saveRDS(nLx, paste0(path2inputs, "nLx/nLx.rds"))

