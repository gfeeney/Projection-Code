rm(list = ls())
metadata <- readRDS("metadata.rds")
path2inputs <- metadata$paths$path2inputs
path2R <- metadata$paths$path2R
subareas <- metadata$place$subareas
source("metadata.R")
library(xlsx)

# IMPORT AGE-SEX COHORT AGGREGATED 1 YEAR AGO CENSUS DATA FOR COUNTIES
path <- paste0(path2inputs, "NIMR/NIMRfiles/")
files <- dir(path)
m <- matrix(0, nrow = 47, ncol = 2)
rownames(m) <- subareas
colnames(m) <- c("Female", "Male")
for (i in 1:length(subareas)) {
  pathfile <- paste0(path, files[i])
  female <- unlist(read.xlsx(pathfile, sheetName = "main", startRow = 4, endRow = 24, colIndex = 34))
  male   <- unlist(read.xlsx(pathfile, sheetName = "main", startRow = 107, endRow = 127, colIndex = 34))
  m[i, ] <- c(sum(female), sum(male))
}
Res1YrAgoCountySex <- m
round(Res1YrAgoCountySex, 0)

# IMPORT LTCSRMIG DATA FOR COUNTIES
path <- paste0(path2inputs, "NIMR/LTCSRMIGfiles/")
files <- dir(path)
LTCSRMig <- vector(mode = "list", length = length(files))
names(LTCSRMig) <- subareas
for (i in 1:length(files)) {
  pathfile <- paste0(path, files[i])
  sl <- vector(mode = "list", length = 3)  # sublist
  names(sl) <- c("Forward", "Backward", "Average")
  
  # Forward Survival
  x <- read.xlsx(pathfile, sheetName = "OUTPUT", startRow = 10, endRow = 27, colIndex = c(3:4, 6:7, 9:10))
  rownames(x) <- getAG(5, 80)
  colnames(x) <- c("CSR-Male", "CSR-Female", "LT-Male", "LT-Female", "Cmp-Male", "Cmp-Female")
  sl[[1]] <- x
  
  # Reverse Survival
  x <- read.xlsx(pathfile, sheetName = "OUTPUT", startRow = 31, endRow = 48, colIndex = c(3:4, 6:7, 9:10))
  rownames(x) <- getAG(5, 80)
  colnames(x) <- c("CSR-Male", "CSR-Female", "LT-Male", "LT-Female", "Cmp-Male", "Cmp-Female")
  sl[[2]] <- x
  
  # Average
  x <- read.xlsx(pathfile, sheetName = "OUTPUT", startRow = 52, endRow = 69, colIndex = c(3:4, 6:7, 9:10))
  rownames(x) <- getAG(5, 80)
  colnames(x) <- c("CSR-Male", "CSR-Female", "LT-Male", "LT-Female", "Cmp-Male", "Cmp-Female")
  sl[[3]] <- x
  
  LTCSRMig[[i]] <- sl
}
LTCSRMig
LTCSRMigCountySex <- t(sapply(LTCSRMig, function(x){apply(x[[3]][, 5:6], 2, sum)}))
colnames(LTCSRMigCountySex) <- c("Male", "Female")
LTCSRMigCountySex <- LTCSRMigCountySex[, 2:1]
round(LTCSRMigCountySex, 0)


# Adjustment Factors
# LTCSRMigCountySex gives annual number of migrants from LTCSRMig.xls
# Res1YrAgoCountySex gives 5 year number of migrants from residence 1 year ago data

round(Res1YrAgoCountySex, 0)
round(LTCSRMigCountySex, 0)
round(5 * LTCSRMigCountySex / Res1YrAgoCountySex, 3)

apply(LTCSRMigCountySex, 2, sum)
apply(Res1YrAgoCountySex, 2, sum)
