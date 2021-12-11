metadata <- readRDS("metadata.rds")
path2inputs <- metadata$paths$path2inputs
library(xlsx)

# STEP 1
# Read in 2019 census ASBR estimates
pathfile <- paste0(path2inputs, "/ASBR/Census2019ASBRestimates.xlsx")
x <- read.xlsx(pathfile, sheetName = "main", colIndex = 1:8)
rownames(x) <- as.character(x[, 1])
x <- as.matrix(x[, -1])
colnames(x) <- c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49")
CensusASBRs <- x
CensusASBRs


# STEP 2
# 
