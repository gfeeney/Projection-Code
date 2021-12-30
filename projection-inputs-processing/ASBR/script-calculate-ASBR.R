metadata <- readRDS("metadata.rds")
path2inputs <- metadata$paths$path2inputs
places <- metadata$place$places
library(xlsx)

# INPUT FILES
# paste0(path2inputs, "ASBR/TFR_Pasex/") (1 file for each place)
# paste0(path2inputs, "/ASBR/Census2019ASBRestimates.xlsx")
# paste0(path2inputs, "ASBR/20210927-SexRatioatBirth2019.csv")

# OUTPUT FILES
# paste0(path2inputs, "ASBR/TFR.rds")
# paste0(path2inputs, "ASBR/CensusASBRs.rds")
# paste0(path2inputs, "ASBR/ASBR.rds")  # THIS IS THE OBJECTIVE!

# STEP 1 Import extrapolated TFRs from .xls files
path <- paste0(path2inputs, "ASBR/source/TFR_Pasex/")
filenames <- dir(path)
filenames
TFRlist <- vector(mode = "list", length = length(filenames))
names(TFRlist) <- metadata$place$places
for (i in 1:length(filenames)) {
  pathfile <- paste0(path, filenames[i])
  x <- read.xlsx(pathfile, sheetName = "INPUT", startRow = 5, endRow = 34, colIndex = 3:4)
  TFRs <- x[, 2]
  names(TFRs) <- x[, 1]
  TFRlist[[i]] <- TFRs
  i <- i + 1
}
TFRlist

pcycles <- metadata$time$pcycles
TFRmid <- function(x, pcycles) {
  lo <- x[c("2022", "2027", "2032", "2037", "2042")]
  hi <- x[c("2023", "2028", "2033", "2038", "2043")]
  TFR <- (lo + hi) / 2
  names(TFR) <- pcycles   
  return(TFR)
}
TFR <- do.call(rbind, lapply(TFRlist, TFRmid, pcycles))
rownames(TFR) <- metadata$place$places
TFR
round(TFR, 2)
fivenum(round(TFR[, 1], 2))
fivenum(round(TFR[, 5], 2))

# STEP 2 Read in 2019 census ASBR estimates
pathfile <- paste0(path2inputs, "ASBR/source/Census2019ASBRestimates.xlsx")
x <- read.xlsx(pathfile, sheetName = "main", colIndex = 1:8)
rownames(x) <- as.character(x[, 1])
x <- as.matrix(x[, -1])
colnames(x) <- c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49")
CensusASBRs <- x
CensusASBRs
CensusTFRs <- 5 * apply(CensusASBRs, 1, sum)
CensusTFRs
fivenum(CensusTFRs)
sort(CensusTFRs)
# TFR of 8 children/woman? Really? Wildly implausible. Note however that the
# TFRs used for projection come from STEP 1 above. Unclear whether these 
# inputs reflect these high TFRs ()

# STEP 3 Calculate ASBR projection input list from TFR and CensusASBRs
# SRB <- rep(1.03, times = 48)  # Be sure this is what we want
# Following data received 27-Sep-2021 from Paul Waweru Ngogi via Telegram msg
pathfile <- paste0(path2inputs, "ASBR/source/20210927-SexRatioatBirth2019.xlsx")
# Are you sure you want to go with these sex ratios at birth?
x <- read.xlsx(pathfile, sheetName = "main", colIndex = 1:2)
SRB <- x[, 2]
places <- metadata$place$places
names(SRB) <- places
SRB
ASBRn <- CensusASBRs  # Normalized ASBRs
ASBRn[ , ] <- 0
for (i in 1:dim(CensusASBRs)[1]) {
  ASBRn[i, ] <- CensusASBRs[i, ] * 0.2 / sum(CensusASBRs[i, ])
}

places <- metadata$place$places
ASBR <- vector(mode = "list", length = length(places))
names(ASBR) <- places
for (i in 1:length(places)) {
  ASBRi <- outer(ASBRn[i, ], TFR[i, ], FUN = "*")
  SRBi <- rep(SRB[i], length = dim(ASBRi)[2])
  ASBRi <- rbind(SRBi, ASBRi)
  rownames(ASBRi)[1] <- "SRB"
  colnames(ASBRi) <- pcycles
  ASBR[[i]] <- ASBRi
}
ASBR <- lapply(ASBR, round, 4)
ASBR
