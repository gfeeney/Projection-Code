metadata <- readRDS("metadata.rds")
path2inputs <- metadata$paths$path2inputs
path <- paste0(path2inputs, "ASBR/TFR_Pasex/")
TFRlist <- vector(mode = "list", length = length(filenames))
names(TFRlist) <- metadata$place$places
library(xlsx)
for (i in 1:length(pathfiles)) {
  pathfile <- paste0(path, filenames[i])
  x <- read.xlsx(pathfile, sheetName = "INPUT", startRow = 5, endRow = 34, colIndex = 3:4)
  TFRs <- x[, 2]
  names(TFRs) <- x[, 1]
  TFRlist[[i]] <- TFRs
}

pcycles <- metadata$time$pcycles
TFRmid <- function(x, pcycles) {
  lo <- x[c("2022", "2027", "2032", "2037", "2042")]
  hi <- x[c("2023", "2028", "2033", "2038", "2043")]
  TFR <- (lo + hi) / 2
  names(TFR) <- pcycles   
  return(TFR)
}

TFR <- do.call(rbind, lapply(TFRlist, TFRmid, pcycles))
rownames(TFR) <- metadata$place$placenames

filepath <- paste0(path2inputs, "ASBR/TFR.rds")
saveRDS(TFR, filepath)
readRDS(filepath)
