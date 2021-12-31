metadata <- readRDS("metadata.rds")
path2inputs <- metadata$paths$path2inputs
path2outputs <- metadata$paths$path2outputs
path2R <- metadata$paths$path2R
source(paste0(path2R, "projection.R"))
source("projection-outputs-processing/annex-tables-functions.R")
require(openxlsx)
# Gotcha: If target .xlsx file is open, code will silently fail

projLM <- readRDS(paste0(path2outputs, "projections/proj5LM.rds"))
crude.rates <- matrix(0, nrow = 48, ncol = 9)
rownames(crude.rates) <- paste0(metadata$place$codes, names(projLM))
colnames(crude.rates) <- c("Births", "Deaths", "NatInc", "NetMig", "PYL", "CBR", "CDR", "CNIR", "CNMR")
for (i in 1:length(projLM)) {
  proj <- projLM[[i]]
  place1 <- names(projLM)[i]
  place2 <- paste0(metadata$place$codes[i], place1)
  ad <- get.AgeSexDistribution(proj)[, , "both"]
  totals <- apply(ad, 2, sum)
  PYL <- ((5 / 2) * (totals[1:5] + totals[2:6])) / 1000
  y <- get.ComponentsMatrix(proj)[c("Births", "Deaths", "NatInc", "NetMig"), ]
  crude.rates[i, 1:4] <- apply(y, 1, sum) / 1000
  crude.rates[i, 5] <- sum(PYL)
}
crude.rates[, 6] <- round(1000 * crude.rates[, 1] / crude.rates[, 5], 1)
crude.rates[, 7] <- round(1000 * crude.rates[, 2] / crude.rates[, 5], 1)
crude.rates[, 8] <- round(1000 * crude.rates[, 3] / crude.rates[, 5], 1)
crude.rates[, 9] <- round(1000 * crude.rates[, 4] / crude.rates[, 5], 1)
crude.rates[, 1:4] <- round(crude.rates[, 1:4], 1)
crude.rates[, 5] <- round(crude.rates[, 5], 0)
crude.rates
write.csv(crude.rates, paste0(path2outputs, "components.2020-2045.csv"))
