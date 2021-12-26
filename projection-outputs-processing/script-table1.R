metadata <- readRDS("metadata.rds")
path2inputs <- metadata$paths$path2inputs
path2outputs <- metadata$paths$path2outputs
path2R <- metadata$paths$path2R
source(paste0(path2R, "projection.R"))
require("XLConnect")
projLM <- readRDS(paste0(path2outputs, "projections/proj5LM.rds"))

make.table1 <- function(i, projLM, md = metadata) {
  # Arg: i index of projection in projLM, metadata
  # Eff: write table1 to Excel spreadsheet file
  proj <- projLM[[i]]
  place <- names(projLM)[i]
  asd <- get.AgeSexDistribution(proj)[, , "both"]
  colnames(asd) <- 2020 + 5 * 0:5
  asd <- as.data.frame(asd)
  asd <- cbind(Age = rownames(asd), asd)
  
  # MUST PREPEND TWO-DIGIT NUMBERS TO PLACE NAMES TO GET ORDER IN FILE MANAGER
  
  cog1 <- get.ComponentsMatrix(proj)[c("Births", "Deaths", "NatInc", "NetMig"), ]
  cog1 <- as.data.frame(cog1)
  cog1 <- cbind(cog1, End = rep("-", times = dim(cog1)[1]))
  colnames(cog1) <- c("2020-25", "2025-30", "2030-35", "2035-40", "2045-50", "2050-55")
  cog1 <- cbind(Component = rownames(cog1), cog1)
  
  cog2 <- matrix(0, nrow = dim(cog1)[1], ncol = length(proj))
  rownames(cog2) <- c("CBR", "CDR", "CNIR", "CNMR")
  colnames(cog2) <- names(proj)
  totals <- apply(asd[, 2:7], 2, sum)
  PYL <- (5 / 2) * (totals[1:5] + totals[2:6])
  cog2["CBR", ] <- 1000 * unlist(cog1["Births", 2:6]) / PYL
  cog2["CDR", ] <- 1000 * unlist(cog1["Deaths", 2:6]) / PYL
  cog2["CNIR", ] <- cog2["CBR", ] - cog2["CDR", ]
  cog2["CNMR", ] <- 1000 * unlist(cog1["NetMig", 2:6]) / PYL
  cog2 <- as.data.frame(cog2)
  cog2 <- cbind(RateComp = rownames(cog2), cog2)
  cog2 <- cbind(cog2, End = rep("-", times = dim(cog2)[1]))
  colnames(cog2) <- colnames(cog1)
  
  table.name <- paste0("table1-", names(projLM)[i], ".xlsx")
  wb <- loadWorkbook(table.name, create = TRUE)
  createSheet(wb, name = "main")
  writeWorksheet(wb, asd, sheet = "main", startRow = 1)
  writeWorksheet(wb, cog1, sheet = "main", startRow = 23)
  writeWorksheet(wb, cog2, sheet = "main", startRow = 28, header = FALSE)
  saveWorkbook(wb)
  return(table.name)
}

for (i in 1:length(projLM)) {
  table.name <- make.table1(i, projLM, md = metadata)
  cat(paste0(table.name, " written . . .\n"))
}
