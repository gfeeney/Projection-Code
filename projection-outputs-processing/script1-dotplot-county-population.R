rm(list=ls())
metadata <- readRDS("metadata.rds")
path2inputs <- metadata$paths$path2inputs
path2outputs <- metadata$paths$path2outputs
path2R <- metadata$paths$path2R
source(paste0(path2R, "projection.R"))
area    <- metadata$place$area
subareas <- metadata$place$subareas
places <- c(area, subareas)
pcycles  <- metadata$time$pycles
ASDrows  <- metadata$age$ASDrows
BaseTime <- metadata$time$BaseTime

calculate.TotalsLog2 <- function(pfmatrix) {
  ASDlist <- lapply(pfmatrix, asd.from.ppframes, ASDrows, BaseTime)
  Totals <- do.call(rbind, lapply(ASDlist, function(x){apply(x, 2, sum)}))
  Totals <- Totals[order(Totals[, 1]), ] / 1000
  return(log2(Totals))
}

dotplot.countypopulation <- function(pfmatrix, xlab, pathfileout, xlim = c(7, 13), xatv = 7:13) {
  TotalsLog2 <- calculate.TotalsLog2(pfmatrix)
  catvalues <- TotalsLog2[-1, 1]
  xlim <- c(7, 13)
  xatv <- 7:13
  pdf(pathfileout, paper="a4", width=7.27, height=10.69, pointsize=9)
  par(mai=c(0.6, 1.3, 0.6, 0.2))
  #browser()
  draw.dotplot(xlab, xlim, xatv, catvalues, pchArg = 1, LOG2 = TRUE)
  catvalues <- TotalsLog2[-1, 6]
  points(catvalues, 1:length(catvalues), pch = 16)
  dev.off()
}

# NO MIGRATION, NO RECONCILIATION
pfmatrix <- readRDS(paste0(path2outputs, "pframe-matrix-NO-MIGRATION.rds"))
xlab <- "County Population (000) - 2020-2045 - NO MIGRATION"
pathfileout <- paste0(path2outputs, "CountyPopulation-NO-MIGRATION.pdf")
dotplot.countypopulation(pfmatrix, xlab, pathfileout)

# MIGRATION, NO RECONCILIATION
pfmatrix <- readRDS(paste0(path2outputs, "pframe-matrix-MIGRATION.rds"))
xlab <- "County Population (000) 2020-2045 - MIGRATION"
pathfileout <- paste0(path2outputs, "CountyPopulation-MIGRATION.pdf")
dotplot.countypopulation(pfmatrix, xlab, pathfileout)

# Dotplot Percent Change 2020-2045
dotplot.PcntChange <- function(pfmatrix, xlab, pathfileout, xlim = c(-40, 40), xatv = -40 + 20 * 0:4) {
  TotalsLog2 <- calculate.TotalsLog2(pfmatrix)
  Pop2020 <- TotalsLog2[-1, 1]
  Pop2045 <- TotalsLog2[-1, 6]
  PcntChange <- 100 * (Pop2045 - Pop2020) / Pop2020
  catvalues <- PcntChange[order(PcntChange)]
  xatv <- xlim[1] + 0:4 * (xlim[2] - xlim[1]) / 4
  pdf(pathfileout, paper="a4", width = 7.27, height = 10.69, pointsize = 9)
  par(mai=c(0.6, 1.3, 0.6, 0.2))
  draw.dotplot(xlab, xlim, xatv, catvalues, pchArg = 1, LOG2 = FALSE)
  dev.off()
}

# NO MIGRATION
pfmatrix <- readRDS(paste0(path2outputs, "pframe-matrix-NO-MIGRATION.rds"))
xlab <- "Percent Change in County Population 2020-2045 - NO MIGRATION"
xlim <- c(0, 16)
pathfileout <- paste0(path2outputs, "CountyPcntChange-NO-MIGRATION.pdf")
dotplot.PcntChange(pfmatrix, xlab, pathfileout, xlim, xatv)

# MIGRATION
pfmatrix <- readRDS(paste0(path2outputs, "pframe-matrix-MIGRATION.rds"))
xlab <- "Percent Change in County Population 2020-2045 - MIGRATION"
xlim <- c(-40, 40)
pathfileout <- paste0(path2outputs, "CountyPcntChange-MIGRATION.pdf")
dotplot.PcntChange(pfmatrix, xlab, pathfileout, xlim, xatv)
