rm(list = ls())
path2R <- "../projections2/R/"
path2inputs <- "../projection-inputs/"
path2outputs <- "../projection-outputs/"
ATP <- readRDS("../projection-inputs/AgeTimePlace.rds")
ASDrows <- ATP$Age$ASDrows
BaseTime <- ATP$Time$Base
source("../projections2/R/projection.R")

# NO MIGRATION, NO RECONCILIATION
projectionResultsList <- readRDS(paste0(path2outputs, "ProjectionResultsList-NO-MIGRATION.rds"))
Totals <- as.matrix(do.call(rbind, lapply(projectionResultsList, function(x){x$TotalPopulation})))
Totals <- round(Totals / 1000, 0)
Totals
t1 <- Totals[, 1]
t2 <- Totals[, 6]
pc <- round(100 * (t2 - t1) / t1, 1)
catvalues <- pc[order(pc)]
catvalues
xlab <- "Percent Change 2020-2045 - NO MIGRATION"
xlim <- c(0, 160)
xatv <- xlim[1] + 0:4 * (xlim[2] - xlim[1]) / 4
par(mai=c(0.6, 1.3, 0.6, 0.2))
draw.dotplot(xlab, xlim, xatv, catvalues, pchArg = 1, LOG2 = FALSE)

pathfileout = paste0(path2outputs, "PercentChange-NO-MIGRATION.pdf")
pdf(pathfileout, paper="a4", width=7.27, height=10.69, pointsize=9)
par(mai=c(0.6, 1.3, 0.6, 0.2))
draw.dotplot(xlab, xlim, xatv, catvalues, pchArg = 1, LOG2 = FALSE)
dev.off()

# MIGRATION, NO RECONCILIATION
projectionResultsList <- readRDS(paste0(path2outputs, "ProjectionResultsList-MIGRATION.rds"))
Totals <- as.matrix(do.call(rbind, lapply(projectionResultsList, function(x){x$TotalPopulation})))
Totals <- round(Totals / 1000, 0)
Totals
t1 <- Totals[, 1]
t2 <- Totals[, 6]
pc <- round(100 * (t2 - t1) / t1, 1)
catvalues <- pc[order(pc)]
catvalues
xlab <- "Percent Change 2020-2045 - MIGRATION"
xlim <- c(-100, 1000)
xatv <- c(xlim[1], 0, 100, 1:5 * 200)
par(mai=c(0.6, 1.3, 0.6, 0.2))
draw.dotplot(xlab, xlim, xatv, catvalues, pchArg = 1, LOG2 = FALSE)

pathfileout = paste0(path2outputs, "PercentChange-MIGRATION.pdf")
pdf(pathfileout, paper="a4", width=7.27, height=10.69, pointsize=9)
par(mai=c(0.6, 1.3, 0.6, 0.2))
draw.dotplot(xlab, xlim, xatv, catvalues, pchArg = 1, LOG2 = FALSE)
dev.off()

gt0 <- which(catvalues > 0)
le0 <- which(catvalues <= 0)
catvalues[gt0] <- log2(catvalues[gt0])

