metadata <- readRDS("metadata.rds")
path2inputs <- metadata$paths$path2inputs
path2outputs <- metadata$paths$path2outputs
path2R <- metadata$paths$path2R
source(paste0(path2R, "projection.R"))

proj5LM <- readRDS(paste0(path2outputs, "projections/proj5LM.rds"))
asdList <- get.AgeSexDistribution(proj5LM$Kenya)  # dim(asdList)
asdBoth <- asdList[, , "both"]
colnames(asdBoth) <- 2020 + 5 * 0:5

projKenya <- proj5LM$Kenya
pframe <- projKenya[[1]]
is.matrix(pframe)

cog <- get.ComponentsMatrix(projKenya)
cog <- cbind(cog, rep(NA, times = dim(cog)[1]))
colnames(cog) <- colnames(asdBoth)
cog[c("Births", "Deaths", "NatInc", "NetMig"), ]

PYL <- (5 / 2) * (asdBoth[, 1:5] + asdBoth[, 2:6])
totals <- apply(asdBoth, 2, sum)
PYL <- (5 / 1) * (totals[1:5] + totals[2:6])
CBR <- round(1000 * cog["Births", 1:5] / PYL, 1)
CDR <- round(1000 * cog["Deaths", 1:5] / PYL, 1)
CRNI <- CBR - CDR

adi2020 <- round(100 * x[, 1] / x[1, 1], 0)
adi2045 <- round(100 * x[, 6] / x[1, 6], 0)

round(100 * (x[, 6] - x[, 1]) / x[, 1], 0)

PcntGro <- round((100 * x[, 6] - x[, 1]) / x[, 1], 0)
adtable <- cbind(x, adi2020, adi2045, adchange)
adtable



write.csv(BothSexes, paste0(path2outputs, "ProjectedAgeDistribution.csv"))
Female <- round(asdList[, , "female"], 0)
Male <- round(asdList[, , "male"], 0)
BothSexes
Female
Male
ASSR <- round(100 * Male / Female, 0)
ASSR
