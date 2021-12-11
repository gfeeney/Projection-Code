rm(list = ls())
metadata <- readRDS("metadata.rds")
path2inputs <- metadata$paths$path2inputs
path2outputs <- metadata$paths$path2outputs
path2R <- metadata$paths$path2R
source(paste0(path2R, "projection.R"))

proj <- readRDS(paste0(path2outputs, "Projections/proj20211019.rds"))
asdList <- get.AgeSexDistribution(proj$Kenya)
dim(asdList)
BothSexes <- round(asdList[, , "both"], 0)
write.csv(BothSexes, paste0(path2outputs, "ProjectedAgeDistribution.csv"))
Female <- round(asdList[, , "female"], 0)
Male <- round(asdList[, , "male"], 0)
BothSexes
Female
Male
ASSR <- round(100 * Male / Female, 0)
ASSR
