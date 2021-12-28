# PREPARATORY STEPS
metadata <- readRDS("metadata.rds")
source(paste0(metadata$paths$path2R, "projection.R"))
path2inputs <- metadata$paths$path2inputs
path2outputs <- metadata$paths$path2outputs
places  <- metadata$place$places
pcycles <- metadata$time$pcycles
place <- places[1]
BaseASD <- readRDS(paste0(path2inputs, "BaseASD/BaseASD.rds"))  # View(BaseASD)
nLx <- readRDS(paste0(path2inputs, "nLx/nLx.rds"))
ASBR <- readRDS(paste0(path2inputs, "ASBR/ASBR.rds"))
NIMRa <- readRDS(paste0(path2inputs, "NIMR/NIMRa.rds"))  # No migration
NIMRb <- readRDS(paste0(path2inputs, "NIMR/NIMRb.rds"))  # Migration raw
NIMRc <- readRDS(paste0(path2inputs, "NIMR/NIMRc.rds"))  # Migration adjusted
NIMRd <- readRDS(paste0(path2inputs, "NIMR/NIMRd.rds"))  # Add decline

# Four alternative projections are calculated based on the NIMR values in the
# preceding four lines. These are not consistent subnational projections. A
# consistent subnational projection for the fourth case, NIMRd, is calculated
# by script2-calculate-consistent-subnational-projections.R

# PROJECTION 1: No migration
NIMR <- NIMRa
proj1LM <- calculate.projections(places, BaseASD, nLx, ASBR, NIMR, md = metadata)
saveRDS(proj1LM, paste0(path2outputs, "projections/proj1LM.rds"))
proj1Totals <- t(sapply(proj1LM, get.projTotals))
write.csv(proj1Totals, paste0(path2outputs, "projections/proj1Totals.csv"))
sort.PlacesByGrowth(proj1Totals)

# PROJECTION 2: Migration raw
NIMR   <- readRDS(paste0(path2inputs, "NIMR/NIMRb.rds"))
proj2LM <- calculate.projections(places, BaseASD, nLx, ASBR, NIMR, md = metadata)
saveRDS(proj2LM, paste0(path2outputs, "projections/proj2LM.rds"))
proj2Totals <- t(sapply(proj2LM, get.projTotals))
write.csv(proj2Totals, paste0(path2outputs, "projections/proj2Totals.csv"))
sort.PlacesByGrowth(proj2Totals)

# PROJECTION 3: Migration adjusted by LTCSRTMig census survival estimates
NIMR  <- readRDS(paste0(path2inputs, "NIMR/NIMRc.rds"))
proj3LM <- calculate.projections(places, BaseASD, nLx, ASBR, NIMR, md = metadata)
saveRDS(proj3LM, paste0(path2outputs, "projections/proj3LM.rds"))
proj3Totals <- t(sapply(proj3LM, get.projTotals))
write.csv(proj3Totals, paste0(path2outputs, "projections/proj3Totals.csv"))
sort.PlacesByGrowth(proj3Totals)

# PROJECTION 4: Migration adjusted with decline
NIMR  <- readRDS(paste0(path2inputs, "NIMR/NIMRd.rds"))
proj4LM <- calculate.projections(places, BaseASD, nLx, ASBR, NIMR, md = metadata)
saveRDS(proj4LM, paste0(path2outputs, "projections/proj4LM.rds"))
proj4Totals <- t(sapply(proj4LM, get.projTotals))
write.csv(proj4Totals, paste0(path2outputs, "projections/proj4Totals.csv"))
sort.PlacesByGrowth(proj4Totals)

# PROJECTION 5: NIMRd with reconciliation of subnational nation to national projections
# See script-projections-reconciliation.R

              

