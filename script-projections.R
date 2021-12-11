# PREPARATORY STEPS
metadata <- readRDS("metadata.rds")
source(paste0(path2R, "projection.R"))
path2inputs <- metadata$paths$path2inputs
path2outputs <- metadata$paths$path2outputs
path2R <- metadata$paths$path2R
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

# NEED TO REWRITE using 'projection' and 'projectionsList' ?? prjs4places listof.projections4places argh!

# PROJECTION 1: No migration
NIMR <- NIMRa
proj1 <- calculate.projections(places, BaseASD, nLx, ASBR, NIMR, md = metadata)
saveRDS(proj1, paste0(path2outputs, "projections/proj1.rds"))
proj1Totals <- t(sapply(proj1, get.projTotals))
proj1Totals <- sort.PlacesByGrowth(proj1Totals)
proj1Totals
write.csv(proj1Totals, paste0(path2outputs, "projections/proj1Totals.csv"))

# PROJECTION 2: Migration raw
NIMR   <- readRDS(paste0(path2inputs, "NIMR/NIMRb.rds"))
proj2 <- calculate.projections(places, BaseASD, nLx, ASBR, NIMR, md = metadata)
saveRDS(proj2, paste0(path2outputs, "projections/proj2.rds"))
proj2Totals <- t(sapply(proj2, get.projTotals))
proj2Totals <- sort.PlacesByGrowth(proj2Totals)
proj2Totals
write.csv(proj2Totals, paste0(path2outputs, "projections/proj2Totals.csv"))

# PROJECTION 3: Migration adjusted by LTCSRTMig census survival estimates
NIMR  <- readRDS(paste0(path2inputs, "NIMR/NIMRc.rds"))
proj3 <- calculate.projections(places, BaseASD, nLx, ASBR, NIMR, md = metadata)
saveRDS(proj3, paste0(path2outputs, "projections/proj3.rds"))
proj3Totals <- t(sapply(proj3, get.projTotals))
proj3Totals <- sort.PlacesByGrowth(proj3Totals)
proj3Totals
write.csv(proj3Totals, paste0(path2outputs, "projections/proj3Totals.csv"))

# PROJECTION 4: Migration adjusted with decline
NIMR  <- readRDS(paste0(path2inputs, "NIMR/NIMRd.rds"))
proj4 <- calculate.projections(places, BaseASD, nLx, ASBR, NIMR, md = metadata)
saveRDS(proj4, paste0(path2outputs, "projections/proj4.rds"))
proj4Totals <- t(sapply(proj4, get.projTotals))
proj4Totals <- sort.PlacesByGrowth(proj4Totals)
proj4Totals
write.csv(proj4Totals, paste0(path2outputs, "projections/proj4Totals.csv"))


              

