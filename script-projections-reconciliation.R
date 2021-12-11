rm(list = ls())
metadata <- readRDS("metadata.rds")
path2inputs <- metadata$paths$path2inputs
path2outputs <- metadata$paths$path2outputs
path2R <- metadata$paths$path2R
source(paste0(path2R, "projection.R"))
places  <- metadata$place$places
pcycles <- metadata$time$pcycles
BaseASD <- readRDS(paste0(path2inputs, "BaseASD/BaseASD.rds"))  # View(BaseASD)
nLx     <- readRDS(paste0(path2inputs, "nLx/nLx.rds"))
ASBR    <- readRDS(paste0(path2inputs, "ASBR/ASBR.rds"))
NIMR   <- readRDS(paste0(path2inputs, "NIMR/NIMRd.rds"))

projections <- vector(mode = "list", length = length(places))
names(projections) <- places
projections.init <- projections
for (i in 1:length(projections)) {
  place <- names(projections)[i]
  projections.init[[i]] <- 
    initialize.projection.pframes(place, BaseASD, nLx, ASBR, NIMR)
  projections[[i]] <- calculate.projection(projections.init[[i]])
}


# National Projection
np <- projections$Kenya
cols <- c("ASDin", "Deaths", "Births", "Survivors", "ASDout")
for (i in 1:length(np)) {
  np[[i]][, cols] <- round(np[[i]][, cols], 0)
}
np  # These are the numbers we want subnational projections to be consistent with

# Consistent subnational projections
snpi <- transpose.ListMatrix(projections.init[2:48])  # This needs explaining
snp <- calculate.consistent.projections(np, snpi)

# Consistency parameters
rMatrixList <- vector(mode = "list", length = length(snp))
names(rMatrixList) <- pcycles
for (i in 1:length(snp)){
  rMatrixList[[i]] <- attr(snp[[i]], "rMatrix")
}
lapply(rMatrixList, round, 3)

# Look at county population change
snp <- transpose.ListMatrix(snp)  # This needs explaining
projectionTotals <- t(sapply(snp, get.projTotals))
projectionTotals <- sort.PlacesByGrowth(projectionTotals)
projectionTotals
write.csv(projectionTotals, paste0(path2outputs, "projections/projectionTotals.csv"))

# Look at components of change for counties
get.ComponentsMatrix(snp$Kwale)
get.ComponentsMatrix(snp$Kilifi)
get.ComponentsMatrix(snp$`Tana River`)
get.ComponentsMatrix(snp$`Taita-Taveta`)
get.ComponentsMatrix(snp$Garissa)
get.ComponentsMatrix(snp$Baringo)
get.ComponentsMatrix(snp$Vihiga)
