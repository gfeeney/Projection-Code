rm(list = ls())
metadata <- readRDS("metadata.rds")
source(paste0(metadata$paths$path2R, "projection.R"))
path2inputs <- metadata$paths$path2inputs
path2outputs <- metadata$paths$path2outputs
path2R <- metadata$paths$path2R
places  <- metadata$place$places
pcycles <- metadata$time$pcycles
BaseASD <- readRDS(paste0(path2inputs, "BaseASD/BaseASD.rds"))  # View(BaseASD)
nLx     <- readRDS(paste0(path2inputs, "nLx/nLx.rds"))
ASBR    <- readRDS(paste0(path2inputs, "ASBR/ASBR.rds"))
NIMR   <- readRDS(paste0(path2inputs, "NIMR/NIMRd.rds"))  # Note NIMRs!!!

proj5LM <- vector(mode = "list", length = length(places))
names(proj5LM) <- places
proj5LM.init <- proj5LM
for (i in 1:length(proj5LM)) {
  place <- names(proj5LM)[i]
  proj5LM.init[[i]] <- initialize.projection.pframes(place, BaseASD, nLx, ASBR, NIMR)
  proj5LM[[i]] <- calculate.projection(proj5LM.init[[i]])
}

# National Projection I'M NOT SURE ABOUT THIS NOW, HOLD OFF
np <- proj5LM$Kenya  # national
# cols <- c("ASDin", "Deaths", "Births", "Survivors", "ASDout")
# for (i in 1:length(np)) {
#   np[[i]][, cols] <- round(np[[i]][, cols], 0)
# }
# np  # These are the numbers we want subnational projections to be consistent with

# Consistent subnational projections
snpi <- transpose.ListMatrix(proj5LM.init[2:48])  # This needs explaining
snp <- calculate.consistent.projections(np, snpi)

# SO NOW I WANT TO PUT np and snp BACK TOGETHER TO GET A CONSISTENT proj5LM
# STOP HERE, TOO TIRED TO DO MORE TONIGHT, WILL JUST SCREW IT UP

# Consistency parameters
rMatrixList <- vector(mode = "list", length = length(snp))
names(rMatrixList) <- pcycles
for (i in 1:length(snp)){
  rMatrixList[[i]] <- attr(snp[[i]], "rMatrix")
}
lapply(rMatrixList, round, 3)

# Look at county population change
snp <- transpose.ListMatrix(snp)  # This needs explaining
proj5Totals <- t(sapply(snp, get.projTotals))
proj5Totals <- sort.PlacesByGrowth(proj5Totals)
proj5Totals  # This is fine, we just don't have Kenya at top
write.csv(proj5Totals, paste0(path2outputs, "projections/proj5Totals.csv"))

# Look at components of change for counties
get.ComponentsMatrix(snp$Kwale)
get.ComponentsMatrix(snp$Kilifi)
get.ComponentsMatrix(snp$`Tana River`)
get.ComponentsMatrix(snp$`Taita-Taveta`)
get.ComponentsMatrix(snp$Garissa)
get.ComponentsMatrix(snp$Baringo)
get.ComponentsMatrix(snp$Vihiga)
