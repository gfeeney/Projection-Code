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
NIMR   <- readRDS(paste0(path2inputs, "NIMR/NIMRd.rds"))        # Note NIMRd

proj5LM.init <- vector(mode = "list", length = length(places))  # 48x5 List Matrix
names(proj5LM.init) <- places
for (i in 1:length(proj5LM.init)) {
  place <- names(proj5LM.init)[i]
  proj5LM.init[[i]] <- initialize.projection.pframes(place, BaseASD, nLx, ASBR, NIMR)
}
# np = national projection ; snp = subnational projection ; init = initialized
np <- calculate.projection(proj5LM.init[[1]])
snp1.init <- proj5LM.init[2:48]                            # 47x5 List Matrix
snp2.init <- transpose.ListMatrix(snp1.init)               # 5x47 List Matrix
snp2 <- calculate.consistent.projections(np, snp2.init)    # 5x47 List Matrix
snp1 <- transpose.ListMatrix(snp2)                         # 47x5 List Matrix
proj5LM <- vector(mode = "list", length = length(places))  # 48x5 List Matrix
names(proj5LM) <- places
proj5LM[[1]] <- np
proj5LM[2:48] <- snp1
saveRDS(proj5LM, paste0(path2outputs, "projections/proj5LM.rds"))

# Consistency parameters
rMatrixList <- vector(mode = "list", length = length(snp2))
names(rMatrixList) <- pcycles
for (i in 1:length(snp2)){
  rMatrixList[[i]] <- attr(snp2[[i]], "rMatrix")
}
lapply(rMatrixList, round, 3)

# Look at county population change
proj5Totals <- t(sapply(proj5LM, get.projTotals))
proj5Totals <- sort.PlacesByGrowth(proj5Totals)
proj5Totals  # This is fine, we just don't have Kenya at top
write.csv(proj5Totals, paste0(path2outputs, "projections/proj5Totals.csv"))

# Look at components of change for counties
# get.ComponentsMatrix(snp$Kwale)
# get.ComponentsMatrix(snp$Kilifi)
# get.ComponentsMatrix(snp$`Tana River`)
# get.ComponentsMatrix(snp$`Taita-Taveta`)
# get.ComponentsMatrix(snp$Garissa)
# get.ComponentsMatrix(snp$Baringo)
# get.ComponentsMatrix(snp$Vihiga)
