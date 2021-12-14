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

proj5LM.init <- vector(mode = "list", length = length(places))     # 48x5 List Matrix
names(proj5LM.init) <- places
for (i in 1:length(proj5LM.init)) {
  place <- names(proj5LM.init)[i]
  proj5LM.init[[i]] <- initialize.projection.pframes(place, BaseASD, nLx, ASBR, NIMR)
}
# np = national projection ; snp = subnational projection ; init = initialized
np <- calculate.projection(proj5LM.init[[1]])                      # National projection
snp47x5LM.init <- proj5LM.init[2:48]                               # Subnational projections
snp5x47LM.init <- transpose.ListMatrix(snp47x5LM.init)             # Subnational projections
snp5x47LM <- calculate.consistent.projections(np, snp5x47LM.init)  # Subnational projections
snp47x5LM <- transpose.ListMatrix(snp5x47LM)                       # Subnational projections
proj5LM <- vector(mode = "list", length = length(places))          # National + Subnational
names(proj5LM) <- places                                           # National + Subnational
proj5LM[[1]] <- np                                                 # National
proj5LM[2:48] <- snp47x5LM                                         # Subnational
saveRDS(proj5LM, paste0(path2outputs, "projections/proj5LM.rds"))  # National + Subnational

# Consistency parameters
cMatrix.pcycles <- vector(mode = "list", length = length(snp5x47LM))
names(cMatrixList) <- pcycles
for (i in 1:length(snp5x47LM)){
  cMatrix.pcycles[[i]] <- attr(snp5x47LM[[i]], "rMatrix")
}
saveRDS(cMatrix.pcycles, paste0(path2outputs, "projections/cMatrix.pcycles.rds"))
lapply(cMatrix.pcycles, round, 3)

# Look at county population change
proj5Totals <- t(sapply(proj5LM, get.projTotals))
write.csv(proj5Totals, paste0(path2outputs, "projections/proj5Totals.csv"))
proj5TotalsSorted <- sort.PlacesByGrowth(proj5Totals)
proj5TotalsSorted
write.csv(proj5TotalsSorted, paste0(path2outputs, "projections/proj5TotalsSorted.csv"))

# Look at components of change for counties
# get.ComponentsMatrix(snp$Kwale)
# get.ComponentsMatrix(snp$Kilifi)
# get.ComponentsMatrix(snp$`Tana River`)
# get.ComponentsMatrix(snp$`Taita-Taveta`)
# get.ComponentsMatrix(snp$Garissa)
# get.ComponentsMatrix(snp$Baringo)
# get.ComponentsMatrix(snp$Vihiga)
