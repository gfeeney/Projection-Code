metadata <- readRDS("metadata.rds")
source(paste0(metadata$paths$path2R, "projection.R"))
path2outputs <- metadata$paths$path2outputs
proj4 <- readRDS(paste0(path2outputs, "projections/proj4.rds"))
names(proj4[[1]])

projection <- proj4[[1]]
asd5x5 <- get.projectedASD(projection)
asd5x1 <- asd5x1.from.asd5x5(asd5x5)
asd5x1

asd5x5.list <- lapply(proj4, get.projectedASD)
names(asd5x5.list)
asd5x1.list <- lapply(asd5x5.list, asd5x1.from.asd5x5)
names(asd5x1.list)

# The above code does the grunt work of producing projected 5 year
# age-sex distributions at 5 year time intervals and of interpolating
# 5x5 age distributions to 5x1 age distributions (at 1 year rather
# than 5 year intervals). Additional work is mainly formating.
