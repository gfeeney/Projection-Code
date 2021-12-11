rm(list=ls())
path2R <- "../projections2/R/"
path2inputs <- "../projection-inputs/"
path2outputs <- "../projection-outputs/"
projectionList <- readRDS(paste0(path2outputs, "ProjectionList-NO-MIGRATION.rds"))
projection <- projectionList$Kajiado
projection
write.csv(projection[[1]], paste0(path2outputs, "pcycle1.csv"))
