rm(list = ls())
metadata <- readRDS("metadata.rds")
path2inputs <- metadata$paths$path2inputs
path2outputs <- metadata$paths$path2outputs
path2R <- metadata$paths$path2R

# Get projected total population for 2020-2045 for each place
proj <- readRDS(paste0(path2outputs, "projections/proj20211019.rds"))
snpLM <- proj[2:48]  # names(sboLM)
get.CountyProjectionTotals(snpLM)

write.csv(projTotals, paste0(path2outputs, "projTotals.csv"))



get.ComponentsMatrix(snp$Kenya)
get.ComponentsMatrix(snp$Mombasa)
get.ComponentsMatrix(proj2$Kitui)

# projNM.components <- lapply(projNM, get.components)
# write.csv(projNM.components, paste0(path2outputs, "projNM.components.csv"))
# 
# # Check consistency of county projections with national projection
# projNM.totals[1, ] - apply(projNM.totals[2:48, ], 2, sum)
# CFA <- projNM.totals[1, ] / apply(projNM.totals[2:48, ], 2, sum)
# CFA
# # Good consistency, but sum of projected 2045 county totals exceeds projected 
# # national total by almost 2 million people. Constant Factor Adjustment forces
# # consistency
# projNM.totals[2:48, ] <- projNM.totals[2:48, ] * matrix(CFA, nrow = 47, ncol = 6, byrow = TRUE)
# projNM.totals[1, ] - apply(projNM.totals[2:48, ], 2, sum)
# CFA <- projNM.totals[1, ] / apply(projNM.totals[2:48, ], 2, sum)
# CFA
# 
# View(projNM.totals[2:48, ])
# 
# # What is the effect of rounding? How to we handle rounding?