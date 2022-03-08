metadata <- readRDS("metadata.rds")
path2inputs <- metadata$paths$path2inputs
path2outputs <- metadata$paths$path2outputs
path2R <- metadata$paths$path2R
source(paste0(path2R, "projection.R"))
source("projection-outputs-processing/annex-tables-functions.R")
require(openxlsx)
# Gotcha: If target .xlsx file is open, code will silently fail

projLM <- readRDS(paste0(path2outputs, "projections/proj5LM.rds"))
for (i in 1:length(projLM)) {
  # Initialize for 5 year projection results (List Matrix)
  proj <- projLM[[i]]        # input projection
  place <- names(projLM)[i]
  filename <- paste0(metadata$place$codes[i], place, ".xlsx")
  pathfile <- paste0(path2outputs, "annex-tables1/", filename)  # output file
  # Write output file
  wb <- loadWorkbook(paste0(path2outputs, "annex-tables1/template1.xlsx"))
  write.page1(wb, proj, pathfile)
  write.page2(wb, proj, pathfile)
  saveWorkbook(wb, pathfile, overwrite = TRUE)
}
