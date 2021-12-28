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
  # Initialize for 5 year projection results
  proj <- projLM[[i]]        # input projection
  place <- names(projLM)[i]
  filename <- paste0(metadata$place$codes[i], place, ".xlsx")
  pathfile <- paste0(path2outputs, "annex-tables0/", filename)  # output file
  # Additional initialization for annual projections 2020-2035
  proj13 <- proj[1:3]
  asd5x5 <- get.projectedASD(proj)
  asd5x1 <- asd5x1.from.asd5x5(asd5x5)
  colnames(asd5x1) <- 2019 + 1:dim(asd5x1)[2]
  female <- asd5x1[1:20, ]
  female <- rbind(female, apply(female, 2, sum))
  rownames(female)[21] <- "All Ages"
  male <- asd5x1[20 + 1:20, ]
  male <- rbind(male, apply(male, 2, sum))
  rownames(male)[21] <- "All Ages"
  both <- female + male
  rownames(both)[21] <- "All Ages"
  
  # Write output file
  wb <- loadWorkbook(paste0(path2outputs, "annex-tables0/template0.xlsx"))
  write.page1(wb, proj, pathfile)
  write.page2(wb, proj, pathfile)
  write.page3(wb, both, male, female)
  write.page4(wb, both, male, female)
  write.page5(wb, both, male, female)
  write.page6(wb, both, male, female)
  write.page7(wb, both, male, female)
  saveWorkbook(wb, pathfile, overwrite = TRUE)
}
