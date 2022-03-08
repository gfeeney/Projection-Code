metadata <- readRDS("metadata.rds")
path2inputs <- metadata$paths$path2inputs
path2outputs <- metadata$paths$path2outputs
path2R <- metadata$paths$path2R
source(paste0(path2R, "projection.R"))
require("openxlsx")
# Gotcha: If target .xlsx file is open, code will silently fail

projLM <- readRDS(paste0(path2outputs, "projections/proj5LM.rds"))

# Initialize list for annual projected age distributions for both sexes
# for calculation of school age numbers
school <- vector(mode = "list", length = length(projLM))
names(school) <- names(projLM)
# End Initialize List for annual . . .

for (i in 1:length(projLM)) {
  # Initialize for 5 year projection results (List Matrix)
  proj <- projLM[[i]]        # input projection
  place <- names(projLM)[i]
  filename <- paste0(metadata$place$codes[i], place, ".xlsx")
  pathfile <- paste0(path2outputs, "annex-tables2/", filename)  # output file
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
  
  school[[i]] <- both  # For calculation of school age numbers
  
  # Write output file for annual projections
  wb <- loadWorkbook(paste0(path2outputs, "annex-tables2/template2.xlsx"))
  # write.page1(wb, proj, pathfile)
  # write.page2(wb, proj, pathfile)
  write.page3(wb, both, male, female)
  write.page4(wb, both, male, female)
  write.page5(wb, both, male, female)
  write.page6(wb, both, male, female)
  write.page7(wb, both, male, female)
  saveWorkbook(wb, pathfile, overwrite = TRUE)
}

# Save 'school' list to annex-tables3 directory for school age number
# calculations
saveRDS(school, paste0(path2outputs, "annex-tables3/school.rds"))
