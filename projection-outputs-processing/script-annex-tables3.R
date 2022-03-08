metadata <- readRDS("metadata.rds")
path2outputs <- metadata$paths$path2outputs
school <- readRDS(paste0(path2outputs, "annex-tables3/school.rds"))
source("projection-outputs-processing/interpolate-school-ages.R")

sages <- vector(mode = "list", length = length(school))
names(sages) <- names(school)
for (i in 1:length(school)) {
  x <- as.data.frame(school[[i]][, 1:16])
  place <- names(school)[i]
  sages[[i]] <- t(sapply(x, 'interpolate.school.ages'))
  #colnames(sages[[i]]) <- c("Age 3-5", "Age 6-13", "Age 14-17")
  filename <- paste0(metadata$place$codes[i], place, ".csv")
  pathfile <- paste0(path2outputs, "annex-tables3/", filename)
  write.csv(sages[[i]], pathfile)
}

Kenya1 <- sages[[1]]
Counties <- sages[2:48]
Kenya2 <- Reduce("+", Counties) 
round(Kenya1 - Kenya2, 0)
Kenya <- Kenya2
write.csv(Kenya, paste0(path2outputs, "annex-tables3/00Kenya.csv"))
