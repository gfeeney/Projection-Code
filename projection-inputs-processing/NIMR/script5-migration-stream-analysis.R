# NOTE This script is optional. I included it because Schola expressed an 
# interest in analyzing migration streams.

rm(list = ls())
metadata <- readRDS("metadata.rds")
path2inputs <- metadata$paths$path2inputs
path2R <- metadata$paths$path2R
source(paste0(path2R, "projection.R"))
source("metadata.R")

# Read in 2019 and 2009 census migration data
md2019 <- readRDS(paste0(path2inputs, "NIMR/MigData2019.rds"))
md2009 <- readRDS(paste0(path2inputs, "NIMR/MigData2009.rds"))

# Before doing migration stream analysis, compare overall level
# of migration indicated by 2009 and 2019 census questions on county
# of residence 1 year ago
dim(md2019)  # We want migration matrix for both sexes and all ages
MigMat <- apply(md2019, 1:2, sum)  # View(MigMat)
NonMig <- sum(diag(MigMat))
NonMig
diag(MigMat) <- 0
Mig <- sum(MigMat)
Mig
Pop <- NonMig + Mig  # Population age 1 year old and over
Rate2019 <- Mig / Pop
Rate2019  # [1] 0.07386088

MigMat <- apply(md2009, 1:2, sum)  # View(MigMat)
NonMig <- sum(diag(MigMat))
NonMig
diag(MigMat) <- 0
Mig <- sum(MigMat)
Mig
Pop <- NonMig + Mig  # Population age 1 year old and over
Rate2009 <- Mig / Pop
Rate2009  # [1] 0.0353481

# So 7.39% in 2019, up from 3.53% in 2009, 739/353 = 2.09, more than doubled

# R function to extract migration streams from ToFrom matrices

# Number of streams is 47 * 47 - 47 = 2162; too many to look
# closely at all of them. Our aim is to identify the largest
# streams individually and summarize the smaller streams. 
# How to define 'large' and how to summarize the smaller streams
# remains to be seen. WE ARE LEARNING HOW TO THINK ABOUT THIS
# KIND OF ANALYSIS! OUR LEARNING IS JUST BEGINNING! The total
# number of streams in the Myanmar analysis was much lower
# (9 * 14 = 126; see Table 8.1 page 96 of the Myanmar report).

ToFrom <- md2019[, , 1, 1]
diag(ToFrom) <- 0
ToFrom[1:8, 1:8]
rownames(ToFrom)
colnames(ToFrom)

streams <- get.streams(ToFrom)
View(streams)



# NEXT STEPS: (1) Consider what streams should be ignored because the 
# numbers are too small to be statistically stable. How to handle 
# these numbers? (2) Look at all the strems to a particular county,
# For example:

ToCountyStreams <- vector(mode = "list", length = 47)
names(ToCountyStreams) <- colnames(ToFrom)
for (i in 1:47) {
  ToCountyStreams[[i]] <- streams[streams[, 1] == colnames(ToFrom)[i], ]  
}
ToCountyStreams


