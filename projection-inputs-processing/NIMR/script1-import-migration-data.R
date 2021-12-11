metadata <- readRDS("metadata.rds")
path2inputs <- metadata$paths$path2inputs
source(paste0(metadata$paths$path2R, "projection.R"))

# INPUTS

# paste0(path2inputs, "Source/NIMsource2019.xlsx") 
# paste0(path2inputs, "Source/NIMsource2019.csv")

# paste0(path2inputs, "Source/NIMsource2009.xlsx") > 
# paste0(path2inputs, "Source/NIMsource2009.csv")

# OUTPUT
# MigData2019: 47x47x2x14 array: County at Census, County 1 year ago, Sex, Age
# MigData2009: 47x47x2x14 array: County at Census, County 1 year ago, Sex, Age

# Parameters for reading 2019 census migration data
pathfile  <- paste0(path2inputs, "NIMR/src/NIMsource2019.csv")  # Note NIMR changed to NIM
AgeGroups <- getAG(5, 95)
dataColsM <- 51:97
dataColsF <- 99:145
dataRows  <- 60:106
increment <- 50
# Read in 2019 data
MigData2019 <- import.MigData(pathfile, AgeGroups, dataColsM, dataColsF, dataRows, increment)
dim(MigData2019)  # [1] 47 47  2 20
dimnames(MigData2019)
# Check 2019 data (note columns EP:JK "intersex")
male <- apply(MigData2019, 1:3, sum)[, , "Male"]
diag(male)[c(1:3, 45:47)]     # Check!
female <- apply(MigData2019, 1:3, sum)[, , "Female"]
diag(female)[c(1:3, 45:47)]   # Check!
both <- male + female
pop <- sum(both)  # [1] 45998420 (less age 0)
diag(both) <- 0
mig <- sum(both)  # 3397484
mig / pop  # [1] 0.07386088
# Save 2019 to .rds
pathfile <- paste0(path2inputs, "NIMR/MigData2019.rds")
saveRDS(MigData2019, pathfile)


# Parameters for reading 2009 census migration data
pathfile  <- paste0(path2inputs, "NIMR/src/NIMsource2009.csv")
AgeGroups <- getAG(5, 65)
dataColsM <- 51:97
dataColsF <- 99:145
dataRows  <- 60:106
increment <- 50
# Read in 2009 data
MigData2009 <- import.MigData(pathfile, AgeGroups, dataColsM, dataColsF, dataRows, increment)
dim(MigData2009)  # [1] 47 47  2 14
# Check 2009 data (note columns EP:JK "intersex")
male <- apply(MigData2009, 1:3, sum)[, , "Male"]
diag(male)[c(1:3, 45:47)]     # Check!
female <- apply(MigData2009, 1:3, sum)[, , "Female"]
diag(female)[c(1:3, 45:47)]   # Check!
both <- male + female
pop <- sum(both) 
diag(both) <- 0
mig <- sum(both)
mig / pop  # [1] 0.0353481
# Save 2009 to .rds
pathfile <- paste0(path2inputs, "NIMR/MigData2009.rds")
saveRDS(MigData2009, pathfile)

# Ratio of 2019 rate to 2009 rate
0.07386088 / 0.0353481  # [1] 2.089529




