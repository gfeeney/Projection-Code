path2R <- "R/" # Edit this line if you change standard directory structure
source(paste0(path2R, "projection.R"))

# Metadata variable values
width <- 5
oeg <- 95
rag <- 4:10  # Reproductive age groups
CensusTime <- 2019 + (31 + 28 + 31 + 30 + 31 + 30 + 31 + 23) / 365  
# 2019.644 23/24 August 2019
BaseTime <- 2020.5
npcycles <- 5
pcycles <- paste0("pcycle", 1:npcycles)
area <- "Kenya"
counties  <- c("Mombasa","Kwale","Kilifi","Tana River","Lamu","Taita-Taveta",
               "Garissa", "Wajir","Mandera","Marsabit","Isiolo","Meru",
               "Tharaka-Nithi","Embu","Kitui", "Machakos","Makueni","Nyandarua",
               "Nyeri","Kirinyaga","Murang'a","Kiambu", "Turkana","West Pokot",
               "Samburu","Trans-Nzoia","Uasin Gishu","Elgeyo-Marakwet","Nandi",
               "Baringo","Laikipia","Nakuru","Narok","Kajiado","Kericho",
               "Bomet", "Kakamega","Vihiga","Bungoma","Busia","Siaya","Kisumu",
               "Homa Bay","Migori", "Kisii","Nyamira","Nairobi City")
subareas <- counties
path2inputs <- "../projection-inputs-data/"
path2outputs <- "../projection-outputs-data/"


metadata <- get.metadata(width, oeg, rag, CensusTime, BaseTime, pcycles, area, 
                         subareas, path2inputs, path2outputs, path2R)
saveRDS(metadata, "metadata.rds")  # Write metadata to .rds
readRDS("metadata.rds")

# NEXT STEPS
# 1. Run script.calculate.BaseASD.R
# 2. Run script-calculate.nLx.R
# 3. Run script-calculate.ASBR.R
# 4. Run script1-import-migration-data.R
# 5. Run script2-calculate-NIM.R
# 6. Run script3-calculate-adjustment-factors.R
# 7. Run script4-calculate-NIMR.R
# 8. Calculate projections

