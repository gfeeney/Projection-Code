metadata <- readRDS("metadata.rds")
path2inputs <- metadata$paths$path2inputs
source(paste0(metadata$paths$path2R, "projection.R"))

# INPUT: NIM5x1.rds
# OUTPUT: countiesOUT, countiesIN, Adjustment Factors 

# STEP 1: Identify net out-migration counties and net in-migration counties
# Calculate total net migrants for each county
NIM <- readRDS(paste0(path2inputs, "NIMR/NIM5x1.rds"))
NIMtotal <- apply(NIM, 2, sum)
NIMtotal
# Net out-migration counties
countiesOUT <- names(NIMtotal[NIMtotal < 0])
countiesOUT
length(countiesOUT)
saveRDS(countiesOUT, paste0(path2inputs, "NIMR/countiesOUT.rds"))
# Net in-migration counties
countiesIN <- names(NIMtotal[NIMtotal > 0])
countiesIN
length(countiesIN)
saveRDS(countiesIN, paste0(path2inputs, "NIMR/countiesIN.rds"))

# STEP 2: Calculate total female and male net migrants in out-migration counties
# and in-migration counties
NIMsc <- matrix(0, nrow = 2, ncol = 47)  # sc=BY SEX AND COUNTYBy sex and county
rownames(NIMsc) <- c("Female", "Male")
colnames(NIMsc) <- colnames(NIM)
NIMsc[1, ] <- apply(NIM[1:20, ], 2, sum)
NIMsc[2, ] <- apply(NIM[21:40, ], 2, sum)
NIMout <- apply(NIMsc[, countiesOUT], 1, sum)
NIMin <- apply(NIMsc[, countiesIN], 1, sum)
NIM1yr <- cbind(NIMout, NIMin)
NIM1yr
################################### Need .rds this?

# STEP 3: Create corresponding matrix from LTCSRMig spreadsheets applied
# to 2009 and 2019 age-sex distributions
oeg.out <- 80
POP <- readRDS(paste0(path2inputs, "BaseASD/census2019asd5.rds"))
POP2019 <- reducePOP(POP, oeg.out)
POP <- readRDS(paste0(path2inputs, "BaseASD/census2009asd5.rds"))
POP2009 <- reducePOP(POP, oeg.out)
# Calculate aggregate age-sex distributions for NetOut and NetIn counties
POP2009out <- apply(POP2009[, countiesOUT], 1, sum)
POP2019out <- apply(POP2019[, countiesOUT], 1, sum)
POP2009in <- apply(POP2009[, countiesIN], 1, sum)
POP2019in <- apply(POP2019[, countiesIN], 1, sum)
LTCSRMigIn <- cbind(POP2009[, 1], POP2019[, 1], 
                    POP2009out , POP2019out, POP2009in, POP2019in)
colnames(LTCSRMigIn)[1:2] <- c("POP2009tot", "POP2019tot")
LTCSRMigIn
write.csv(LTCSRMigIn, paste0(path2inputs, "NIMR/LTCSRMigInputs.csv"))

# STEP 3B: Estimate net migration using LTCSRMig spreadsheets for females and
# males for out-migration counties and in-migration counties, enter results
LTCSRMig <- NIM1yr
LTCSRMig[, ] <- 0 
LTCSRMig[1, ] <- c(-151948, 126785)
LTCSRMig[2, ] <- c(-143151, 130201)
LTCSRMig

# STEP 4: Calculate adjustment factors
AdjFac <- round(LTCSRMig / NIM1yr, 3)
AdjFac
saveRDS(AdjFac, paste0(path2inputs, "NIMR/AdjFac.rds"))

