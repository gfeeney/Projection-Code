metadata <- readRDS("metadata.rds")
source(paste0(metadata$paths$path2R, "projection.R"))
ASDrows <- metadata$age$ASDrows
subareas <- metadata$place$subareas
path2inputs <- metadata$paths$path2inputs

# INPUT: NIM1x1, Adjustment Factors

# OUTPUT 1: NIMRa No migration
# OUTPUT 2: NIMRb Migration raw
# OUTPUT 3: NIMRc Migration adjusted (LTCRSMig)
# OUTPUT 4: NIMRd Migration adjusted plus declining rates

# Calculate NIM5x5 directly from census 1 year ago numbers
NIM1x1 <- readRDS(paste0(path2inputs, "NIMR/NIM1x1.rds"))
NIM5x5 <- calculate.NIM5x5(NIM1x1, md = metadata)
ASD5 <- readRDS(paste0(path2inputs, "BaseASD/census2019asd5.rds"))
NIMR5x5 <- NIM5x5 / ASD5[, -1]  # 1st column of ASD5 is Kenya
NIMR5x5 <- round(NIMR5x5, 3)
# View(NIMR5x5)  # Rising rates over age 65 unacceptable
NIMR5x5 <- impute.NIMR5x5.65plus(NIMR5x5, Fac = 0.4)
NIMR5x5 <- round(NIMR5x5, 4)  # Some rates < -1
NIMR5x5[NIMR5x5 < -1] <- -1  # Replace any cells < -1 by -1
# View(NIMR5x5)
write.csv(NIMR5x5, paste0(path2inputs, "NIMR/NIMR5x5.csv"))

# ASSUMPTION B
NIMRb <- calculate.NIMR(NIMR5x5, md = metadata)
saveRDS(NIMRb, paste0(path2inputs, "NIMR/NIMRb.rds"))

# ASSUMPTION A
NIMRa <- lapply(NIMRb, function(x){x[ , ] <- 0; return(x)})
saveRDS(NIMRa, paste0(path2inputs, "NIMR/NIMRa.rds"))

# ASSUMPTION C
# NIMRc Adjust NIM5x5 by LTSCRMig adjustment factors and recalculate NIMR
AdjFac <- readRDS(paste0(path2inputs, "NIMR/AdjFac.rds"))
countiesOUT <- readRDS(paste0(path2inputs, "NIMR/countiesOUT.rds"))
countiesIN <- readRDS(paste0(path2inputs, "NIMR/countiesIN.rds"))
NIM5x5[1:20, countiesOUT] <- 
  NIM5x5[1:20, countiesOUT] * AdjFac["Female", "NIMout"]
NIM5x5[20 + 1:20, countiesOUT] <- 
  NIM5x5[20 + 1:20, countiesOUT] * AdjFac["Male",   "NIMout"]
NIM5x5[1:20, countiesIN] <- 
  NIM5x5[1:20, countiesIN] * AdjFac["Female", "NIMin"]
NIM5x5[20 + 1:20, countiesIN]  <- 
  NIM5x5[20 + 1:20, countiesIN] * AdjFac["Male", "NIMin"]
ASD5 <- readRDS(paste0(path2inputs, "BaseASD/census2019asd5.rds"))
NIMR5x5 <- NIM5x5 / ASD5[, -1]  # 1st column of ASD5 is Kenya
# View(round(NIMR5x5, 1))  # Rising rates over age 65 unacceptable
NIMR5x5 <- impute.NIMR5x5.65plus(NIMR5x5, Fac = 0.4)
NIMR5x5 <- round(NIMR5x5, 4)
# View(NIMR5x5)
NIMRc <- calculate.NIMR(NIMR5x5, md = metadata)
saveRDS(NIMRc, paste0(path2inputs, "NIMR/NIMRc.rds"))

# ASSUMPTION D
# Introduce trend to reverse 2009-2019 increase
# Crude Net Migration Rate doubled between 2009 and 2019 
# Reverse this increase between pcycle1 and pcycle3
introduce.trend <- function(NIMRi) {
  # NIMRi is component of NIMR
  NIMRi.out <- NIMRi
  NIMRi.out[, 2] <- NIMRi[, 1] * sqrt(0.5)
  NIMRi.out[, 3:5] <- NIMRi[, 3:5] * 0.5
  return(NIMRi.out)
}
NIMRd <- lapply(NIMRc, introduce.trend)
NIMRd <- lapply(NIMRd, round, 4)
# View(NIMRd[[2]])
saveRDS(NIMRd, paste0(path2inputs, "NIMR/NIMRd.rds"))

