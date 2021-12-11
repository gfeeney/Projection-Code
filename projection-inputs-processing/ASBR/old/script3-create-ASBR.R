metadata <- readRDS("metadata.rds")
path2inputs <- metadata$paths$path2inputs
placenames <- metadata$place$placenames
placenames
pcycles <- metadata$time$pcycles
pcycles

TFRlist <- readRDS(paste0(path2inputs, "TFR.rds"))
ASBRest <- readRDS(paste0(path2inputs, "ASBR/Census2019ASBRestimates.rds"))
SRB <- rep(1.03, times = 48)  # Be sure this is what we want

ASBRn <- ASBRest  # Normalized ASBRs
ASBRn[ , ] <- 0
for (i in 1:dim(ASBRest)[1]) {
  ASBRn[i, ] <- ASBRest[i, ] * 0.2 / sum(ASBRest[i, ])
}

ASBRlist <- vector(mode = "list", length = length(placenames))
names(ASBRlist) <- placenames
for (i in 1:length(placenames)) {
  ASBRtable <- outer(ASBRn[i, ], TFRlist[i, ], FUN = "*")
  SRBi <- rep(SRB[i], length = dim(ASBRtable)[2])
  SRB.ASBR <- rbind(SRBi, ASBRtable)
  rownames(SRB.ASBR)[1] <- "SRB"
  ASBRlist[[i]] <- SRB.ASBR
}

ASBRlist
pathfile <- paste0(path2inputs, "ASBR/ASBR.rds")
saveRDS(ASBRlist, pathfile)
readRDS(pathfile)

