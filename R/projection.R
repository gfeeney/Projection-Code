# ------------------------------ Define Metadata ----------------------------- #
get.metadata <- function(width, oeg, rag, 
                         CensusTime, BaseTime, pcycles, 
                         area, subareas,
                         path2inputs, path2outputs, path2R) {
  # age
  ADrows <- getAG(width, oeg)
  ASDrows <- c(paste0("f", ADrows), paste0("m", ADrows))
  ASDBrows <- c("fBirths", paste0("f", ADrows), "mBirths", paste0("m", ADrows))
  ASBRrows <- paste0("f", ADrows[rag])
  x <- c("Births", ADrows, paste0(oeg + width, "+"), "Total")
  pframerows <- c(paste0("f", x), paste0("m", x))
  nLxrows.pframe <- c(paste0("f", c(ADrows, paste0(oeg + 5, "+"))), 
                      paste0("m", c(ADrows, paste0(oeg + 5, "+"))))
  ADplus5 <- getAG(width, oeg + 5)
  nLxrows.source <- c(paste0("f", ADplus5), paste0("m", ADplus5))
  age <- list(width = width, oeg = oeg, rag = rag,
              ADrows = ADrows, ASDrows = ASDrows,
              ASDBrows = ASDBrows, ASBRrows = ASBRrows,
              pframerows = pframerows, 
              nLxrows.source = nLxrows.source, 
              nLxrows.pframe = nLxrows.pframe)
  
  # time
  time <- list(CensusTime = CensusTime, BaseTime = BaseTime, pcycles =  pcycles)
  
  # place
  area <- "Kenya"
  subareas <- counties
  places <- c(area, subareas)
  codes <- formatC(0:length(subareas), format = "d", width = 2, flag = "0")
  place <- list(area = area, subareas = subareas, places = places, codes = codes)
  
  # paths
  paths <- list(path2inputs = path2inputs, path2outputs = path2outputs, path2R = path2R)
  
  list(age = age, time = time, place = place, paths = paths)
}
# ---------------------------- END Define Metadata --------------------------- #

# --------------------- calculate.projection and friends --------------------- #
construct.pframe <- function(metadata) {
  # Construct empty pframe (called by initialize pframe)
  pframerows <- metadata$age$pframerows
  pframecols <- c("ASDin", "nLx", "Deaths", "ASBR", "Births", 
                  "Survivors", "NIMR", "NIM", "ASDout")
  pframe <- matrix(0, nrow = length(pframerows), ncol = length(pframecols))
  rownames(pframe) <- pframerows
  colnames(pframe) <- pframecols
  pframe[c("fBirths", "mBirths"), c("Births", "Survivors", "NIMR", "NIM", "ASDout")] <- NA
  oeg <- metadata$age$oeg
  width <- metadata$age$width
  rag <- metadata$age$rag
  pframe[paste0(c("f", "m"), paste0(oeg + width, "+")), -2] <- NA
  pframe[pframerows[-(1 + rag)], c("ASBR", "Births")] <- NA
  return(pframe)
}
check.input.data <- function(places, pcycles, nLx, ASBR, NIMR, 
                             md = metadata) {
  # Check consistency of arguments, places and pcycles consistent with metadata,
  # pcycles in nLx-ASBR-NIMR consistent with pcycles argument and metadata, etc
  # to come
}
initialize.projection.pframes <- function(place, BaseASD, nLx, ASBR, NIMR, 
                                          md = metadata) { 
  # Initialize pframe for particular place and projection cycle. Construct the
  # empty pframe, then assign values to the nLx, ASBR, and NIMR columns, and
  # for pcycle1 only, the BaseASD column
  nLxrows.pframe <- md$age$nLxrows.pframe
  nLxrows.source <- md$age$nLxrows.source
  ASBRrows <- md$age$ASBRrows
  ASDrows <- md$age$ASDrows
  pcycles <- md$time$pcycles
  pframes <- lapply(pcycles, function(x){construct.pframe(md)})
  names(pframes) <- pcycles
  
  for (i in 1:length(pcycles)) {
    pcycle <- pcycles[i]
    pframe <- pframes[[i]]
    if (i == 1) {
      pframe[rownames(BaseASD), "ASDin"] <- BaseASD[, place]
    }
    ASDrows <- md$age$ASDrows
    n <- length(ASDrows) / 2
    ASDin <- pframe[ASDrows, "ASDin"]
    pframe[c("fTotal", "mTotal"), "ASDin"] <- c(sum(ASDin[1:n]), sum(ASDin[n + 1:n]))
    nLx_ <- nLx[[place]][, pcycle]
    pframe[nLxrows.pframe, "nLx"] <- nLx_
    n <- length(nLx_) / 2
    pframe[c("fTotal", "mTotal"), "nLx"] <- c(sum(nLx_[1:n]), sum(nLx_[n + 1:n]))
    pframe[c("fBirths", "mBirths"), "nLx"] <- 5
    ASBR_ <- ASBR[[place]][, pcycle][-1]  # Exclude SRB
    pframe[ASBRrows, "ASBR"] <- ASBR_
    pframe["fTotal", "ASBR"] <- 5 * sum(ASBR_)
    pframe["fBirths", "ASBR"] <- ASBR[[place]][, pcycle][1]
    NIMR_ <- NIMR[[place]][, pcycle]
    pframe[ASDrows, "NIMR"] <- NIMR_
    n <- length(NIMR_) / 2
    pframe[c("fTotal", "mTotal"), "NIMR"] <- c(sum(NIMR_[1:n]), sum(NIMR_[n + 1:n])) 
    ASDout <- pframe[ASDrows, "ASDout"]
    pframe[c("fTotal", "mTotal"), "ASDout"] <- c(sum(ASDout[1:n]), sum(ASDout[n + 1:n]))
    pframes[[i]] <- pframe
  }
  attr(pframes, "place") <- place
  return(pframes)
  }
calculate.projection <- function(initialized.pframes, md = metadata) {
  # Calculate projections for initialized pframelist
  # CORRESPONDING MODIFICATION HERE IF NECESSARY; EXISTING IS TOO COMPLICATED, MAKE
  # THE BASE FUNCTIONS SIMPLER, USE A SIMPLE DO LOOP OVER areas IN THE SCRIPTS
  ASBRrows <- md$Age$ASBRrows
  ASDrows <- md$Age$ASDrows
  width   <- md$Age$width
  nLxrows.pframe <- md$Age$nLxrows.pframe
  projection <- initialized.pframes
  for (i in 1:length(projection)) {
    pf0 <- projection[[i]]
    pf1 <- calculate.DeathsIP(pf0, md)
    pf2 <- calculate.SurvivorsIP(pf1)
    pf3 <- calculate.Births1(pf2)
    pf4 <- calculate.Births2(pf3)
    pf5 <- calculate.DeathsB(pf4)
    pf6 <- calculate.SurvivorsB(pf5)
    pf7 <- calculate.NIM(pf6)
    pf8 <- calculate.ASDout(pf7)
    pf9 <- calculate.totals(pf8)
    demographic.equation.check(pf9)
    projection[[i]] <- pf9
    if (i < length(projection)) {
      projection[[i + 1]][, "ASDin"] <- projection[[i]][, "ASDout"]
    }
  }
  attr(projection, "place") <- place
  return(projection)
}
calculate.projections <- function(places, BaseASD, nLx, ASBR, NIMR, 
                                  md = metadata) {
  # Arg: places, BaseASD, nLx, ASBR, NIMR, metadata
  # Val: Projection list matrix, place by pcycle
  projections <- vector(mode = "list", length = length(places))
  names(projections) <- places
  projections.init <- projections
  for (i in 1:length(projections)) {
    place <- names(projections)[i]
    projections.init[[i]] <- 
      initialize.projection.pframes(place, BaseASD, nLx, ASBR, NIMR)
    projections[[i]] <- calculate.projection(projections.init[[i]])
  }
  return(projections)
}
# ----------------- Functions called by calculate.projection ----------------- #
calculate.DeathsIP <- function(pframe, md = metadata) {
  # Deaths of initial population
  width <- md$age$width
  oeg   <- md$age$oeg
  rows <- 2:(1 + oeg/width)
  calculate <- function(pframe, rows) {
    SR <- pframe[rows + 1, "nLx"]/pframe[rows , "nLx"]
    pframe[rows, "Deaths"] <- (1 - SR) * pframe[rows, "ASDin"]
    row <- rows[length(rows)] + 1
    SR <- pframe[row + 1, "nLx"]/sum(pframe[row + 0:1, "nLx"])
    Deaths <- (1 - SR) * pframe[row, "ASDin"]
    pframe[row, "Deaths"] <- Deaths
    return(pframe)
  }
  pframe <- calculate(pframe, rows)
  rows <- rows + dim(pframe)[1]/2
  pframe <- calculate(pframe, rows)
  return(pframe)
}
calculate.SurvivorsIP <- function(pframe, md = metadata) {
  # Survivors of initial population (consider revising)
  n <- dim(pframe)[1]/2  - 5
  rows <- 2:(n + 1)
  calculate <- function(pframe, rows) {
    Survivors <- pframe[rows, "ASDin"] - pframe[rows, "Deaths"]
    pframe[rows + 1, "Survivors"] <- Survivors
    rows <- rows[length(rows)] + 1:2
    Survivors <- sum(pframe[rows, "ASDin"]) - sum(pframe[rows, "Deaths"])
    pframe[rows[2], "Survivors"] <- Survivors 
    return(pframe) 
  }
  pframe <- calculate(pframe, rows)
  rows <- rows + dim(pframe)[1]/2
  pframe <- calculate(pframe, rows)
  return(pframe)
}
calculate.Births1 <- function(pframe, md = metadata) {
  # Births by age of mother (both sexes)
  width <- md$age$width
  ASBRrows <- md$age$ASBRrows
  PYL <- width * (pframe[ASBRrows, "ASDin"] + pframe[ASBRrows, "Survivors"]) / 2
  Births <- pframe[ASBRrows, "ASBR"] * PYL
  pframe[ASBRrows, "Births"] <- Births
  return(pframe)
}
calculate.Births2 <- function(pframe, md = metadata) {
  # Total female births and total male births
  ASBRrows <- md$age$ASBRrows
  BirthsT <- sum(pframe[ASBRrows, "Births"])
  pframe["fTotal", "Births"] <- BirthsT  # Total births in Births column
  srb <- pframe["fBirths", "ASBR"]       # SRB given in 1st row of ASBR column
  BirthsF <- BirthsT * 1/(1 + srb)
  pframe["fBirths", "ASDin"] <- BirthsF
  pframe["mBirths", "ASDin"] <- BirthsT - BirthsF
  return(pframe)
}
calculate.DeathsB <- function(pframe) {
  # Deaths of births
  pdf <- (1 - pframe["f0-4", "nLx"] / pframe["fBirths", "nLx"])
  pframe["fBirths", "Deaths"] <- pframe['fBirths', "ASDin"] * pdf
  pdm <- (1 - pframe["m0-4", "nLx"] / pframe["mBirths", "nLx"])
  pframe["mBirths", "Deaths"] <- pframe['mBirths', "ASDin"] * pdm
  return(pframe)
}
calculate.SurvivorsB <- function(pframe) {
  # Survivors of births
  pframe["f0-4", "Survivors"] <- pframe["fBirths", "ASDin"] - pframe["fBirths", "Deaths"]
  pframe["m0-4", "Survivors"] <- pframe["mBirths", "ASDin"] - pframe["mBirths", "Deaths"]
  return(pframe)
}
calculate.NIM <-  function(pframe, md = metadata) {
  ASDrows <- md$age$ASDrows
  pframe[ASDrows, "NIM"] <- pframe[ASDrows, "Survivors"] * pframe[ASDrows, "NIMR"]
  return(pframe)
}
calculate.ASDout <- function(pframe, md = metadata) {
  ASDrows <- md$age$ASDrows
  pframe[ASDrows, "ASDout"] <- pframe[ASDrows, "Survivors"] + pframe[ASDrows, "NIM"]
  # if (!pframe[ASDrows, "ASDout"] >= 0) {  # Added 08-Oct-2021
  #   stop("Negative numbers in ASDout!")
  # }
  return(pframe)
}
calculate.totals <- function(pframe, md = metadata) {
  nLxrows.pframe <- md$age$nLxrows.pframe
  ASDrows <- md$age$ASDrows
  ASBRrows <- md$age$ASBRrows
  n <- length(ASDrows) / 2
  pframe["fTotal", "Deaths"] <- sum(pframe[c("fBirths", ASDrows[1:n]), "Deaths"])
  pframe["mTotal", "Deaths"] <- sum(pframe[c("mBirths", ASDrows[n + 1:n]), "Deaths"])
  pframe["fTotal", "Survivors"] <- sum(pframe[ASDrows[1:n], "Survivors"])
  pframe["mTotal", "Survivors"] <- sum(pframe[ASDrows[n + 1:n], "Survivors"])
  pframe["fTotal", "NIM"] <- sum(pframe[ASDrows[1:n], "NIM"])
  pframe["mTotal", "NIM"] <- sum(pframe[ASDrows[n + 1:n], "NIM"])
  pframe["fTotal", "ASDout"] <- sum(pframe[ASDrows[1:n], "ASDout"])
  pframe["mTotal", "ASDout"] <- sum(pframe[ASDrows[n + 1:n], "ASDout"])
  return(pframe)
}
demographic.equation.check <- function(pframe, md = metadata) {
  pop1 <- pframe[c("fTotal"), "ASDin"] + pframe[c("mTotal"), "ASDin"]
  pop2 <- pframe[c("fTotal"), "ASDout"] + pframe[c("mTotal"), "ASDout"]
  growth1 <- pop2 - pop1
  births <- pframe["fTotal", "Births"]
  deaths <- pframe[c("fTotal"), "Deaths"] + pframe[c("mTotal"), "Deaths"]
  natinc <- births - deaths
  netmig <- pframe[c("fTotal"), "NIM"] + pframe[c("mTotal"), "NIM"]
  growth2 <- natinc + netmig
  if (abs(growth1 - growth2) > 0.01) {
    stop("demographic.equation.check failed")
  }
}
# ------------ END Functions called by calculate.projection ------------------ #

# -------------- calculate.consistent.projections and friends ---------------- #
transpose.ListMatrix <- function(LM) {
  # LM: List with 1 component for each subnational area, each component the 
  # list of pframes for the subnational area
  # pcycle for this area
  cnames <- names(LM[[1]])
  LMt <- lapply(cnames, function(x) {NULL})
  names(LMt) <- cnames
  rnames <- names(LM)
  for (i in 1:length(cnames)) {
    LMt[[i]] <- lapply(rnames, function(x) {NULL})
    names(LMt[[i]]) <- rnames
  }
  for (i in 1:length(cnames)) {
    for (j in 1:length(rnames)) {
      LMt[[i]][[j]] <- LM[[j]][[i]]
    }
  }
  return(LMt)
}
transpose.ListMatrix2 <- function(LM) {
  # Value: List with 1 component for each subnational 
  # area, each component the list of pframes for each 
  # pcycle for this area
  cnames <- names(LM[[1]])
  LMt <- lapply(cnames, function(x) {NULL})
  names(LMt) <- cnames
  rnames <- names(LM)
  for (i in 1:length(cnames)) {
    LMt[[i]] <- lapply(rnames, function(x) {NULL})
    names(LMt[[i]]) <- rnames
  }
  for (i in 1:length(cnames)) {
    for (j in 1:length(rnames)) {
      LMt[[i]][[j]] <- LM[[j]][[i]]
    }
  }
  return(LMt)
}
calculate.subnational.pcycle <- function(Nat, SubNat, md = metadata) {
  # Nat:    CALCULATED national pframe for a pcycle  # md <- metadata
  # SubNat: List of INITIALIZED subnational pframes for the same pcycle
  # Value:  List of COMPLETED subnational pframes consistent with national pframe
  ASDrows  <- md$age$ASDrows
  ASDBrows <- md$age$ASDBrows
  width <- md$age$width
  oeg <- md$age$oeg
  rag <- md$age$rag

  # Initialize matrix of reconciliation parameters
  rMatrix <- matrix(0, nrow = length(ASDBrows), ncol = 3)
  rownames(rMatrix) <- ASDBrows
  colnames(rMatrix) <- c("Deaths", "Births", "NetMigs")
  
  SubNat <- lapply(SubNat, calculate.DeathsIP)
  adj <- CFA.calculate(SubNat, Nat, ASDrows, "Deaths")
  SubNat <- CFA.adjust(SubNat, ASDrows, "Deaths", adj)
  CFA.check(SubNat, Nat, ASDrows, "Deaths")
  rMatrix[ASDrows, "Deaths"] <- adj
  
  SubNat <- lapply(SubNat, calculate.SurvivorsIP)
  
  SubNat <- lapply(SubNat, calculate.Births1)
  Brows <- ASDrows[rag]
  adj <- CFA.calculate(SubNat, Nat, Brows, "Births")
  SubNat <- CFA.adjust(SubNat, Brows, "Births", adj)
  CFA.check(SubNat, Nat, Brows, "Births")
  rMatrix[Brows, "Births"] <- adj
  
  SubNat <- lapply(SubNat, calculate.Births2)
  
  SubNat <- lapply(SubNat, calculate.DeathsB)
  DBrows <- c("fBirths", "mBirths")
  adj <- CFA.calculate(SubNat, Nat, DBrows, "Deaths")
  SubNat <- CFA.adjust(SubNat, DBrows, "Deaths", adj)
  CFA.check(SubNat, Nat, DBrows, "Deaths")
  rMatrix[DBrows, "Deaths"] <- adj
  
  SubNat <- lapply(SubNat, calculate.SurvivorsB)
  
  SubNat <- lapply(SubNat, calculate.NIM)
  x <- x.calculate(SubNat, ASDrows,"NIM")
  SubNat <- x.adjust(SubNat, ASDrows, "NIM", x)
  x.check(SubNat, ASDrows, "NIM")
  
  rMatrix[ASDrows, "NetMigs"] <- x
  
  SubNat <- lapply(SubNat, calculate.ASDout)
  
  attr(SubNat, "rMatrix") <- rMatrix
  return(SubNat)
}
calculate.consistent.projections <- function(np, snpi, md = metadata) {
  # Arg: np = national projection
  # Arg: snpi = Initialized subnational projection List Matrix (5x47)
  # Val: snp = consistent subnational projection List Matrix (5x47)
  snp <- snpi
  rowsASD <- metadata$age$ASDrows
  for (i in 1:length(snpi)) {
    SubNat <- snpi[[i]]
    Nat <- np[[i]]
    if (i > 1) {
      SubNat.last <- snp[[i - 1]]
      for (j in 1:length(SubNat)){
        SubNat[[j]][rowsASD, "ASDin"] <- SubNat.last[[j]][rowsASD, "ASDout"]
      }
    }
    snp[[i]] <- calculate.subnational.pcycle(Nat, SubNat)
  }
  return(snp)
}
CFA.calculate <- function(SubNat, Nat, rows, column) {
  cm <- do.call(cbind, lapply(SubNat, function(x) x[rows, column]))
  cfa <- Nat[rows, column] / apply(cm, 1, sum)
  return(cfa)
}
CFA.adjust <- function(SubNat, rows, column, adj) {
  lapply(SubNat, function(x){x[rows, column] <- x[rows, column] * adj; return(x)})
}
CFA.check <- function(SubNat, Nat, rows, column) {
  cm <- lapply(SubNat, function(x){x[rows, column]})
  round(Nat[rows, column] - Reduce('+', cm), 10)
}  
x.calculate <- function(SubNat, rows, column) {
  # Calculate x that will be used to adjust Positive and Negative NIM
  # so that sum of NIM over all subnational areas is zero
  
  cm <- do.call(cbind, lapply(SubNat, function(x) x[rows, column]))
  Pmig <- cm * as.numeric(cm > 0)
  P <- apply(Pmig, 1, sum)
  Nmig <- cm * as.numeric(cm < 0)
  N <- -apply(Nmig, 1, sum)
  x   <- (P - N) / (P + N)  # adjustment factors are 1 - x and 1 + x
  #x[is.infinite(x) | is.nan(x)] <- 0
  return(x)
}
x.adjust <- function(SubNat, rows, column, x) {
  # See pages ... of Consistent Subnational Population Projection (Feeney 2017)
  cm <- do.call(cbind, lapply(SubNat, function(x) x[rows, column]))
  Pmig <- cm * as.numeric(cm > 0)
  P <- apply(Pmig, 1, sum)
  Nmig <- cm * as.numeric(cm < 0)
  Padj <- Pmig * matrix(1 - x, nrow=length(x), ncol=dim(Pmig)[2])
  Nadj <- Nmig * matrix(1 + x, nrow=length(x), ncol=dim(Nmig)[2])
  NIM <- Padj + Nadj
  for (i in 1:length(SubNat)) {
    SubNat[[i]][rows, "NIM"] <- NIM[, i]
  }
  return(SubNat)
}
x.check <- function(SubNat, rows, column) {
  cm <- lapply(SubNat, function(x){x[rows, column]})
  round(Reduce('+', cm), 10)
}
# ------- END calculate.consistent.subnational.projections and friends ------- #

# --------------- Functions for processing projection outputs ---------------- #
get.projectedASD <- function(projection, md = metadata){
  # Arg: projection (list of pframes)
  # Val: matrix of base and projected age distributions, female over male
  ASDrows <- md$age$ASDrows
  BaseTime <- md$time$BaseTime
  x1 <- lapply(projection, function(x){x[ASDrows, "ASDout"]})
  x2 <- do.call(cbind, x1)
  x3 <- cbind(projection[[1]][ASDrows, "ASDin"], x2) # For BaseASD
  colnames(x3) <- BaseTime + 5 * (1:dim(x3)[2] - 1)
  return(x3)
}
asd5x1.from.asd5x5 <- function(asd5x5) {
  # ad5x5: matrix of 5 year age groups at 5 year time intervals
  # ad5x1: matrix of 5 year age groups at 1 year time intervals
  interpolate <- function(ad5x5){
    x <- as.numeric(colnames(ad5x5))
    xout <- (x[1] - 1) + 1:(1 + 5 * (length(x) - 1))
    ad5x1 <- matrix(0, nrow = dim(ad5x5)[1], ncol = length(xout))
    rownames(ad5x1) <- rownames(ad5x5)
    colnames(ad5x1) <- xout
    for (i in 1:dim(ad5x5)[1]) {
      ad5x1[i, ] <- approx(x, ad5x5[i, ], xout)$y
    }
    return(ad5x1)
  }
  n <- dim(asd5x5)[1] / 2
  adf5x1 <- interpolate(asd5x5[1:n, ])
  adm5x1 <- interpolate(asd5x5[n + 1:n, ])
  asd5x1 <- rbind(adf5x1, adm5x1)
  return(asd5x1)
}
get.AgeSexDistribution <- function(projection, md = metadata){
  ASDrows <- md$age$ASDrows
  BaseTime <- md$time$BaseTime
  x1 <- lapply(projection, function(x){x[ASDrows, "ASDout"]})
  x2 <- do.call(cbind, x1)
  x3 <- cbind(projection[[1]][ASDrows, "ASDin"], x2)
  colnames(x3) <- BaseTime + 5 * (1:dim(x3)[2] - 1)
  sex <- c("female", "male", "both")
  age <- md$age$ADrows
  time <- colnames(x3)
  asd <- array(0, dim = c(length(age), length(time), length(sex)),
               dimnames = list(age = age, time = time, sex = sex))
  n <- dim(x3)[1] / 2
  asd[, , "female"] <- x3[1:n, ]
  asd[, , "male"] <- x3[n + 1:n, ]
  asd[, , "both"] <- asd[, , "female"] + asd[, , "male"]
  attr(asd, "place") <- attr(projection[[1]], "place")
  return(asd)
}
get.projectionResults <- function(projection) {
  area <- attr(projection, "area")
  asd <- get.AgeSexDistribution(projection)
  sd <- t(apply(asd, c(2, 3), sum))
  pop <- apply(sd, 2, sum)
  projectionResults <- list(Area = area,
                            AgeSexDistribution = asd,
                            SexDistribution = sd, 
                            TotalPopulation = pop)
  return(projectionResults)
}
get.projTotals <- function(proj, md = metadata) {
  # Arg: proj is a projection (list of pframes)
  # Val: Vector giving base and projected population totals
  out <- sapply(proj, function(x){sum(x[md$age$ASDrows, "ASDin"])})[1]
  out <- c(out, sapply(proj, function(x){sum(x[md$age$ASDrows, "ASDout"])}))
  names(out) <- 2020 + 5 * 0:length(md$time$pcycles)
  out <- round(out, 0)
  return(out)
}
sort.PlacesByGrowth <- function(totals) {
  # Arg: totals is matrix of projection totals, place by pcycle
  # Val: Growth ratio column appended, table sorted by growth
  n <- dim(totals)[2]
  Growth <- round(100 * totals[, n] /totals[, 1], 0)
  out <- cbind(totals, Growth)
  out <- out[rev(order(out[, n + 1])), ]
  return(out)
}

get.ComponentsMatrix <- function(projection, md = metadata) {
  # Arg: A projection
  # Value: A matrix showing components for each projection cycle
  get.components <- function(pframe, md = metadata) {
    # Arg: A single calculated projection frame
    # Value: Vector of demographic equation terms
    ASDrows <- metadata$age$ASDrows
    pop1 <- sum(pframe[ASDrows, "ASDin"])
    pop2 <- sum(pframe[ASDrows, "ASDout"])
    growth1 <- pop2 - pop1
    rag <- metadata$age$rag
    births <- sum(pframe[1 + rag, "Births"])
    ASDBrows <- metadata$age$ASDBrows
    deaths <- sum(pframe[ASDBrows, "Deaths"])
    natinc <- births - deaths
    netmig <- sum(pframe[ASDrows, "NIM"])
    growth2 <- natinc + netmig
    out <- c(pop1, pop2, growth1, births, deaths, natinc, netmig, growth2)
    names(out) <- c("Pop1", "Pop2", "Growth1", "Births", "Deaths", "NatInc",
                    "NetMig", "Growth2")
    return(out)
  }
  components <- matrix(0, nrow = length(pcycles), ncol = 8)
  rownames(components) <- pcycles
  colnames(components) <- c("Pop1", "Pop2", "Growth1", "Births", "Deaths",
                            "NatInc", "NetMig", "Growth2")
  for (i in 1:length(pcycles)) {
    components[i, ] <- get.components(projection[[i]])
  }
  components <- round(components, 0)
  return(t(components))
}
# ------------- END Functions for processing projection outputs -------------- #

# ---------------------------- Plotting functions ---------------------------- #
draw.dotplot <- function(xlab, xlim, xatv, catvalues, pchArg = 1, LOG2 = TRUE) 
  {
  # catvalues must be named with category names!
  #browser()
  ylim <- c(0, 1 + length(catvalues))
  plot(xlim, ylim, type="n", xlab="", xaxt="n", xaxs="i", ylab="", yaxt="n", yaxs="i")
  axis(1, at = xatv)
  if (LOG2 == TRUE) {
    mtext(paste0(xlab, " - LogBase2"), side = 1, line = 2.5)
    axis(3, at = xatv, labels = 2^xatv)  
  }
  if (LOG2 == FALSE) {
    mtext(xlab, side = 1, line = 2.5)
    axis(3, at = xatv)  
  }
  mtext(xlab, side = 3, line = 2.5)
  for (i in 2:(length(xatv) - 1)) {
    lines(c(xatv[i], xatv[i]), ylim, lwd = 0.25, lty = "solid", col = gray(0.40))
  }
  axis(2, at = 1:length(catvalues), labels = names(catvalues), tick = FALSE, las = 2, line = -0.5, cex = 0.5)
  for (i in 1:length(catvalues)) {
    lines(xlim, c(i,i), lwd = 0.75, lty = "dashed", col = gray(0.25))
  }
  points(catvalues, 1:length(catvalues), pch = pchArg, cex = 1.5)
}
draw.grid <- function(xlim, ylim, xatv, yatv, xaxs, yaxs, grayvalue=0.75) {
  # Horizontal lines
  if (xaxs == "r") {
    xf <- 0.04
  } else if (xaxs == "i") {
    xf <- 0
  } else {
    stop("Invalid xaxs value")
  }
  xlims <- c(xlim[1] - xf*(xlim[2]-xlim[1]), xlim[2]+xf*(xlim[2]-xlim[1]))
  # Later: if xf=0 we want to exclude the first and last lines
  for (i in yatv) {
    lines(xlims, c(i, i), col=gray(grayvalue))
  }
  
  # Vertical lines
  if (yaxs == "r") {
    xf <- 0.04
  } else if (yays == "i") {
    xf <- 0
  }
  ylims <- c(ylim[1] - xf*(ylim[2]-ylim[1]), ylim[2] + xf*(ylim[2]-ylim[1]))
  # Later: as above
  for (i in xatv) {
    lines(c(i, i), ylims, col=gray(grayvalue))
  }
}
construct.plot.frame <- function(xlabel, xlim, ylabel, ylim) {
  par(mar=c(4, 4, 4, 4))  # Set margins
  plot(xlim, ylim, type = "n", xlab = "", xaxt="n", xaxs = "r",
       ylab="", yaxt = "n", yaxs = "r", cex.axis = 0.5)
  
  xave <- sum(xlim)/2
  tickinterval <- (xlim[2] - xlim[1]) / 4
  xatv <- xlim[1] + tickinterval * 0:4
  axis(1, at = xatv)
  mtext(xlabel, side = 1, line = 2.5)
  axis(3, at = xatv)
  mtext(xlabel, side = 3, line = 2.5)
  
  tickinterval <- (ylim[2] - ylim[1]) / 4
  yatv <- ylim[1] + tickinterval * 0:4
  axis(2, at = yatv)
  mtext(ylabel, side = 2, line = 2.5)
  axis(4, at = yatv)
  mtext(ylabel, side = 4, line = 2.5)
  
  draw.grid(xlim, ylim, xatv, yatv, xaxs = "r", yaxs = "r", grayvalue = 0.75)
}
calculate.ymax <- function(x) {
  y <- 1:100
  y1 <- y[y / 4 == floor(y / 4)]
  y <- c(y1, 10 * y1, 100 * y1)
  ymax <- min(y[y >= x])
  return(ymax)
}
plot.asd <- function(asd, xlabel = "Age", xlim = c(0, 100), 
                     ylabel = "Number (000)", Place = "") {
  y <- asd / 1000
  ymax <- calculate.ymax(max(y))
  x <- 5 * 1:dim(y)[1] - 2.5
  construct.plot.frame(xlabel = "Age", xlim = c(0, 100), 
                       ylabel = "Number (000)", ylim = c(0, ymax) )
  for (i in 1:dim(y)[2]) {
    lines(x, y[, i], lwd = 2, col = gray(0.6))
    if (i == 1) {
      lines(x, y[, i], lwd = 2, col = "black")  
    }
    if (i == dim(asd)[2]) {
      lines(x, y[, i], lwd = 2, col = "red")  
    }
  }
  text(175/2, ymax - ymax/16, Place, adj = 0.5, cex = 1.5)
}
# -------------------------- END Plotting functions -------------------------- #

# --------------------------- Projection Utilities --------------------------- #
extend.projection <- function(nLx, ASBR, NIMR, times = 5) {
  for (i in 1:times) {
    nLx <- lapply(nLx, function(x){cbind(x, x[, dim(x)[2]])})
    ASBR <- lapply(ASBR, function(x){cbind(x, x[, dim(x)[2]])})
    NIMR <- lapply(NIMR, function(x){cbind(x, x[, dim(x)[2]])})    
  }
  nLx <- lapply(nLx, function(x){colnames(x) <- paste0("pcycle", 1:dim(x)[2]); return(x)})
  ASBR <- lapply(ASBR, function(x){colnames(x) <- paste0("pcycle", 1:dim(x)[2]); return(x)})
  NIMR <- lapply(NIMR, function(x){colnames(x) <- paste0("pcycle", 1:dim(x)[2]); return(x)})
  assign("nLx", nLx, envir = .GlobalEnv)
  assign("ASBR", ASBR, envir = .GlobalEnv)
  assign("NIMR", NIMR, envir = .GlobalEnv)
}
# ------------------------- END Projection Utilities ------------------------- #

# ------------------ Age distribution analysis and adjustment ---------------- #
getAG <- function(width, oeg) {
  # Arg: width of age groups, beginning of open ended age group
  # Val: Character vector of age groups labels
  if (oeg / width != floor(oeg / width)) {
    stop("Open-ended group is not integral multiple of width of age group")
  }
  if (width == 1) {
    return(c(0:(oeg - 1), paste0(oeg, "+")))
  } else {
    ngroups <- oeg / width
    left <- width * 0:(ngroups - 1)
    x <- c(paste(left, left + width - 1, sep = "-"), paste0(oeg, "+"))
    return(x)
  }
}
unZigZag <- function(x, i = 7 + 2 * 0:6, plotout = FALSE) {
  # x - age distribution, five year groups for index min(i) and above
  # i indexes of age groups beginning with ages ending in '0' (e.g.
  # 30-34, 40-44, . . . ) from which some fraction is to be distributed
  # to surrounding age groups
  out <- matrix(0, nrow = length(x), ncol = 3)
  # rownames(out) <- names(ad5)
  colnames(out) <- c("x", "xfer", "y")
  out[, "x"] <- x
  out[, "xfer"] <- NA
  xfer <- rep(0, times = length(i))
  calculate.rough <- function(xfer) {
    y <- x
    y[i] <- x[i] - xfer
    y[i - 1] <- x[i - 1] + xfer / 2
    y[i + 1] <- y[i + 1] + xfer / 2
    sum((y[i] - ((y[i - 1] + y[i + 1]) / 2))^2) / 1000000
  }
  z <- calculate.rough(xfer)
  z
  yrough <- function(y, i) { # produces the initial rough estimate for transfer
    y[i] - (y[i - 1] + y[i + 1])/2
  }
  yx <- yrough(x, i)
  xfer <- optim(yx, calculate.rough, control = list(maxit = 500))$par
  out[i, "xfer"] <- xfer  # round(xfer, 0)
  y <- x
  y[i] <- x[i] - xfer
  y[i - 1] <- x[i - 1] + xfer / 2
  y[i + 1] <- y[i + 1] + xfer / 2
  out[, "y"] <- y  # round(y, 0)
  if (abs(sum(y) - sum(x)) > 0.4) {  # Per Sam 9/19/2021, != check fails with -1.12e-10 diff
    stop("unZigZag: abs(sum(y) - sum(x)) > 0.4")
  }
  if (plotout == TRUE) {
    uZZ <- out
    plot.unZigZag <- function(uZZ) {
      x <- 5 * 1:20 - 2.5
      y1 <- uZZ[, "x"]
      y2 <- uZZ[, "y"]
      plot(x, y1, cex = 2)
      lines(x, y1)
      points(x, y2, pch = 16)
      lines(x, y2)
    }
    plot.unZigZag(uZZ)
  }
  return(out)
}
plot.unZigZag.ad5 <- function(out) {
  x <- 5 * 1:20 - 2.5
  y1 <- out[, "x"]
  y2 <- out[, "y"]
  plot(x, y1, cex = 2)
  lines(x, y1)
  points(x, y2, pch = 16)
  lines(x, y2)
}
modified.midpoint.interpolation1 <- function(x, k) {
  # x - 5 year age distribution, k - adjustment factors for
  # 5 year age groups, length(k) == length(x) - 1
  
  mmpi.base <- function(x, k) {
    # Initialize table1
    # mmpi.base(x, k)
    table1 <- matrix(0, nrow = length(x), ncol = 5)
    rownames(table1) <- names(x)
    colnames(table1) <- c("x", "k", "xk", "xf", "df^2")
    n1 <- length(x) - 1
    table1[,"x"] <- x
    table1[1:n1, "k"] <- k
    table1[1:n1, "xk"] <- x[1:n1] * k
    table1[dim(table1)[1], "xf"] <- table1[dim(table1)[1], "x"]
    table1[dim(table1)[1], c("k", "xk")] <- NA
    # Initialize table2
    AG <- getAG(1, 95)
    table2 <- matrix(0, nrow = length(AG), ncol = 3)
    rownames(table2) <- AG
    colnames(table2) <- c("y1", "y2", "y3")
    table2[dim(table2)[1], ] <- table1[dim(table1)[1], "x"]
    # Calculate y1
    xin <- 5 * 1:n1 - 3 
    yin <- table1[1:n1, "xk"] / 5
    n2 <- 5 * n1
    xout <- 1:n2 - 1
    y1 <- approx(xin, yin, xout)$y
    names(y1) <- 1:length(y1) - 1
    changeleft <- y1[4] - y1[3]
    y1[1:2] <- y1[3] - 2:1 * changeleft
    changeright <- y1[n2 - 3] - y1[n2 - 4]
    y1[(n2 - 1): n2] <- y1[n2 - 2] + 1:2 * changeright
    x1 <- 1:length(y1) - 0.5
    # plot(x1, y1)
    # lines(x1, y1)
    table2[1:n2, "y1"] <- y1
    # Caclculate y2
    y2 <- y1
    i <- 2:(length(y2) - 2)
    y2[i] <- (y1[i - 1] + y1[i] + y1[i + 1]) / 3
    # plot(x1, y2)
    # lines(x1, y2)
    table2[1:n2, "y2"] <- y2
    # Calculate y3
    y3 <- y2
    y3[i] <- (y2[i - 1] + y2[i] + y2[i + 1]) / 3
    # plot(x1, y3)
    # lines(x1, y3)
    table2[1:n2, "y3"] <- y3
    # Finalize table1
    table1[1:n1, "xf"] <- apply(matrix(table2[1:n2, "y3"], nrow = n1, ncol = 5, byrow = TRUE), 1, sum)
    table1[1:n1,"df^2"] <- ((table1[1:n1, "x"] - table1[1:n1, "xf"])^2)/1000
    ssd <- sum(table1[,"df^2"])
    # Return results
    list(table1 = table1, table2 = table2, ssd = ssd)
  }
  
  # Define function for optim()
  ssd.from.k <- function(k) {
    mmpi.base(x, k)$ssd
  }
  
  # Apply optim() to get factors that minimize SSD
  out <- optim(k, ssd.from.k, method = "BFGS")
  
  if(abs(out$value) > 0.001) {
    #stop("abs(out$value) > 0.001")
    cat("\n")
    cat(paste0("Problem with #", j, "\n"))
    cat(paste0(x, sep = " "))
    cat("\n")
    cat(paste0(k, sep = " "))
    cat("\n")
  }
  
  # Use these factors to produce mmpi.base() output
  mmpi.base(x, out$par)
}
modified.midpoint.interpolation <- function(ad5, k) {
  # x: 5 year age distribution with oeg
  # k: adj factors 5yr age groups
  # No k for oeg, length(k) == length(x) - 1
  
  mmpi.base <- function(ad5, k) {
    # ad5: age dist 5yr groups with oeg
    # k: adj factors for 5 yr groups (NOT oeg)
    # length(k) == length(ad5) - 1
    # Initialize table1
    table1 <- matrix(0, nrow = length(ad5), ncol = 6)
    rownames(table1) <- names(ad5)
    colnames(table1) <- c("ad5", "ad5+", "k", "ad5+*k", "sumsy", "df^2")  # k = factors
    n1 <- length(ad5) - 1
    table1[,"ad5"] <- ad5
    CONSTANT <- 0
    if (min(ad5) <= 0) {
      CONSTANT <- round(1.25 * abs(min(ad5)), 0)  # Used in table2 as well
    }
    table1[, "ad5+"] <- ad5 + CONSTANT
    table1[1:n1, "k"] <- rep(1, times = n1)
    table1[1:n1, "ad5+*k"] <- table1[1:n1, "ad5+"] * k
    table1[dim(table1)[1], c("ad5+", "k", "ad5+*k", "sumsy", "df^2")] <- NA
    
    # Initialize table2
    AG <- getAG(1, 95)
    table2 <- matrix(0, nrow = length(AG), ncol = 4)
    rownames(table2) <- AG
    colnames(table2) <- c("y1+", "y2+", "y3+", "syint")
    table2[dim(table2)[1], 1:3] <- NA
    # Calculate y1: linear interpolation/extrapolation
    xin <- 5 * 1:n1 - 3  # Middle ages of 5 year age groups
    yin <- table1[1:n1, "ad5+*k"] / 5
    n2 <- 5 * n1
    xout <- 1:n2 - 1
    y1 <- approx(xin, yin, xout)$y
    names(y1) <- 1:length(y1) - 1
    changeleft <- y1[4] - y1[3]
    y1[1:2] <- y1[3] - 2:1 * changeleft
    changeright <- y1[n2 - 3] - y1[n2 - 4]
    y1[(n2 - 1): n2] <- y1[n2 - 2] + 1:2 * changeright
    x1 <- 1:length(y1) - 0.5
    # plot(x1, y1)
    # lines(x1, y1)
    table2[1:n2, "y1+"] <- y1
    # Calculate y2: 3 point moving average (round piecewise linear joins)
    y2 <- y1
    i <- 2:(length(y2) - 2)
    y2[i] <- (y1[i - 1] + y1[i] + y1[i + 1]) / 3
    # plot(x1, y2)
    # lines(x1, y2)
    table2[1:n2, "y2+"] <- y2
    # Calculate y3: another 3 point moving average (round a bit more)
    y3 <- y2
    y3[i] <- (y2[i - 1] + y2[i] + y2[i + 1]) / 3
    # plot(x1, y3)
    # lines(x1, y3)
    table2[1:n2, "y3+"] <- y3
    # Calculate syint (take out CONSTANT)
    table2[1:n2, "syint"] <- y3 - CONSTANT / 5
    table2[dim(table2)[1], "syint"] <- table1[dim(table1)[1], "ad5"]
    
    # Finalize table1: sumsy is sum of single year values in 5 year age groups
    #                  df^2 is squared differences of ad5*k and sumsy
    table1[1:n1, "sumsy"] <- apply(matrix(table2[1:n2, "y3+"], nrow = n1, ncol = 5, byrow = TRUE), 1, sum)
    table1[1:n1,"df^2"] <- ((table1[1:n1, "ad5"] - table1[1:n1, "sumsy"])^2)
    table1[1:n1,"df^2"] <- round(table1[1:n1,"df^2"], 4)
    ssd <- sum(table1[1:n1, "df^2"])
    # Return results
    list(table1 = table1, CONSTANT = CONSTANT, table2 = table2, ssd = ssd)
  }
  
  # Define function for optim()
  ssd.from.k <- function(k) {
    mmpi.base(ad5, k)$ssd
  }
  
  # Apply optim() to get factors that minimize SSD
  out <- optim(k, ssd.from.k, method = "BFGS")
  
  if(abs(out$value) > 0.01) {
    #stop("abs(out$value) > 0.01")
    cat("\n")
    cat(paste0("Problem with #", j, "\n"))
    cat(paste0(ad5, sep = " "))
    cat("\n")
    cat(paste0(k, sep = " "))
    cat("\n")
  }
  
  # Use these factors to produce mmpi.base() output
  mmpi.base(ad5, out$par)
}
reduce.oeg <- function(ad5, oeg.out) {
  # Input age distribution 5 year groups
  # Check that oeg argument is multiple of 5 and lower than oeg for ad5
  oeg.in <- 5 * length(ad5) - 5
  n <- oeg.out / 5
  ad5.out <- rep(0, times = n + 1)
  names(ad5.out) <- getAG(5, oeg.out)
  ad5.out[1:n] <- ad5[1:n]
  ad5.out[length(ad5.out)] <- sum(ad5[(n + 1):length(ad5)])
  return(ad5.out)
}
reducePOP <- function(POP, oeg.out) {
  POP <- as.data.frame(POP)
  n <- dim(POP)[1] / 2
  Female <- round(sapply(POP[1:n, ], reduce.oeg, oeg.out), 0)
  rownames(Female) <- paste0("f", rownames(Female))
  Male   <- round(sapply(POP[n + 1:n, ], reduce.oeg, oeg.out), 0)
  rownames(Male) <- paste0("m", rownames(Male))
  return(rbind(Female, Male))
}
calculate.asgr <- function(x, md = metadata) {
  # x - matrix of age distributions at equidistant times t1, t2, . . .
  asgr <- matrix(0, nrow = dim(x)[1], ncol = dim(x)[2] - 1)
  rownames(asgr) <- rownames(x)
  colnames(asgr) <- md$time$pcycles
  for (j in 1:dim(asgr)[2]) {
    asgr[, j] <- log(x[, j + 1] / x[, j]) / 5
    }
  return(asgr)  
}
# ---------------- END Age distribution analysis and adjustment -------------- #

# ---------------------------- Migration functions --------------------------- #
import.MigData <- function(pathfile, AgeGroups, dataColsM, dataColsF, dataRows, 
                           increment, md = metadata) {
  County <- metadata$place$subareas
  code <- metadata$place$code[-1]
  County <- paste0(metadata$place$codes[-1], County)
  x <- read.csv(pathfile, header = FALSE, stringsAsFactors = FALSE)
  is.data.frame(x)  # TRUE
  dim(x)  # 1056   95  # 1st column holds row names
  colnames(x) <- c("RowLabel", paste0(County, "M"), paste0(County, "F"))
  x[1, 1] <- "Table 1"
  x[, 1] <- trimws(x[, 1])
  
  MigData <- array(0, dim = c(47, 47, 2, length(AgeGroups)), 
                   dimnames = list(Census = County, OneYearAgo = County, 
                                   Sex = c("Female", "Male"), Age = AgeGroups))
  # Function used in following loop
  reformatMigMatrix <- function(m) {
    m <- data.matrix(sapply(m, as.numeric))
    m[is.na(m)] <- 0
    return(m)
  }
  for (i in 1:dim(MigData)[4]) {
    oldw <- getOption("warn")  # Suppress warnings
    options(warn = -1)
    MigData[, , "Female", i] <- reformatMigMatrix(x[dataRows + (i - 1) * increment, dataColsF])
    MigData[, , "Male",   i] <- reformatMigMatrix(x[dataRows + (i - 1) * increment, dataColsM])
    options(warn = oldw)
  }
  return(MigData)
}
calculate.AgeCohort5 <- function(y, md = metadata) {
  # Arg: Numbers of net migrants at single years of age through 95+ in year
  #      prior to the census (y) VECTOR
  # Val: Numbers of net migrants during 5 years prior to census to 5 year 
  # age cohorts, AgeCohort5 VECTOR
  y <- y[-length(y)]
  y1 <- c(0, 0, 0, 0, y, 0, 0, 0, 0)  # Extend by 5 zeros at each end
  ycum <- rep(0, times = length(y) + 4)
  j <- 0:4
  for (i in 1:100) {
    ycum[i] <- sum(y1[i + j])
  }
  m20x5 <- matrix(ycum, nrow = 20, ncol = 5, byrow = TRUE)
  AgeCohort5 <- apply(m20x5, 1, sum)
  AgeCohort5[length(AgeCohort5)] <- 0
  names(AgeCohort5) <- metadata$age$ADrows
  # # For checking only
  # sytable <- matrix(0, nrow = 100, ncol = 3)
  # rownames(sytable) <- 0:99
  # colnames(sytable) <- c("y.in", "y.out", "AgeCohort5")
  # sytable[1:length(y.in), "y.in"] <- y.in
  # sytable[, "y.out"] <- y.out
  # sytable[1:20, "AgeCohort5"] <- AgeCohort5
  # # return(sytable)
  # 
  # if (abs(sum(AgeCohort5) - 5 *sum(y.in)) > 0.001) {
  #   stop(abs(sum(AgeCohort5) - 5 *sum(y.in)) > 0.001)
  # }
  return(AgeCohort5)
}
calculate.NIM5x5 <- function(NIM1x1, md = metadata) {
  # Applies calculate.AgeCohort5 to columns of NIMR1x1. Note that the input is
  # ages 0-94; 95+ in NIM5x5 is set to 0
  # NIM1x1: Interpolated numbers of migrants by single years of age for
  #         year prior to census (sex-age1  x subareas matrix)
  # NIM5x5: Number of migrants to 5 year age cohorts during the 5 years
  #         prior to the census (sex-age5 x subareas matrix)
  ASDrows <- md$age$ASDrows
  subareas <- md$place$subareas
  NIM5x5 <- matrix(0, nrow = length(ASDrows), ncol = length(subareas))
  rownames(NIM5x5) <- ASDrows
  colnames(NIM5x5) <- subareas
  n1 <- dim(NIM1x1)[1] / 2
  n2 <- length(ASDrows) / 2
  for (j in 1:length(subareas)) {
    NIM5x5[1:n2, j] <- calculate.AgeCohort5(NIM1x1[1:n1, j])
    NIM5x5[n2 + 1:n2, j] <- calculate.AgeCohort5(NIM1x1[n1 + 1:n1, j])
  }
  return(NIM5x5)
}
calculate.NIMR <- function(NIMR5x5, md = metadata) {
  # NIMR5x5: See calculate.NIMR5x5
  # NIMR: list of sex-age by pcycle matrices for all places (area and subarea),
  #       internal migration input required by projection programs
  # Initialized NIMR: List of sex-age x pcycle matrices for each place, area
  #           
  places <- md$place$places
  NIMR <- vector(mode = "list", length = length(places))
  ASDrows <- md$age$ASDrows
  pcycles <- md$time$pcycles
  M <- matrix(0, nrow = length(ASDrows), ncol = length(pcycles))
  rownames(M) <- ASDrows
  colnames(M) <- pcycles
  NIMR <- lapply(places, function(x){M})
  names(NIMR) <- places
  # Populate NIMR
  n <- length(ASDrows) / 2
  for (j in 2:48) {
    NIMR[[j]][1:n, ]     <- NIMR5x5[1:n, j - 1]
    NIMR[[j]][n + 1:n, ] <- NIMR5x5[n + 1:n, j - 1]
  }
  NIMR <- lapply(NIMR, function(x){round(x, 4)})
  return(NIMR) 
}

calculate.AgeCohort5OLD <- function(y.in, md = metadata) {
  # y gives numbers of net migrants at single years of age through 95+
  yx <- c(0, 0, 0, 0, y.in, 0, 0, 0, 0)  # y extended by zeros at each end
  y.out <- rep(0, times = length(y.in) + 4)
  j <- 0:4
  for (i in 1:100) {
    y.out[i] <- sum(yx[i + j])
  }
  m20x5 <- matrix(y.out, nrow = 20, ncol = 5, byrow = TRUE)
  AgeCohort5 <- apply(m20x5, 1, sum)
  names(AgeCohort5) <- metadata$age$ADrows
  
  # For checking only
  sytable <- matrix(0, nrow = 100, ncol = 3)
  rownames(sytable) <- 0:99
  colnames(sytable) <- c("y.in", "y.out", "AgeCohort5")
  sytable[1:length(y.in), "y.in"] <- y.in
  sytable[, "y.out"] <- y.out
  sytable[1:20, "AgeCohort5"] <- AgeCohort5
  # return(sytable)
  
  if (abs(sum(AgeCohort5) - 5 *sum(y.in)) > 0.001) {
    stop(abs(sum(AgeCohort5) - 5 *sum(y.in)) > 0.001)
  }
  
  return(AgeCohort5)
}
impute.NIMR5x5.65plus <- function(NIMR5x5, Fac) {
  # Arg: NIMR table for county  # Fac <- 0.4
  # Val: Same with declining rates imputed after age 65
  NIMR5x5.out <- NIMR5x5
  for (i in 15:20) {
    NIMR5x5.out[i, ] <- NIMR5x5.out[i - 1, ] * Fac
  }
  for (i in 20 + 15:20) {
    NIMR5x5.out[i, ] <- NIMR5x5.out[i - 1, ] * Fac
  }
  return(NIMR5x5.out)
}
get.streams <- function(ToFrom) {
  streams <- data.frame(To = character(0), From = character(0), Number = numeric(0),
                        stringsAsFactors = FALSE)
  for (i in 1:dim(ToFrom)[1]) {
    for (j in 1:dim(ToFrom)[2]) {
      if (i == j) {
        next
      }
      streams <- rbind(streams, data.frame(To = rownames(ToFrom)[i], 
                                           From = colnames(ToFrom)[j], 
                                           Number = ToFrom[i, j]))
    }
  }
  streams <- streams[rev(order(streams[, 3])), ]
  rownames(streams) <- 1:dim(streams)[1]
  Percent <- round(100 * streams[, 3] / sum(streams[, 3]), 2)
  CumPcnt <- round(cumsum(Percent), 1)
  streams <- cbind(streams, Percent, CumPcnt)
  return(streams)
}
# -------------------------- END Migration functions ------------------------- #