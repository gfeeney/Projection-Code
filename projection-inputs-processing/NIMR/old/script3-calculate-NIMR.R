metadata <- readRDS("metadata.rds")
subareas <- metadata$place$subareas
areas <- metadata$place$areas
pcycles <- metadata$time$pcycles
path2inputs <- metadata$paths$path2inputs
NIMRpath <- paste0(path2inputs, "/NIMR/")
ToFrom2019 <- readRDS(paste0(NIMRpath, "ToFromMatrices2019.rds"))
dim(ToFrom2019)  # [1] 47 47  2 14
dimnames(ToFrom2019)
is.array(ToFrom2019)  # TRUE
ToFrom2019[1:4, 1:4, , ]

# Append 5 rows to the bottom of each ToFrom matrix; capture total population
# at census and diagonal entries in two of thse rows and out, in, and net
# migrations in the remaining 3 rows.
# Begin by initializing target array
agegroups <- dimnames(ToFrom2019)[4]$Age
sexes <- dimnames(ToFrom2019)[3]$Sex
placenames <- dimnames(ToFrom2019)[1]$To
placenamesplus <- c(placenames, c("Pop", "Diag", "In", "Out", "Net"))
ToFrom2019Plus <- array(0, dim = c(length(placenamesplus),
                                       length(placenames),
                                       length(sexes),
                                       length(agegroups)),
                            dimnames = list(placenamesplus = placenamesplus,
                                            placenames = placenames,
                                            sex = sexes,
                                            age = agegroups)
                            )
dim(ToFrom2019Plus)
for (age in AgeGroups) {
  for (sex in Sexes) {
    ToFrom <- ToFrom2019[, , sex, age]
    Pop <- apply(ToFrom, 1, sum)
    Diag <- diag(ToFrom)
    Mig <- ToFrom
    diag(Mig) <- 0
    In <- apply(Mig, 1, sum)
    Out <- apply(Mig, 2, sum)
    Net <- In - Out
    ToFromPlus <- rbind(ToFrom, Pop, Diag, In, Out, Net)
    ToFrom2019Plus[, , sex, age] <- ToFromPlus
  }
}
ToFrom2019Plus[, 1:4, 1, 1]

# calculate array of in and out migrants by age and ex for each place
MigRates <- array(0, dim = c(length(AgeGroups), 3, 2, 47),
                  dimnames = list(Age = AgeGroups,
                                  MigRates = c("OutRate", "InRate", "NetRate"),
                                  Sex = Sexes,
                                  County = subareas)
                  )
dim(MigRates)
dimnames(MigRates)
for (age in AgeGroups) {
  for (sex in Sexes) {
    OutRate <- ToFrom2019Plus["Out", , sex, age] / ToFrom2019Plus["Pop", , sex, age]
    InRate  <- ToFrom2019Plus["In", , sex, age]  / ToFrom2019Plus["Pop", , sex, age]
    NetRate <- InRate - OutRate
    MigRates[age, "OutRate", sex, ] <- OutRate
    MigRates[age, "InRate", sex, ] <- InRate
    MigRates[age, "NetRate", sex, ] <- NetRate
  }
}
MigRates2019 <- MigRates
dim(MigRates2019)
MigRates2019[, , 1, 1]
dim(MigRates2019)


# Construct NIMR input list
NIMR <- vector(mode = "list", length(subareas))
names(NIMR) <- subareas
ASDrows <- metadata$age$ASDrows
for (i in 1:length(subareas)) {
  x <- matrix(0, nrow = length(ASDrows), ncol = length(pcycles))
  rownames(x) <- ASDrows
  colnames(x) <- pcycles
  x[ASDrows[1:14], 1:length(pcycles)] <- MigRates2019[, "NetRate", "Female", subareas[i]]
  x[ASDrows[20 + 1:14], 1:length(pcycles)] <- MigRates2019[, "NetRate", "Male", subareas[i]]
  NIMR[[i]] <- round(x, 4)
}
filepath <- paste0(NIMRpath, "NIMR.rds")
saveRDS(NIMR, filepath)
readRDS(filepath)



# Quick Plots of 2019 Rates
x <- 5 * 1:14 - 2.5
pdf(file = "../projection-inputs/NIMR/NetMigRatePlots.pdf", onefile = TRUE)
for (county in Places) {
  for (sex in Sexes) {
    y <- MigRates[, "NetRate", sex, county]
    plot(x, y)
    lines(x, y)
    lines(c(0, 70), c(0, 0))
    text(35, min(y) + 0.05 * (max(y) - min(y)), paste(county, sex))
  }
}
dev.off()





