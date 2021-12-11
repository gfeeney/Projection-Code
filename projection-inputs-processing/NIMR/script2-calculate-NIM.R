metadata <- readRDS("metadata.rds")
path2inputs <- metadata$paths$path2inputs
source(paste0(metadata$paths$path2R, "projection.R"))

# INPUT: Migration matrices from 2019 census (script1)
# OUTPUT: NIM5x1 matrix

# STEP 1: Calculate net internal migration (NIM5x1) numbers for 2019
MigData <- readRDS(paste0(path2inputs, "NIMR/MigData2019.rds"))  # INPUT
dim(MigData)  # [1] 47 47  2 20
dimnames(MigData)
MigData[c(1:2, 46:47), c(1:2, 46:47), , ]  # Note diagonals non-zero
# Initialize NIM matrix
ASDrows <- metadata$age$ASDrows
subareas <- metadata$place$subareas
NIM <- matrix(0, nrow = length(ASDrows), ncol = length(subareas))
rownames(NIM) <- ASDrows
colnames(NIM) <- subareas
calculateNIM <- function(MigMatrix){
  diag(MigMatrix) <- 0
  In <- apply(MigMatrix, 1, sum)
  Out <- apply(MigMatrix, 2, sum)
  NetMigrants <-  In - Out
  return(NetMigrants)
}
# Populate NIM matrix
n <- dim(NIM)[1] / 2
for (i in 1:n) {
  MigMatrix <- MigData[, , "Female", i]
  NIM[i, ] <- calculateNIM(MigMatrix)
}
for (i in 1:n) {
  MigMatrix <- MigData[, , "Male", i]
  NIM[n + i, ] <- calculateNIM(MigMatrix)
}
# View(NIM)  # Some zero cells, but we deal with this later
# Look at rates
POP <- readRDS(paste0(path2inputs, "BaseASD/census2019asd5.rds"))
NIMR <- NIM /POP[, -1]
# View(round(NIMR, 2))
# Problem with rates at older ages, they are too high and they go UP
# For now, a quick fix, . . .
move.toward.zero <- function(x) {
  for (i in 2:length(x)) {
    if (abs(x[i]) > abs(x[i - 1])) {x[i] <- 0.8 * x[i - 1]}
  }
  return(x)
}
for (j in 1:47) {
  county <- colnames(NIMR)[j]
  NIMR[11:20, j] <- move.toward.zero(NIMR[11:20, j])
  NIMR[31:40, j] <- move.toward.zero(NIMR[31:40, j])
}
# View(round(NIMR, 4))
saveRDS(NIMR, paste0(path2inputs, "NIMR/NIMR5x1.rds"))
# Plot to inspect results
pathfile <- paste0(path2inputs, "NIMR/plots/NIMR5x1.pdf")
pdf(pathfile, onefile = TRUE)
for (i in 1:47) {
  county <- colnames(NIMR)[i]
  sex <- "Female"
  y <- NIMR[1:20, i]
  x <- 5 * 1:length(y) - 2.5
  plot(x, y, xlim = c(0, 100), xlab = "Age", ylab = "NIMR")
  lines(x, y)
  lines(c(0, 100), c(0,0))
  text(80, mean(y), paste(j, county, sex))
  sex <- "Male"
  y <- NIMR[20 + 1:20, i]
  x <- 5 * 1:length(y) - 2.5
  plot(x, y, xlim = c(0, 100), xlab = "Age", ylab = "NIMR")
  lines(x, y)
  lines(c(0, 100), c(0,0))
  text(80, mean(y), paste(j, county, sex))
}
dev.off()
# Now study at the plots!

# Now adjust NIM (numbers!) accordingly
NIM5x1 <- round(POP[, -1] * NIMR, 0)
# View(NIM5x1)
saveRDS(NIM5x1, paste0(path2inputs, "NIMR/NIM5x1.rds"))
write.csv(NIM5x1, paste0(path2inputs, "NIMR/NIM5x1.csv"))

# STEP 2: Interpolate NIM (5x1) to single years (1x1)
NIM5x1 <- readRDS(paste0(path2inputs, "NIMR/NIM5x1.rds"))
ADrows <- getAG(1, 95)
length(ADrows)  # 96
ASDrows <- c(paste0("f", ADrows), paste0("m", ADrows))
NIM1x1 <- matrix(0, nrow = length(ASDrows), ncol = dim(NIM5x1)[2])
rownames(NIM1x1) <- ASDrows
colnames(NIM1x1) <- colnames(NIM5x1)
for (j in 1:dim(NIM1x1)[2]) {
  ad5 <- NIM5x1[1:20, j]
  k <- rep(1, times = length(ad5) - 1)
  NIM1x1[1:96, j] <- modified.midpoint.interpolation(ad5, k)$table2[, "y3+"]
  ad5 <- NIM5x1[20 + 1:20, j] 
  k <- rep(1, times = length(ad5) - 1)
  NIM1x1[96 + 1:96, j] <- modified.midpoint.interpolation(ad5, k)$table2[, "y3+"]
  cat(paste0(" . ", j))
}
NIM1x1 <- round(NIM1x1, 0)
# View(NIM1x1)
# Impute zero values 95+ (jump between 94 and 95+ no good)
for (i in 1:length(subareas)) {
  NIM1x1[c(96, 192), i] <- 0
}
# View(round(NIM1x1, 0))
saveRDS(NIM1x1, paste0(path2inputs, "NIMR/NIM1x1.rds"))
write.csv(NIM1x1, paste0(path2inputs, "NIMR/NIM1x1.csv"))

# STEP 3: Plot interpolated single year distributions
pathfile <- paste0(path2inputs, "NIMR/plots/NIM1x1.pdf")
pdf(file = pathfile, onefile = TRUE)
n1 <- (dim(NIM5x1)[1] / 2)
n2 <- (dim(NIM1x1)[1] / 2)
for (j in 1:dim(NIM1x1)[2]) {
  county <- colnames(NIM1x1)[j]
  sex <- "female"
  y <- NIM1x1[1:(n2 - 1), j]  # Do not plot oeg
  x <- 1:length(y) - 0.5
  plot(x, y, xlim = c(0, 100))
  lines(x, y)
  lines(c(0, 100), c(0, 0))
  y <- NIM5x1[1:(n1 - 1), j] / 5
  x <- 5 * 1:length(y) - 2.5
  points(x, y, cex = 2, pch = 16, col = gray.colors(1))
  text(80, mean(y), paste(j, county, sex))
  
  sex <- "male"
  y <- NIM1x1[n2 + 1:(n2 - 1), j]
  x <- 1:length(y) - 0.5
  plot(x, y, xlim = c(0, 100))
  lines(x, y)
  lines(c(0, 100), c(0, 0))
  y <- NIM5x1[n1 + 1:(n1 - 1), j] / 5  # Do not plot oeg
  x <- 5 * 1:length(y) - 2.5
  points(x, y, cex = 2, pch = 16, col = gray.colors(1))
  text(80, mean(y), paste(j, county, sex))
}
dev.off()
# Now study the plots!


