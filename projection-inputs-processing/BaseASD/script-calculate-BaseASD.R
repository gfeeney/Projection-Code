metadata <- readRDS("metadata.rds")
source(paste0(metadata$paths$path2R, "projection.R"))
path2inputs <- metadata$paths$path2inputs
library(xlsx)

# STEP 1 Import 2019 and 2009 census age-sex distributions for Kenya + counties 
# as matrices and save to .rds
get.censusasd5 <- function(pathfile) {
  x <- read.xlsx(file = pathfile, sheetName = 1, 
                 header = FALSE, colIndex = 1:49,
                 stringsAsFactors = FALSE)
  rnames <- as.character(x[-1, 1])
  cnames <- trimws(as.character(x[1, -1]))
  y <- x[-1, -1]
  z <- do.call(cbind, lapply(y, function(x){as.numeric(x)}))
  rownames(z) <- rnames
  colnames(z) <- cnames
  return(z)
}

census2019asd5 <- get.censusasd5(paste0(path2inputs, 
                                        "BaseASD/source/census2019asd5.xlsx"))
colnames(census2019asd5) <- metadata$place$places
census2019asd5[, 1] - apply(census2019asd5[, 2:48], 1, sum)  # Input data check
View(census2019asd5)
saveRDS(census2019asd5, paste0(path2inputs, "/BaseASD/census2019asd5.rds"))

census2009asd5 <- get.censusasd5(paste0(path2inputs, 
                                        "BaseASD/source/census2009asd5.xlsx"))
colnames(census2009asd5) <- metadata$place$places
census2009asd5[, 1] - apply(census2009asd5[, 2:48], 1, sum)  # Input data check
View(census2009asd5)
saveRDS(census2009asd5, paste0(path2inputs, "/BaseASD/census2009asd5.rds"))

# STEP 2 Smooth older age group numbers by unZigZag method
asd5adj <- census2019asd5
asd5adj[ , ] <- 0
pathfile <- paste0(path2inputs, "BaseASD/plots/census2019unZigZagPlots.pdf")
pdf(file = pathfile, onefile = TRUE)
i <- 7 + 2 * 0:5  # 30-34, 40-44, . . . 80-84
for (j in 1:dim(asd5adj)[2]) {
  place <- colnames(asd5adj)[j]
  sex <- "Female"
  ad5f <- census2019asd5[1:20, j]
  out <- unZigZag(ad5f, i, plotout = TRUE)
  text(0, paste(place, sex), adj = 0)
  asd5adj[1:20, j] <- out[, "y"]
  sex <- "Male"
  out <- unZigZag(census2019asd5[21:40, j], i, plotout = TRUE)
  text(0, paste(place, sex), adj = 0)
  asd5adj[21:40, j] <- out[, "y"]
}
dev.off()
asd5adj[, 1] - apply(asd5adj[, 2:48], 1, sum)
CFA <- asd5adj[, 1] / apply(asd5adj[, 2:48], 1, sum)
CFA
asd5adj[, 2:48] <- asd5adj[, 2:48] * matrix(CFA, nrow = length(CFA), ncol = 47)
asd5adj[, 1] - apply(asd5adj[, 2:48], 1, sum)
CFA <- asd5adj[, 1] / apply(asd5adj[, 2:48], 1, sum)
CFA
# Discrepanies due to rounding errors
round(asd5adj[, 1], 0) - apply(round(asd5adj[, 2:48], 0), 1, sum)

census2019asd5adj <- asd5adj
pathfile <- paste0(path2inputs, "BaseASD/census2019asd5adj.rds")
saveRDS(census2019asd5adj, pathfile)
# DO NOT ROUND HERE!

# STEP 3 Move adjusted 2019 Census age-sex distribution forward to mid-2020 
# using 2009-2019 growth rate
total2019 <- sum(census2019asd5[, "Kenya"])
total2009 <- sum(census2009asd5[, "Kenya"])
r <- log(total2019/total2009)/10
CensusTime <- metadata$time$CensusTime
BaseTime <- metadata$time$BaseTime
BaseASD <- census2019asd5adj * exp(r * (BaseTime - CensusTime))
BaseASD[, 1] - apply(BaseASD[, 2:48], 1, sum)
BaseASD[, 1] / apply(BaseASD[, 2:48], 1, sum)
BaseASD <- round(BaseASD, 0)
BaseASD[, 1] - apply(BaseASD[, 2:48], 1, sum)
BaseASD[, 1] / apply(BaseASD[, 2:48], 1, sum)
View(BaseASD)
saveRDS(BaseASD, paste0(path2inputs, "BaseASD/BaseASD.rds"))
write.csv(BaseASD, paste0(path2inputs, "BaseASD/BaseASD.csv"))

# STEP 4 Interpolate unZigZagged 5 year age distributions to single years using
# modified midpoint interpolation
# Initialize output matrix
BaseASDsy <- matrix(0, nrow = 2 * 96, ncol = 48)
AG <- getAG(1, 95)
rownames(BaseASDsy) <- c(paste0("f", AG), paste0("m", AG))
colnames(BaseASDsy) <- colnames(BaseASD)
BaseASDsy[c("f95+", "m95+"), ] <- BaseASD[c("f95+", "m95+"), ]
View(BaseASDsy)

# Interpolate to single years
adjfac <- rep(1, times = 19)
for (j in 1:dim(BaseASDsy)[2]) {
  place <- colnames(BaseASDsy)[j]
  sex <- "female"
  out <- modified.midpoint.interpolation(BaseASD[1:20, j], adjfac)
  if (out$ssd > 0.001) {
    stop("modified.midpoint.interpolation: out$ssd > 0.001")
  }
  BaseASDsy[1:96, j] <- out$table2[, "y3+"]
  
  sex <- "male"
  out <- modified.midpoint.interpolation(BaseASD[20 + 1:20, j], adjfac)
  if (out$ssd > 0.001) {
    stop("modified.midpoint.interpolation: out$ssd > 0.001")
  }
  BaseASDsy[96 + 1:96, j] <- out$table2[, "y3+"]
}
BaseASDsy[, 1] - apply(BaseASDsy[, 2:48], 1, sum)
CFA <- BaseASDsy[, 1] / apply(BaseASDsy[, 2:48], 1, sum)
CFA
BaseASDsy[, 2:48] <- BaseASDsy[, 2:48] * matrix(CFA, nrow = length(CFA), ncol = 47)
BaseASDsy[, 1] - apply(BaseASDsy[, 2:48], 1, sum)
CFA <- BaseASDsy[, 1] / apply(BaseASDsy[, 2:48], 1, sum)
CFA

# Plot interpolated distributions
pathfile <- paste0(path2inputs, "BaseASD/plots/BaseASDsyPlots.pdf")
pdf(file = pathfile, onefile = TRUE)
for (j in 1:dim(BaseASDsy)[2]) {
  place <- colnames(BaseASDsy)[j]
  
  sex <- "female"
  y <- BaseASDsy[1:96, j]
  x <- 1:length(y) - 0.5
  plot(x, y)
  lines(x, y)
  text(0, 0, paste(place, sex), adj = 0)
  
  sex <- "male"
  y <- BaseASDsy[96 + 1:96, j]
  plot(x, y)
  lines(x, y)
  text(0, 0, paste(place, sex), adj = 0)
}
dev.off()

