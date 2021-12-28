metadata <- readRDS("metadata.rds")
path2inputs <- metadata$paths$path2inputs
path2outputs <- metadata$paths$path2outputs
path2R <- metadata$paths$path2R
source(paste0(path2R, "projection.R"))
require("openxlsx")

# Gotcha: If target .xlsx file is open, code will silently fail

# Initialize for 5 year projection results
projLM <- readRDS(paste0(path2outputs, "projections/proj5LM.rds"))
i <- 1
proj <- projLM[[i]]        # input projection
place <- names(projLM)[i]
filename <- paste0(metadata$place$codes[i], place, ".xlsx")
pathfile <- paste0(path2outputs, "annex-tables/", filename)  # output file
# Additional initialization for annual projections 2020-2035
proj13 <- proj[1:3]
asd5x5 <- get.projectedASD(proj)
asd5x1 <- asd5x1.from.asd5x5(asd5x5)
colnames(asd5x1) <- 2019 + 1:dim(asd5x1)[2]
female <- asd5x1[1:20, ]
female <- rbind(female, apply(female, 2, sum))
rownames(female)[21] <- "All Ages"
male <- asd5x1[20 + 1:20, ]
male <- rbind(male, apply(male, 2, sum))
rownames(male)[21] <- "All Ages"
both <- female + male
rownames(both)[21] <- "All Ages"

# Write output file
wb <- loadWorkbook(paste0(path2outputs, "annex-tables/template.xlsx"))
write.page1(wb, proj, pathfile)
write.page2(wb, proj, pathfile)
write.page3(wb, both, male, female)
write.page4(wb, both, male, female)
write.page5(wb, both, male, female)
# write.page6(wb, both, male, female)
# write.page7(wb, both, male, female)
saveWorkbook(wb, pathfile, overwrite = TRUE)

write.page5 <- function(wb, both, male, female) {
  # Do not step through this function!
  male <- male[, 6:11]
  female <- female[, 6:11]
  male <- append.sexratio(male, female)
  for (i in 2:9) {
    addStyle(wb, sheet = "page5", style = createStyle(numFmt = "#,###,###"), rows = 4:24, cols = i)
  }
  writeData(wb, male, sheet = "page5", xy = c(2, 4), array = TRUE, colNames = FALSE)
  for (i in 2:7) {
    addStyle(wb, sheet = "page5", style = createStyle(numFmt = "#,###,###"), rows = 29:49, cols = i)
  }
  writeData(wb, female, sheet = "page5", xy = c(2, 29), array = TRUE, colNames = FALSE)
}

write.page4 <- function(wb, both, male, female) {
  # Do not step through this function!
  female <- female[, 1:6]
  for (i in 2:7) {
    addStyle(wb, sheet = "page4", style = createStyle(numFmt = "#,###,###"), rows = 4:24, cols = i)
  }
  writeData(wb, female, sheet = "page4", xy = c(2, 4), array = TRUE, colNames = FALSE)
  both <- both[, 6:11]
  both <- append.indices(both)
  for (i in 2:10) {
    addStyle(wb, sheet = "page4", style = createStyle(numFmt = "#,###,###"), rows = 29:49, cols = i)
  }
  writeData(wb, both, sheet = "page4", xy = c(2, 29), array = TRUE, colNames = FALSE)
}

write.page3 <- function(wb, both, male, female) {
  # Do not step through this function!
  both <- both[, 1:6]
  male <- male[, 1:6]
  female <- female[, 1:6]
  both <- append.indices(both)
    for (i in 2:10) {
      addStyle(wb, sheet = "page3", style = createStyle(numFmt = "#,###,###"), rows = 4:24, cols = i)
    }
    writeData(wb, both, sheet = "page3", xy = c(2, 4), array = TRUE, colNames = FALSE)
    for (i in 2:9) {
      addStyle(wb, sheet = "page3", style = createStyle(numFmt = "#,###,###"), rows = 29:49, cols = i)
    }
    male <- append.sexratio(male, female)
    writeData(wb, male, sheet = "page3", xy = c(2, 29), array = TRUE, colNames = FALSE)
}

append.indices <- function(x) {
  change <- 100 * (x[, 6] - x[, 1]) / x[, 1]
  index1 <- 100 * x[, 1] / x[1, 1]
  index1[length(index1)] <- NA
  index2 <- 100 * x[, 6] / x[1, 6]
  index2[length(index2)] <- NA
  return(cbind(x, change, index1, index2))  
}

append.sexratio <- function(male, female) {
  # Append sex ratio columns to male table and return
  sr1 <- 100 * male[, 1] / female[, 1]
  sr2 <- 100 * male[, 6] / female[, 6]
  male <- cbind(male, sr1, sr2)
  return(male)
}

write.page2 <- function(wb, proj, pathfile) {
  # Calculate and write page2
  x <- get.AgeSexDistribution(proj)[, , "male"]
  x <- rbind(x, apply(x, 2, sum))
  y <- get.AgeSexDistribution(proj)[, , "female"]
  y <- rbind(y, apply(y, 2, sum))
  rownames(x)[dim(x)[1]] <- "All Ages"
  rownames(y)[dim(y)[1]] <- "All Ages"
  x <- cbind(x, 100 * x[, 1] / y[, 1])
  x <- cbind(x, 100 * x[, 6] / y[, 6])
  colnames(x) <- c(2020 + 5 * 0:5, c(2020, 2045))  
  colnames(y) <- 2020 + 5 * 0:5
  for (i in 2:9) {
    addStyle(wb, sheet = "page2", style = createStyle(numFmt = "#,###,####"), rows = 4:24, cols = i)
  }
  writeData(wb, x, sheet = "page2", xy = c(2, 4), array = TRUE, colNames = FALSE)
  for (i in 2:7) {
    addStyle(wb, sheet = "page2", style = createStyle(numFmt = "#,###,####"), rows = 29:49, cols = i)
  }
  writeData(wb, y, sheet = "page2", xy = c(2, 29), array = TRUE, colNames = FALSE)
}

write.page1 <- function(wb, proj, pathfile) {
  # Select ith projection in projLM, calculate panels for page1 worksheet
  # of output file, write to page1 sheet of wb object
  # Initialization
    
  # Panel for asd, totals, indices
  x <- get.AgeSexDistribution(proj)[, , "both"]
  x <- rbind(x, apply(x, 2, sum))
  x <- cbind(x, 100 * (x[, dim(x)[2]] - x[, 1]) / x[, 1])
  n <- dim(x)[1] - 1
  x <- cbind(x, c(100 * x[1:n, 1] / x[1, 1], NA))
  x <- cbind(x, c(100 * x[1:n, 6] / x[1, 6], NA))
  rownames(x)[dim(x)[1]] <- "All Ages"
  colnames(x) <- c(2020 + 5 * 0:5, "Change", "Ind2020", "Ind2045")
  for (i in 2:10) {
   addStyle(wb, sheet = "page1", style = createStyle(numFmt = "#,###,####"), rows = 4:24, cols = i)
  }
  writeData(wb, sheet = "page1", x, xy = c(2, 4), array = TRUE, colNames = FALSE)

  # Panel for components numbers
  y <- get.ComponentsMatrix(proj)
  y <- y[c("Births", "Deaths", "NatInc", "NetMig"), ] 
  colnames(y) <- c("2020-25", "2025-30", "2030-35", "2035-40", "2040-45")
  for (i in 2:6) {
    addStyle(wb, sheet = "page1", style = createStyle(numFmt = "#,###,####"), rows = 29:32, cols = i)
  }
  writeData(wb, y, sheet = "page1", xy = c(2, 29), array = TRUE, colNames = FALSE)

  # Panel for components rates
  PYL <- (5 / 2) * (x[dim(x)[1], 1:5] + x[dim(x)[1], 2:6])
  z <- y
  z[1, ] <- 1000 * y[1, ] / PYL
  z[2, ] <- 1000 * y[2, ] / PYL
  z[3, ] <- 1000 * y[3, ] / PYL
  z[4, ] <- 1000 * y[4, ] / PYL
  colnames(z) <- colnames(y)
  for (i in 2:6) {
    addStyle(wb, sheet = "page1", style = createStyle(numFmt = "0.0"), rows = 33:36, cols = i)
  }
  writeData(wb, z, sheet = "page1", xy = c(2, 33), array = TRUE, colNames = FALSE)
}




