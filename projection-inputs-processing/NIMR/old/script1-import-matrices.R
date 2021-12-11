metadata <- readRDS("metadata.rds")
path2inputs <- metadata$paths$path2inputs
path2R <- metadata$paths$path2R
source(paste0(path2R, "projection.R"))
AgeGroups <- getAG(5, 65)
dataColsM <- 1:47
dataColsF <- 48:94
dataRows  <- 2:48
increment <- 54

import.matrices <- function(pathfile, AgeGroups, dataColsM, dataColsF, dataRows, increment) {
  x <- read.csv(pathfile, header = FALSE, stringsAsFactors = FALSE)
  dim(x)  # 755  95
  rnames <- trimws(x[-1, 1], which = c("both", "left", "right"), whitespace = "[ \t\r\n]")
  cnames <- trimws(x[1, -1], which = c("both", "left", "right"), whitespace = "[ \t\r\n]")
  y <- data.matrix(x[-1, -1])
  is.matrix(y)  # TRUE
  dim(y)  # 754  94
  rownames(y) <- rnames
  colnames(y) <- cnames  # View(y)
  
  County <- colnames(y)[1:(length(colnames(y)) / 2)]
  ToFromMatrices <- array(0, dim = c(47, 47, 2, 14), dimnames = list(To = County, 
                                                               From = County, 
                                                               Sex = c("Female", "Male"), 
                                                               Age = AgeGroups))
  for (i in 1:dim(ToFromMatrices)[4]) {
    ToFromMatrices[, , "Female", i] <- y[dataRows + (i - 1) * increment, dataColsF]
    ToFromMatrices[, , "Male",   i] <- y[dataRows + (i - 1) * increment, dataColsM]
  }
  return(ToFromMatrices)
}

# 2019 census
pathfile  <- paste0(path2inputs, "NIMR/NIMRsource2019.csv")
ToFromMatrices2019 <- import.matrices(pathfile, AgeGroups, dataColsM, dataColsF, dataRows, increment)
pathfile <- paste0(path2inputs, "NIMR/ToFromMatrices2019.rds")
saveRDS(ToFromMatrices2019, pathfile)  # Save to .rds
readRDS(pathfile)

# Check 0-4 age group
ToFromMatrices2019[c(1, 47), c(1, 47) , "Male", "0-4"]    # 53313   612  184633  765  CHECK!
ToFromMatrices2019[c(1, 47), c(1, 47) , "Female", "0-4"]  # 51687   638  182273  675  CHECK!
# Check 65+ age group
ToFromMatrices2019[c(1, 47), c(1, 47) , "Male", "65+"]    #  9463  45  23919  154  CHECK!
ToFromMatrices2019[c(1, 47), c(1, 47) , "Female", "65+"]  #  9657  92  21506  208  CHECK!

# 2009 census
increment <- 50  # TNIMRsource2009 file came from Paul 20210902
pathfile <- paste0(path2inputs, "NIMR/NIMRsource2009.csv")
ToFromMatrices2009 <- import.matrices(pathfile, AgeGroups, dataColsM, dataColsF, dataRows, increment)
pathfile <- paste0(path2inputs, "NIMR/ToFromMatrices2009.rds")
saveRDS(ToFromMatrices2009, pathfile)
readRDS(pathfile)

# Check 0-4 age group
ToFromMatrices2009[c(1, 47), c(1, 47) , "Male", "0-4"]    #  45083  345  139708  324 CHECK!
ToFromMatrices2009[c(1, 47), c(1, 47) , "Female", "0-4"]  #  43941  372  138376  281 CHECK!
# Check 65+ age group
ToFromMatrices2009[c(1, 47), c(1, 47) , "Male", "65+"]    #  7036  25  16011  29  CHECK!
ToFromMatrices2009[c(1, 47), c(1, 47) , "Female", "65+"]  #  7118  30  15024  38  CHECK!
