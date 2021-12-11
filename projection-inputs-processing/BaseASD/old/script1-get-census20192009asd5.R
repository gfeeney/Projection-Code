metadata <- readRDS("metadata.rds")
library(xlsx)
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
path2inputs <- metadata$paths$path2inputs

census2019asd5 <- get.censusasd5(paste0(path2inputs, "BaseASD/census2019asd5.xlsx"))
pathfile2019 <- paste0(path2inputs, "BaseASD/census2019asd5.rds") 
saveRDS(census2019asd5, pathfile2019)
# View(readRDS(pathfile2019))

census2009asd5 <- get.censusasd5(paste0(path2inputs, "BaseASD/census2009asd5.xlsx"))
pathfile2009 <- paste0(path2inputs, "BaseASD/census2009asd5.rds") 
saveRDS(census2009asd5, pathfile2009)
# View(readRDS(pathfile2009))


