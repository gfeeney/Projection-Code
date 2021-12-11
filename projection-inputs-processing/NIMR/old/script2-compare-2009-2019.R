metadata <- readRDS("metadata.rds")
path2inputs <- metadata$paths$path2inputs
pathfile2019 <- paste0(path2inputs, "NIMR/ToFromMatrices2019.rds")
md2019 <- readRDS(pathfile2019)
pathfile2009 <- paste0(path2inputs, "NIMR/ToFromMatrices2009.rds")
md2009 <- readRDS(pathfile2009)

# 2019 Census
Persons2019 <- apply(md2019, c(1, 2), sum)
sum(Persons2019)  # 45,998,420
Migrants2019 <- Persons2019
diag(Migrants2019) <- 0
Migrants2019[1:4, 1:4]
sum(Migrants2019)  # 3,397,484
sum(Migrants2019) / sum(Persons2019)  # 0.07386088 -> 7.4%

# 2009 Census
Persons2009 <- apply(md2009, c(1, 2), sum)
sum(Persons2009)  # 37,258,613
Migrants2009 <- Persons2009
diag(Migrants2009) <- 0
Migrants2009[1:4, 1:4]
sum(Migrants2009)        # 1,317,021
sum(Migrants2009) / sum(Persons2009)  # 0.0353481 or 3.5%


