metadata <- readRDS("metadata.rds")
path2inputs <- metadata$paths$path2inputs

census2019asd5 <- readRDS(paste0(path2inputs, "BaseASD/census2019asd5.rds"))
total2019 <- sum(census2019asd5[, "Kenya"])
total2019

census2009asd5 <- readRDS(paste0(path2inputs, "BaseASD/census2009asd5.rds"))
total2009 <- sum(census2009asd5[, "Kenya"])
total2009

r <- log(total2019/total2009)/10
r

BaseASD1 <- census2019asd5 * exp(r * (2020.5 - 2019.644))
BaseASD1[, "Kenya"] - apply(BaseASD1[, 2:48], 1, sum)
# Need to decide whether these 1 digit differences are worth doing something about
# Perfectly acceptable for report to include a statement that 'Sum of distributions
# may differ slightly from totals due to rounding error
pathfile <- paste0(path2inputs, "BaseASD/BaseASD1.rds")
saveRDS(BaseASD1, pathfile)
# View(readRDS(pathfile))
total <- sum(BaseASD1[, "Kenya"])
total
total/total2019
