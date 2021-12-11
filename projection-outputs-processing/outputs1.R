# Compare Kenya both sexes age distributions with sum of County both sexes age distributions
adNat
adSubNat <- lapply(SubNat, function(x){get.AgeSexDistribution(x)[, , "both"][, 1:2]})
adSum <- Reduce("+", adSubNat)
round(adNat / asdSum, 3)
Nat <- adNat[, 2]
Sum <- adSum[, 2]
CFA <- Nat / Sum



# Calculate age-specific growth rates
adt <- get.AgeSexDistribution(Nat)[, , "both"]
asgr <- calculate.asgr(adt)
round(asgr, 2)
x <- 5 * 1:dim(asgr)[1] - 2.5
plot(x, asgr[, 5])
lines(x, asgr[, 5])

