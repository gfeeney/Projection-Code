# EXPERIMENT Adjust national age-distribution only, use CFA to get
# adjusted subnational age-sex distributions. INPUT Matrix with age-sex
# distributions in columns, initial column for Kenya, following columns
# for 47 counties in standard order. See 'Census2019asd1.xlsx'. Save it
# to .csv for reading into R.

x <- read.csv("../projection-inputs/BaseASD/Census2019asd1NatAdj.csv", 
              header = FALSE, stringsAsFactors = FALSE)
rnames <- x[-1, 1]
cnames <- x[1, -1]
y <- data.matrix(x[-1, -1])
rownames(y) <- rnames
colnames(y) <- cnames
y[c(97, 194), ]
y <- y[-c(97, 194), ]  # NR not worth bothering with
Nat <- y[, 1, drop = FALSE]
SubNat0 <- y[, 2:48]
cfa <- Nat / apply(SubNat0, 1, sum)  # All 1's here because Col 1 not adjusted
SubNat1 <- y[, 2:48] * matrix(cfa, nrow = 192, ncol = 47)
round(Nat - apply(SubNat1, 1, sum), 0)

pdf(file = "../projection-inputs/BaseASD/countyADadjusted.pdf", onefile = TRUE)
for (j in 1:47) {
  x <- 1:96 - 0.5
  y <- SubNat0[1:96, j]
  plot(x, y, main = paste(colnames(SubNat)[j], "Female Unadjusted"))
  lines(x, y)
  y <- SubNat1[1:96, j]
  plot(x, y, main = paste(colnames(SubNat)[j], "Female Adjusted"))
  lines(x, y)
  # y <- SubNat[97:192, j]
  # plot(x, y, main = paste(colnames(SubNat)[j], "Male"))
  # lines(x, y)
}
dev.off()
