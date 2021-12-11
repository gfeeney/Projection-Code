BaseASD <- readRDS("../projection-inputs/BaseASD/BaseASD.rds")


np <- readRDS("../projections/kenya0/np.rds")       # Projection data
setup <- readRDS("../projection-inputs/setup.rds")  # Setup parameters
source("../projections/R/projection-utilities.R")
asdList <- asd.from.pframelist(np, setup)
asdList <- lapply(asdList, function(x) {round(x, 0)})

# Plot female age distributions
y <- asdList$Female/1000
source("../projections/R/plotting.R")
construct.plot.frame(xlabel = "Age", xlim = c(0, 100),
                     ylabel = "Number (000)", ylim = c(0, 4280))
x <- 5 * 1:20 - 2.5
lines(x, y[, 1], lwd = 2, col = gray(0.8))
lines(x, y[, 2], lwd = 2, col = gray(0.6))
lines(x, y[, 3], lwd = 2, col = gray(0.4))
lines(x, y[, 4], lwd = 2, col = gray(0.2))
lines(x, y[, 5], lwd = 2, col = gray(0.0))
lines(x, y[, 6], lwd = 2, col = gray(0.0))

