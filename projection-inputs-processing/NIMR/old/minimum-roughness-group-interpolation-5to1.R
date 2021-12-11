x <- c(1229, 1132, 1510, 4857, 8555, 5743, 4211, 2547, 2131, 1229,
       812,  796,  696,  307,  210,  102,   72,   32,   14,   10)
f <- rep(0.2, times = 4 * (length(x) - 1))
calculate.rough <- function(f, x) {
  n <- length(x) - 1
  if (length(f)/4 != n) {
    stop("length(f)/4 != length(x) - 1)")
  }
  f4 <- matrix(f, nrow = n, ncol = 4)
  f5 <- cbind(f4, 1 - apply(f4, 1, sum))
  f5x <- f5 * matrix(x[1:n], nrow = n, ncol = 5)
  abs(apply(f5x, 1, sum) - x[1:n])  # Check
  y <- c(t(f5x))
  i <- 2:(n * 5 - 2)
  sum((y[i] - (y[i - 1] + y[i + 1])/2)^2)
}
roughness(f, x)
out <- optim(f, fn = calculate.rough, gr = NULL, x)

f <- out$par
f4 <- matrix(f, nrow = n, ncol = 4)
f5 <- cbind(f4, 1 - apply(f4, 1, sum))
f5x <- f5 * matrix(x[1:n], nrow = n, ncol = 5)
abs(apply(f5x, 1, sum) - x[1:n])  # Check
y <- c(t(f5x))
plot(1:length(y) - 0.5, y)
roughness(f, x)
