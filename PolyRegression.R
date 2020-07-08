# Input data
data <- data.frame(
  x = c(0.50, 1.00, 1.50, 2.00, 3.00, 4.00, 5.00),
  y = c(6.24, 14.21, 23.31, 32.62, 49.28, 67.65, 85.59)
)

# Fit poly-regression
fit.plm <- lm(y ~ x, data = data)
fit.plm <- lm(y ~ x + I(x^2), data = data)
fit.plm <- lm(y ~ x + I(x^2) + I(x^3), data = data)
# fit.plm <- lm(y ~ poly(x, degree = 2, raw = TRUE), data = data)

# Summary Table
summary(fit.plm)
sjPlot::tab_model(fit.plm,
  show.se = T, show.stat = T, show.df = T,
  show.fstat = T, show.dev = T, file = "./plm.html"
)
