# Input data --------------------------------------------------------------

set.seed(20)
x <- runif(100,0,100)
y <- 1.10*x - 0.001*x^2 + rnorm(100,0,1)*(2 + 0.05*x) + 15


# regression --------------------------------------------------------------

library(mcr)

# Deming regression
dem.reg <- mcreg(x,y, method.reg = "Deming", alpha=0.05, method.ci="jackknife")
printSummary(dem.reg)

# WDeming regression
wdem.reg <- mcreg(x,y, method.reg = "WDeming", alpha=0.05, method.ci="jackknife")
printSummary(wdem.reg)

# Passing-Bablok regression
pb.reg <- mcreg(x,y, method.reg = "PaBa", alpha=0.05, method.ci="bootstrap")
printSummary(pb.reg)
