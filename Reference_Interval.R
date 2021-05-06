#' Detect outliers by dixon
#'
#' @param data 
#'
#' @return
#' @export
#'
#' @examples
dixon <- function(data){
  d <- sort(data$test)
  dr1 <- dr2 <- 1
  
  while (dr1 >= 1/3 | dr2 >= 1/3) {
    R <- max(d) - min(d)
    D1 <- abs(max(d) - d[length(d)-1])
    D2 <- abs(min(d) - d[2])
    dr1 <- D1 / R
    dr2 <- D2 / R
    if (dr1 >= 1/3) {
      d <- d[d != max(d)]
    }
    if (dr2 >= 1/3) {
      d <- d[d != min(d)]
    }
  }
  return(d)
}


# Input data --------------------------------------------------------------
library(tidyverse)

data <- data.table::fread("./inputData.txt")

# Detect outliers ---------------------------------------------------------

d <- dixon(data$test)

# Evaluate Reference Interval ---------------------------------------------
# Parametric
referenceIntervals::refLimit(d, out.method = "horn", out.rm = FALSE, 
                             RI = "p", CI = "p", refConf = 0.90, 
                             limitConf = 0.95, bootStat = "basic")

# Non-parametric
referenceIntervals::refLimit(d, out.method = "horn", out.rm = FALSE, 
                             RI = "n", CI = "n", refConf = 0.90, 
                             limitConf = 0.95, bootStat = "basic")
# Robust
referenceIntervals::refLimit(d, out.method = "horn", out.rm = FALSE, 
                             RI = "r", CI = "boot", refConf = 0.90, 
                             limitConf = 0.95, bootStat = "basic")

