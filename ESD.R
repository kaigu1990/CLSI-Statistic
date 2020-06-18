# Compute the critical value for ESD Test
esd.critical <- function(alpha, N, i) {
  v <- N - i - 1
  p <- 1 - alpha / (2 * (N - i + 1))
  t <- qt(p, v)
  lambda <- t * (N - i) / sqrt((N - i + 1) * (v + t^2))
  return(lambda)
}

#' Calculate ESD(generalized ESD)
#' @description 
#' To detect outliers from whthin a single distribution when the number of outliers is unknown
#' 
#' @details
#' The algorithm is from CLSI EP09-A3
#' @param x 
#' @param level assume what percentage of outliers in the dataset
#' @return results table and outliers list
#' @export
#' @examples
#' y = c(-0.25, 0.68, 0.94, 1.15, 1.20, 1.26, 1.26,
#'  1.34, 1.38, 1.43, 1.49, 1.49, 1.55, 1.56,
#'  1.58, 1.65, 1.69, 1.70, 1.76, 1.77, 1.81,
#'  1.91, 1.94, 1.96, 1.99, 2.06, 2.09, 2.10,
#'  2.14, 2.15, 2.23, 2.24, 2.26, 2.35, 2.37,
#'  2.40, 2.47, 2.54, 2.62, 2.64, 2.90, 2.92,
#'  2.92, 2.93, 3.21, 3.26, 3.30, 3.59, 3.68,
#'  4.30, 4.64, 5.34, 5.42, 6.01)
#' ESD_detect(y, level = 0.2)
ESD_detect <- function(x, level = 0.05) {
  # Define values and vectors.
  x2 <- x
  n <- length(x)
  alpha <- 0.05
  h <- round(level * n) + 1
  
  # Removed from the sample base on ESDi
  res <- data.frame(stringsAsFactors = F)
  for (i in 1:h) {
    if (sd(x2) == 0){
      break
    } 
    temp <- abs(x2 - mean(x2)) / sd(x2)
    esdi <- max(temp)
    index <- which(temp == esdi)[1]
    
    lambda <- esd.critical(alpha, n, i)
    res <- rbind(res, data.frame(ID = x2[index], i = i, ESDi = esdi, lambda = lambda))
    
    ## iterated remove esdi
    x2 <- x2[temp != esdi]
  }
  
  # Keep the non-outliers
  # If ESDh and ESDh+1 are equal(a tie) then neither one should be seen as an outlier
  # The number of outliers is determined by finding the largest i such that ESDi > λi
  if (res$ESDi[h] == res$ESDi[h-1]) {
    res <- res[-((h-1):h),]
    if (length(which(res$ESDi > res$lambda)) > 0) {
      outlier <- res[1:which(res$ESDi > res$lambda),]
    }else{
      outlier <- NULL
    }
    
  }else{
    if (length(which(res$ESDi > res$lambda)) > 0) {
      outlier <- res[1:which(res$ESDi > res$lambda),]
    }else{
      outlier <- NULL
    }
  }
  
  out <- list(results = res, outliers = outlier)
  
  return(out)
}

# y = c(-0.25, 0.68, 0.94, 1.15, 1.20, 1.26, 1.26,
#       1.34, 1.38, 1.43, 1.49, 1.49, 1.55, 1.56,
#       1.58, 1.65, 1.69, 1.70, 1.76, 1.77, 1.81,
#       1.91, 1.94, 1.96, 1.99, 2.06, 2.09, 2.10,
#       2.14, 2.15, 2.23, 2.24, 2.26, 2.35, 2.37,
#       2.40, 2.47, 2.54, 2.62, 2.64, 2.90, 2.92,
#       2.92, 2.93, 3.21, 3.26, 3.30, 3.59, 3.68,
#       4.30, 4.64, 5.34, 5.42, 6.01)
# ESD_detect(y, level = 0.2)