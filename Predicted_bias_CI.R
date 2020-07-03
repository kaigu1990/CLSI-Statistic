#' Calculate medical decision point and its confidence interval
#' @description
#' Calculate medical decision point and its CI by functions from EP09-A2.
#' If the CI for predicted bias includes the defined acceptable bias, then the data do not 
#' show that bias of the candidate method is different from the acceptable bias.
#' What's more, if the acceptable bias is less than the lower limit of CI of the predicted bias,
#' the candidate method is not equivalent to current method. 
#' If acceptable bias is greater than the higher limit of CI of the predicted bias, the candidate 
#' method is equivalent to current method.
#'
#' @param x reference sample (考核试剂)
#' @param y test sample (比对试剂)
#' @param Xc Medical Decision Level Xc 
#'
#' @return results table including Xc, Bc(predicted bias), lower, upper. 
#' @export
#'
#' @examples
#' x <- c(
#'  58.50, 57.55, 64.95, 93.50, 91.65, 49.35, 49.05,
#'  67.40, 85.05, 56.00, 74.60, 53.50, 55.80, 62.00,
#'  94.30, 68.25, 88.45, 80.85, 38.05, 69.10
#' )
#' y <- c(
#'  59.20, 57.70, 65.85, 93.85, 92.49, 50.30, 49.30,
#'  68.65, 85.30, 56.20, 75.10, 53.95, 56.80, 63.25,
#'  95.30, 68.25, 89.30, 82.10, 38.35, 70.55
#' )
#' predicted_bias(x = x, y = y, Xc = 60)
predicted_bias <- function(x, y, Xc){
  # assume Passing-Bablok regression
  PB_fit <- mcr::mcreg(x, y, method.reg = "PaBa", method.ci = "bootstrap", rng.seed = 12345)
  
  # predict y from PB regression
  predict_y <- mcr::calcResponse(PB_fit, x)[, "Y"]
  
  # calculate standard error of estimate S(y,x)
  Syx <- sqrt(sum((y - predict_y)^2) / (length(x) - 2))
  
  # caculate Bc(predicted bias) 
  Bc <- mcr::calcBias(PB_fit, Xc)[1, "Bias"]
  
  # calculate 95% confidence interval for Bc
  se <- 2 * Syx * sqrt(1 / length(x) + (Xc - mean(x))^2 / sum((x - mean(x))^2))
  bias_upper <- Bc + se
  bias_lower <- Bc - se
  
  # result
  res <- c(Xc = Xc, estimate_Bc = Bc, bias_lower = bias_lower, bias_upper = bias_upper)
  return(res)
}

