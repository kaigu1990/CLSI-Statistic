#---------------------------------------------------------------------
# Functions
#' Calculate necessary statistics for sensitivity, specificity, Kappa, PLR, NLR, PPV, NPV, PR, RR
#' in Qualitative performance test.
#' 
#' @param tp true positive
#' @param fp false positive
#' @param fn false negative
#' @param tn true negative
#'
#' @return All results in data.frame
#' @export
#'
#' @examples
diagAcc <- function(tp, fp, fn, tn) {
  # a=tp, b=fp, c=fn, d=tn
  dat <- matrix(c(tp, fp, fn, tn), byrow = T, nrow = 2)
  
  #-------------------------------------------------------------------
  # Sensitivity
  sens <- dat[1,1] / (dat[1,1] + dat[2,1])
  sens_CI <- DescTools::BinomCI(x = dat[1,1], n = dat[1,1] + dat[2,1], conf.level = 0.95, method = "wilson")
  
  # Specificity
  spec <- dat[2,2] / (dat[2,2] + dat[1,2])
  spec_CI <- DescTools::BinomCI(x = dat[2,2], n = dat[1,2] + dat[2,2], conf.level = 0.95, method = "wilson")
  
  # Prevalence
  prev <- (dat[1,1] + dat[2,1]) / sum(dat)
  prev_CI <- DescTools::BinomCI(x = dat[1,1] + dat[2,1], n = sum(dat), conf.level = 0.95, method = "wilson")
  
  #-------------------------------------------------------------------
  # PLR(positive likelihood ratio)
  plr <- sens / (1 - spec)                  # (a/(a+c))/(b/(b+d))
  # For plr CI
  p1 <- sens                                # dat[1,1] / (dat[1,1] + dat[2,1])
  p2 <- 1 - spec                            # 1 - dat[2,2] / (dat[2,2] + dat[1,2])
  x1 <- dat[1,1]                            # =>a
  x2 <- dat[1,2]                            # =>b
  plr_ll <- plr * exp(-1.96 * sqrt((1 - p1) / x1 + (1 - p2) / x2))
  plr_ul <- plr * exp(+1.96 * sqrt((1 - p1) / x1 + (1 - p2) / x2))
  plr_CI <- c(est = plr, lwr.ci = plr_ll, upr.ci = plr_ul)
  
  # NLR(negative likelihood ratio)
  nlr <- (1 - sens) / spec                  # (c/(a+c))/(d/(b+d))
  # For nlr CI
  p1 <- (1 - sens)                          # 1 - dat[1,1] / (dat[1,1] + dat[2,1]) => c/(a+c)
  p2 <- spec                                # dat[2,2] / (dat[2,2] + dat[1,2]) => d/(b+d)
  x1 <- dat[2,1]                            # =>c
  x2 <- dat[2,2]                            # =>d
  nlr_ll <- nlr * exp(-1.96 * sqrt((1 - p1) / x1 + (1 - p2) / x2))
  nlr_ul <- nlr * exp(+1.96 * sqrt((1 - p1) / x1 + (1 - p2) / x2))
  nlr_CI <- c(est = nlr, lwr.ci = nlr_ll, upr.ci = nlr_ul)
  
  #-------------------------------------------------------------------
  # PPV(positive predictive value)
  # ppv <- dat[1,1] / (dat[1,1] + dat[1,2])         # a/(a+b)
  ppv <- 1 / (1 + (1 - prev) / (plr * prev))
  # For ppv CI
  ppv_ll <- 1 / (1 + (1 - prev) / (plr_ll * prev))
  ppv_ul <- 1 / (1 + (1 - prev) / (plr_ul * prev))
  ppv_CI <- c(est = ppv, lwr.ci = ppv_ll, upr.ci = ppv_ul)
  
  # NPV(negative predictive value)
  # npv <- dat[2,2] / (dat[2,2] + dat[2,1])         # c/(c+d)
  npv <- 1 / (1 + nlr * prev / (1 - prev))
  # For ppv CI
  npv_ll <- 1 / (1 + nlr_ul * prev / (1 - prev))
  npv_ul <- 1 / (1 + nlr_ll * prev / (1 - prev))
  npv_CI <- c(est = npv, lwr.ci = npv_ll, upr.ci = npv_ul)
  
  
  #-------------------------------------------------------------------
  # point risk
  ar <- dat[1,1] / (dat[1,2] + dat[1,1])
  # pr <- dat[1,1] / (dat[1,2] + dat[1,1])          # a/(a+b)
  # pr <- 1 / (1 + (1 - prev) / (plr * prev))
  # # For pr CI
  ar_ll <- DescTools::BinomCI(x = dat[1,1], n = dat[1,2] + dat[1,1], conf.level = 0.95, method = "wilson")[1,2]
  ar_ul <- DescTools::BinomCI(x = dat[1,1], n = dat[1,2] + dat[1,1], conf.level = 0.95, method = "wilson")[1,3]
  # pr_ll <- 1 / (1 + (1 - prev) / (plr_ll * prev))
  # pr_ul <- 1 / (1 + (1 - prev) / (plr_ul * prev))
  # pr_CI <- c(est = pr, lwr.ci = pr_ll, upr.ci = pr_ul)
  ar_CI <- c(est = ar, lwr.ci = ar_ll, upr.ci = ar_ul)
  
  
  #-------------------------------------------------------------------
  # Relative risk
  rr <- (dat[1,1] / (dat[1,1] + dat[1,2])) / (dat[2,1] / (dat[2,1] + dat[2,2]))
  p1 <- dat[1,1] / (dat[1,1] + dat[1,2])               # P(D=1|cobas4800=Positive)
  p2 <- dat[2,1] / (dat[2,1] + dat[2,2])               # P(D=1|cobas4800=Negative)
  x1 <- dat[1,1]                                       # =>a
  x2 <- dat[2,1]                                       # =>c
  rr <- p1 / p2
  rr_ll <- rr * exp(-1.96 * sqrt((1 - p1) / x1 + (1 - p2) / x2))
  rr_ul <- rr * exp(+1.96 * sqrt((1 - p1) / x1 + (1 - p2) / x2))
  rr_CI <- c(est = rr, lwr.ci = rr_ll, upr.ci = rr_ul)
  
  res <- rbind(sens_CI, spec_CI, prev_CI, plr_CI, nlr_CI, ppv_CI, npv_CI, ar_CI, rr_CI)
  row.names(res) <- c("Sensitivity", "Specificity", "Prevalence", "PLR", "NLR", "PPV", "NPV", "AR", "RR")
  
  res[c(1:3,6:8),] <- res[c(1:3,6:8),] * 100
  res[,] <- sprintf("%.2f", res)
  return(res)
}


#---------------------------------------------------------------------
# Functions
#' Calculate necessary statistics for PPA, NPA, OPA
#' in Qualitative performance test.
#' 
#' @param tp true positive
#' @param fp false positive
#' @param fn false negative
#' @param tn true negative
#'
#' @return All results in data.frame
#' @export
#'
#' @examples
without_diagAcc <- function(tp, fp, fn, tn) {
  # a=tp, b=fp, c=fn, d=tn
  dat <- matrix(c(tp, fp, fn, tn), byrow = T, nrow = 2)
  
  #-------------------------------------------------------------------
  # PPA(Positive percent agreement)
  # ppa <- a/(a+c)
  ppa_CI <- DescTools::BinomCI(x = dat[1,1], n = dat[1,1] + dat[2,1], conf.level = 0.95, method = "wilson")
  
  # NPA(Negative percent agreement)
  # npa <- d/(d+b)
  npa_CI <- DescTools::BinomCI(x = dat[2,2], n = dat[2,2] + dat[1,2], conf.level = 0.95, method = "wilson")
  
  # OPA(Overall percent agreement)
  # opa <- (a+d)/n
  opa_CI <- DescTools::BinomCI(x = dat[1,1] + dat[2,2], n = dat[1,1] + dat[1,2] + dat[2,1] + dat[2,2], conf.level = 0.95, method = "wilson")
  
  res <- rbind(ppa_CI, npa_CI, opa_CI) %>%
    as.data.frame(stringsAsFactors = F) %>%
    mutate_if(is.character, as.numeric)
  res[c(1:3),] <- res[c(1:3),] * 100
  res <- mutate_all(res, sprintf, fmt = "%.2f")
  row.names(res) <- c("PPA", "NPA", "OPA")
  
  return(res)
}

