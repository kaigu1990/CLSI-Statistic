# CLSI-Statistic

Keep code and notes about statistical methods from CLSI-EP. If there are any misunderstanding or inconsistent versions , please let me know.

#### EP09-A3

##### EDS, Detecting Aberrant Results(Outliers)

Code：`EDS.R`, Note: [识别离群点-ESD(Generalized ESD)](https://www.bioinfo-scrounger.com/archives/detection-ESD/)

#### EP09-A2

##### Computeing Predicted Bias and Its Confidence Interval

Calculate the medical decision level and its confidence interval in according to EP09-A2 and 2type IVD guidance.

* If the confidence interval of Bc includes acceptance bias, it means the test is comparable to the reference.
* If the confidence interval of Bc excludes acceptance bias, but the bias is higher to the upper CI of Bc，it means the test can be comparable to the reference.
* If the confidence interval of Bc excludes acceptance bias, but the bias is lower to the lower CI of Bc，it means the test can not be comparable to the reference.

Code: `Predicted_bias_CI.R`, Note: [医学决定水平（MDL）的预期偏倚](https://www.bioinfo-scrounger.com/archives/MSL-bias/)

#### EP12-A2

##### Comparator Is Diagnostic Accuracy Criteria

* Sensitivity
* Specificity
* Accuracy
* Kappa
* PPV(positive predictive value)
* NPV(negative predictive value)

##### Comparator Other Than Diagnostic Accuracy Criteria

* OPA(Overall percent agreement)
* PPA(Positive percent agreement)
* NPA(Negative percent agreement) 

##### Others

* PLR(positive likelihood ratio)
* NLR(negative likelihood ratio)
* PR(Point risk)
* RR(Relative risk)

Confidence Interval: Wilson, or clopper-pearson

Code: `DiagAcc.R`, Notes: [诊断试验评价指标](https://www.bioinfo-scrounger.com/archives/Diagnose-evaluation/), [Measures of risk](https://www.bioinfo-scrounger.com/archives/Measures-of-risk/), [Agreement statistics in diagnostic accuracy](https://www.bioinfo-scrounger.com/archives/agreement-statistics-in-diagnostic-accuracy/)

#### EP28-A3

##### Estimating and Verifying Reference Intervals

Detect outliers by dixon method, and request the `referenceIntervals` R package to estimate reference interval

Code: `Reference_Interval`, Note: [Analysis of Reference Value](https://www.bioinfo-scrounger.com/archives/Analysis-of-Reference-Value/)




