# CLSI-Statistic

本仓库记录CLSI中一些统计学方法的notes和code（以R为主），以便不时之用，若有理解偏差或者版本迭代导致的错误，请告知下

#### EP09-A3

##### EDS, Detecting Aberrant Results(Outliers)

使用EDS来识别单变量离群点（这里的单变量一般是指考核和对比试剂的绝对差值，但是也有用相对差值的），代码：`EDS.R`，笔记：[识别离群点-ESD(Generalized ESD)](https://www.bioinfo-scrounger.com/archives/detection-ESD/)

#### EP09-A2

##### Computeing Predicted Bias and Its Confidence Interval

根据EP09-A2（或者二类诊断试剂临床试验指导原则）中的公式，计算医学决定水平的预期偏倚的估计值和置信区间

* 假如Bc（计算所得偏倚）的可信区间包含了可接受偏倚/范围（临床预设的允许偏差），则说明考核方法与比对方法有可比性
* 假如Bc的可信区间不包含可接受偏倚时，并且可接受偏倚大于Bc可信区间的上限，则说明考核方法与比对方法有可比性
* 假如Bc的可信区间不包含可接受偏倚时，并且可接受偏倚小于Bc可信区间的下限，则说明考核方法与比对方法不具有可比性

代码：`Predicted_bias_CI.R`


