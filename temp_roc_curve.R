library(psych)
library(irr)
library(pROC)
library(caret)
library(hmeasure)

# install.packages("ggpubr")

pth="/home/elias/Data/STATISTICS/danah_50/data/"

file="raters_data_long.csv"

# Reading csv file
data_1stday   = read.csv( paste0(pth, file), header = TRUE, sep = ",", quote = "" )
str(data_1stday)

file="raters_data_long_2ndday.csv"
data_2ndday   = read.csv( paste0(pth, file), header = TRUE, sep = ",", quote = "" )
str(data_2ndday)

file="data_tasks.csv"
data_tasks   = read.csv( paste0(pth, file), header = TRUE, sep = ",", quote = "" )
str(data_tasks)


#Total Score
AUT<-rowSums(data_tasks[-c(1,2)])
out = HMeasure(true.class = data_tasks$TaskD<5, scores=AUT  ); out$metrics
# ROC is a graphical plot which illustrates the performance of a binary classifier system as its discrimination threshold is varied (from wikipedia), 
# AUC - the Area Under ROC Curve - The AUC normally takes values between 0.5 and 1,
# true positive (TP) - true negative rate(TN). False positive FP False negative FN
# true positive rate (TPR) and false positive rate (FPR). ==== TPR = TP/(TP+FN), FPR = FP/(TN+FP)
# Sens Spec - sensitivity   same as TPR ----  specificity  = 1−FPR
# Gini coefficient of zero expresses perfect equality - all the residents has the same income. of 1 (100%) express maximall inequality among values.
# ER is the Error Rate
# ER = ER(t˜), which corresponds to the value of t that achieves the minimum ER(t) over the test dataset.
# Error rate considers FPs as important as FNs.

# precision --> TP/(TP+FP)  ----- Recall = same as Sensitivity, or TPR
#  F measure and the Youden index are scalar measures - a more balanced view of the two different objectives than ER does
#   The former (F) is given by the harmonic mean of Precision and Recall, and the latter (Youden) by Sensitivity + Specificity − 1.
# H: the H-measure
# AUCH: the Area Under the convex Hull of the ROC Curve
# KS: the Kolmogorov-Smirnoff statistic
# MER: the Minimum Error Rate
# MWL: the Minimum cost-Weighted Error Rate
# Spec.Sens95: Specificity when Sensitivity is held fixed at 95%
# Sens.Spec95: Sensitivity when Specificity is held fixed at 95%
#  MER MWL ER FPR FP FN




ICC(cbind(data_1stday$TaskA_r1, data_1stday$TaskA_r2))
ICC(cbind(data_1stday$TaskB_r1, data_1stday$TaskB_r2))
ICC(cbind(data_1stday$TaskC_r1, data_1stday$TaskC_r2))
ICC(cbind(data_1stday$TaskD_r1, data_1stday$TaskD_r2))
ICC(cbind(data_1stday$TaskE_r1, data_1stday$TaskE_r2))


ICC(cbind(data_2ndday$TaskA_r1, data_2ndday$TaskA_r2))
ICC(cbind(data_2ndday$TaskB_r1, data_2ndday$TaskB_r2))
ICC(cbind(data_2ndday$TaskC_r1, data_2ndday$TaskC_r2))
ICC(cbind(data_2ndday$TaskD_r1, data_2ndday$TaskD_r2))
ICC(cbind(data_2ndday$TaskE_r1, data_2ndday$TaskE_r2))


options(download.file.method = "wget")




fit.tasks <- glm(Athletes~TaskA+TaskB+TaskC+TaskD+TaskE, data=data_tasks, family="binomial")
fit.taskA <- glm(Athletes~TaskA, data=data_tasks, family="binomial")
fit.taskB <- glm(Athletes~TaskB, data=data_tasks, family="binomial")
fit.taskC <- glm(Athletes~TaskC, data=data_tasks, family="binomial")
fit.taskD <- glm(Athletes~TaskD, data=data_tasks, family="binomial")
fit.taskE <- glm(Athletes~TaskE, data=data_tasks, family="binomial")

pred_tasks = ifelse( predict( fit.tasks, type = "link" ) > 0, "Yes", "No" )
pred_taskA = ifelse( predict( fit.taskA, type = "link" ) > 0, "Yes", "No" )
pred_taskB = ifelse( predict( fit.taskB, type = "link" ) > 0, "Yes", "No" )
pred_taskC = ifelse( predict( fit.taskC, type = "link" ) > 0, "Yes", "No" )
pred_taskD = ifelse( predict( fit.taskD, type = "link" ) > 0, "Yes", "No" )
pred_taskE = ifelse( predict( fit.taskE, type = "link" ) > 0, "Yes", "No" )

# ==============================================================================
# We write a function which allows use to make predictions based on different probability cutoffs.

get_logistic_pred = function(mod, data, res = "y", pos = 1, neg = 0, cut = 0.5) {
  probs = predict(mod, newdata = data, type = "response")
  ifelse(probs > cut, pos, neg)
}

# obtain predictions using a low, medium, and high cutoff.
pred_10A = get_logistic_pred( fit.taskA, data = data_tasks, res = y, pos = "Yes", neg = "No",  cut = 0.1 )
pred_50A = get_logistic_pred( fit.taskA, data = data_tasks, res = y, pos = "Yes", neg = "No",  cut = 0.5 )
pred_90A = get_logistic_pred( fit.taskA, data = data_tasks, res = y, pos = "Yes", neg = "No",  cut = 0.9 )
pred_10B = get_logistic_pred( fit.taskB, data = data_tasks, res = y, pos = "Yes", neg = "No",  cut = 0.1 )
pred_50B = get_logistic_pred( fit.taskB, data = data_tasks, res = y, pos = "Yes", neg = "No",  cut = 0.5 )
pred_90B = get_logistic_pred( fit.taskB, data = data_tasks, res = y, pos = "Yes", neg = "No",  cut = 0.9 )
pred_10C = get_logistic_pred( fit.taskC, data = data_tasks, res = y, pos = "Yes", neg = "No",  cut = 0.1 )
pred_50C = get_logistic_pred( fit.taskC, data = data_tasks, res = y, pos = "Yes", neg = "No",  cut = 0.5 )
pred_90C = get_logistic_pred( fit.taskC, data = data_tasks, res = y, pos = "Yes", neg = "No",  cut = 0.9 )
pred_10D = get_logistic_pred( fit.taskD, data = data_tasks, res = y, pos = "Yes", neg = "No",  cut = 0.1 )
pred_50D = get_logistic_pred( fit.taskD, data = data_tasks, res = y, pos = "Yes", neg = "No",  cut = 0.5 )
pred_90D = get_logistic_pred( fit.taskD, data = data_tasks, res = y, pos = "Yes", neg = "No",  cut = 0.9 )
pred_10E = get_logistic_pred( fit.taskE, data = data_tasks, res = y, pos = "Yes", neg = "No",  cut = 0.1 )
pred_50E = get_logistic_pred( fit.taskE, data = data_tasks, res = y, pos = "Yes", neg = "No",  cut = 0.5 )
pred_90E = get_logistic_pred( fit.taskE, data = data_tasks, res = y, pos = "Yes", neg = "No",  cut = 0.9 )

tab_10BA = table( predicted = pred_10B, actual = pred_taskA )
tab_50BA = table( predicted = pred_50B, actual = pred_taskA )
tab_90BA = table( predicted = pred_90B, actual = pred_taskA )
tab_10CA = table( predicted = pred_10C, actual = pred_taskA )
tab_50CA = table( predicted = pred_50C, actual = pred_taskA )
tab_90CA = table( predicted = pred_90C, actual = pred_taskA )
tab_10DA = table( predicted = pred_10D, actual = pred_taskA )
tab_50DA = table( predicted = pred_50D, actual = pred_taskA )
tab_90DA = table( predicted = pred_90D, actual = pred_taskA )
tab_10EA = table( predicted = pred_10E, actual = pred_taskA )
tab_50EA = table( predicted = pred_50E, actual = pred_taskA )
tab_90EA = table( predicted = pred_90E, actual = pred_taskA )


mat_10BA = confusionMatrix( tab_10BA, positive = "Yes" )
mat_50BA = confusionMatrix( tab_50BA, positive = "Yes" )
mat_90BA = confusionMatrix( tab_90BA, positive = "Yes" ) 
mat_10CA = confusionMatrix( tab_10CA, positive = "Yes" )
mat_50CA = confusionMatrix( tab_50CA, positive = "Yes" )
mat_90CA = confusionMatrix( tab_90CA, positive = "Yes" ) 
mat_10DA = confusionMatrix( tab_10DA, positive = "Yes" )
mat_50DA = confusionMatrix( tab_50DA, positive = "Yes" )
mat_90DA = confusionMatrix( tab_90DA, positive = "Yes" ) 
mat_10EA = confusionMatrix( tab_10EA, positive = "Yes" )
mat_50EA = confusionMatrix( tab_50EA, positive = "Yes" )
mat_90EA = confusionMatrix( tab_90EA, positive = "Yes" ) 




# Metrics

metricsBA = rbind(
  c( mat_10BA$overall["Accuracy"], mat_10BA$byClass["Sensitivity"], mat_10BA$byClass["Specificity"] ),
  c( mat_50BA$overall["Accuracy"], mat_50BA$byClass["Sensitivity"], mat_50BA$byClass["Specificity"] ),
  c( mat_90BA$overall["Accuracy"], mat_90BA$byClass["Sensitivity"], mat_90BA$byClass["Specificity"] )
)
rownames( metricsBA ) = c( "c = 0.10", "c = 0.50", "c = 0.90" )

metricsCA = rbind(
  c( mat_10CA$overall["Accuracy"], mat_10CA$byClass["Sensitivity"], mat_10CA$byClass["Specificity"] ),
  c( mat_50CA$overall["Accuracy"], mat_50CA$byClass["Sensitivity"], mat_50CA$byClass["Specificity"] ),
  c( mat_90CA$overall["Accuracy"], mat_90CA$byClass["Sensitivity"], mat_90CA$byClass["Specificity"] )
)
rownames( metricsCA ) = c( "c = 0.10", "c = 0.50", "c = 0.90" )

metricsDA = rbind(
  c( mat_10DA$overall["Accuracy"], mat_10DA$byClass["Sensitivity"], mat_10DA$byClass["Specificity"] ),
  c( mat_50DA$overall["Accuracy"], mat_50DA$byClass["Sensitivity"], mat_50DA$byClass["Specificity"] ),
  c( mat_90DA$overall["Accuracy"], mat_90DA$byClass["Sensitivity"], mat_90DA$byClass["Specificity"] )
)
rownames( metricsDA ) = c( "c = 0.10", "c = 0.50", "c = 0.90" )

metricsEA = rbind(
  c( mat_10EA$overall["Accuracy"], mat_10EA$byClass["Sensitivity"], mat_10EA$byClass["Specificity"] ),
  c( mat_50EA$overall["Accuracy"], mat_50EA$byClass["Sensitivity"], mat_50EA$byClass["Specificity"] ),
  c( mat_90EA$overall["Accuracy"], mat_90EA$byClass["Sensitivity"], mat_90EA$byClass["Specificity"] )
)
rownames( metricsEA ) = c( "c = 0.10", "c = 0.50", "c = 0.90" )

metricsBA; metricsCA; metricsDA; metricsEA


test_probA =  predict( fit.taskA, newdata = data_tasks, type = "response" )
test_probB =  predict( fit.taskB, newdata = data_tasks, type = "response" )
test_probC =  predict( fit.taskC, newdata = data_tasks, type = "response" )
test_probD =  predict( fit.taskD, newdata = data_tasks, type = "response" )
test_probE =  predict( fit.taskE, newdata = data_tasks, type = "response" )

test_rocA  =  roc    ( data_tasks$Athletes ~ test_probA, plot = TRUE, print.auc = TRUE )
test_rocB  =  roc    ( data_tasks$Athletes ~ test_probB, plot = TRUE, print.auc = TRUE )
test_rocC  =  roc    ( data_tasks$Athletes ~ test_probC, plot = TRUE, print.auc = TRUE )
test_rocD  =  roc    ( data_tasks$Athletes ~ test_probD, plot = TRUE, print.auc = TRUE )
test_rocE  =  roc    ( data_tasks$Athletes ~ test_probE, plot = TRUE, print.auc = TRUE )

plot(test_rocA, col = 1, lty = 2, main = "ROC")
plot(test_rocB, col = 4, lty = 3, add = TRUE)
plot(test_rocC, col = 5, lty = 1, add = TRUE)
plot(test_rocD, col = 7, lty = 4, add = TRUE)
plot(test_rocE, col = 6, lty = 5, add = TRUE)

optim <- which.max( rowSums( as.data.frame( test_rocA[ c("sensitivities", "specificities") ] )))

# Get the best threshold

thresholds_best<-
rbind(
coords(test_rocA, "best", ret=c("threshold", "specificity", "sensitivity", "accuracy","precision", "recall"), transpose = FALSE),
coords(test_rocB, "best", ret=c("threshold", "specificity", "sensitivity", "accuracy","precision", "recall"), transpose = FALSE),
coords(test_rocC, "best", ret=c("threshold", "specificity", "sensitivity", "accuracy","precision", "recall"), transpose = FALSE),
coords(test_rocD, "best", ret=c("threshold", "specificity", "sensitivity", "accuracy","precision", "recall"), transpose = FALSE),
coords(test_rocE, "best", ret=c("threshold", "specificity", "sensitivity", "accuracy","precision", "recall"), transpose = FALSE)
)
rownames(thresholds_best) <- c( "TaskA", "TaskB", "TaskC", "TaskD", "TaskE" )

conf_intrvls_AUC <- cbind( ci(test_rocA), ci(test_rocB), ci(test_rocC), ci(test_rocD), ci(test_rocE) )
colnames(conf_intrvls_AUC) <- c( "TaskA", "TaskB", "TaskC", "TaskD", "TaskE" )

# CI of the curve
ci_curve<- rbind(
ci.se(test_rocA, specificities=seq(0, 1)),
ci.se(test_rocB, specificities=seq(0, 1)),
ci.se(test_rocC, specificities=seq(0, 1)),
ci.se(test_rocD, specificities=seq(0, 1)),
ci.se(test_rocE, specificities=seq(0, 1))
)
colnames(ci_curve) <- c( "sp.low", "sp.median", "sp.high")

# CI of thresholds
ci_thresholds <-   rbind( 
  as.data.frame( ci.thresholds(test_rocA )), 
  as.data.frame( ci.thresholds(test_rocB )), 
  as.data.frame( ci.thresholds(test_rocC )), 
  as.data.frame( ci.thresholds(test_rocD )),
  as.data.frame( ci.thresholds(test_rocE ))
)

# Power
power.roc.test(test_rocA)

# Sample size 
power.roc.test(test_rocA, power = 0.9)


roc_curve <- roc(data_tasks$Athletes ~ test_prob,
             percent=TRUE,                                                  # arguments for auc
             partial.auc=c(100, 90), partial.auc.correct=TRUE,
             partial.auc.focus="sens",                                      # arguments for ci
             ci=TRUE, boot.n=100, ci.alpha=0.9, stratified=FALSE,           # arguments for plot
             plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
             print.auc=TRUE, show.thres=TRUE)

