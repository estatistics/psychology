library(psych)
library(dplyr)
library(car)
library("gplots")
library("ggpubr")
# install.packages("ggpubr")


pth="/home/"

file="AA__data_01.csv"

pth="/home/

file="data.csv"

# Reading csv file
data_0     = read.csv( paste0(pth, file), header = TRUE, sep = ",", quote = "" )
str(data_0)


# One Way Anova
data_0$Age <- f(data_0$Age,labels=c("<30 ΧΡ","31-40 ΧΡ",">41 ΧΡ"))
data_0$Age <- ordered(data_0$Age,  levels = c("<30 ΧΡ","31-40 ΧΡ",">41 ΧΡ"))

# Graphs

ggboxplot(data_0, x = "Age", y = "f_1", 
          color = "Age", palette = c("lightgray", "grey30", "black"),
          fill=c("lightgray", "grey30", "black"),
          order = c("<30 ΧΡ","31-40 ΧΡ",">41 ΧΡ"),
          ylab = "f_1", xlab = "Treatment")


par(mfrow=c(1,3),mar=c(6,3,1,1) )
lapply( 1:length(levels(data_0$Age)), function(x) 
multi.hist(data_0[data_0$Age == levels(data_0$Age)[x],]$f_1, 1, density=T, 
           bcol="lightyellow", dcol=c("white","black"), 
           main="", ylab="", yaxt='n')  )

multi.hist(data_0$f_1, 1, density=T, 
           bcol="lightyellow", dcol=c("white","black"), 
           main="", ylab="", yaxt='n') 

par(mfrow=c(2,3),mar=c(6,3,1,1) )
lapply( 1:length(levels(data_0$Age)), function(x) 
  hist(data_0[data_0$Age == levels(data_0$Age)[x],]$f_1,
       col="lightyellow", main="", ylab="", xlab=paste0("Ηλικία ", levels(data_0$Age)[x]), yaxt='n') )

# Mean plots
ggline(data_0, x = "Age", y = "f_1", 
       add = c("mean_se", "jitter"), 
       order = c("<30 ΧΡ","31-40 ΧΡ",">41 ΧΡ"),
       ylab = "f_1", xlab = "Treatment")

plotmeans(f_1 ~ Age, data = data_0, frame = FALSE,
          xlab = "", ylab = "",
          main="Mean Plot with 95% CI") 


# Compute the analysis of variance
res.aov <- aov(f_1 ~ Age, data = data_0)
# Summary of the analysis
summary(res.aov)

# t Pairwise comparisons
pairwise.t.test(  data_0$f_1, g = data_0$Age, p.adjust.method = "bonferroni", paired =F)

# 1. Homogeneity of variances
plot(res.aov, 1)

# levene
leveneTest(f_1 ~ Age, data = data_0)

# Extract the residuals
aov_residuals <- residuals(object = res.aov )
# Run Shapiro-Wilk test
shapiro.test(x = aov_residuals )


#############################################################


# Repeatd Anova
modelAOV <- aov(value~f(variable)+Error(f(ID)), data = melt_data)
print(summary(modelAOV))
obtF <- summary(modelAOV)$"Error: Within"[[1]][[4]][1]

# Preparation - Repeatd Anova with Maunchly's test
data_new<-as.matrix(data_0[4:8])
model<-lm(data_new ~ 1)
design<-f(colnames(data_0[4:8]) )

data_new2 <- as.data.frame(data_new)
colnames(data_new2) <- f(colnames(data_0[4:8]) )
melt_data <- melt(data_0[c(1,4:8)],id.vars = "ID")


options(contrasts=c("contr.sum", "contr.poly"))
results<-Anova(model, idata=data.frame(design), idesign=~design, type="III")
summary(results, multivariate=F)

ptt <- pairwise.t.test(melt_data$value, melt_data$variable, paired=TRUE,
                       alternative="two.sided", p.adjust.method="bonferroni")


plotmeans(value ~ variable, data = melt_data, frame = FALSE,
          xlab = "Treatment", ylab = "Weight",
          main="Mean Plot with 95% CI") 
