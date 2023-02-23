library(psych)
library(dplyr)
library(reshape)
library("gplots")
library("ggpubr")
# install.packages("ggpubr")

pth="/home/output/"

file="data.csv"

# Reading csv file
data_0     = read.csv( paste0(pth, file), header = TRUE, sep = ",", quote = "" )
str(data_0)

write.csv(x = datas_100, file =,append = F, sep = ";", dec = ".")


dt<-data_0[c(1,5:23)]
cor_df <- cor(dt[-1])

# KMO
KMO(cor_df)

melt_dt <- melt( dt, id="ID")
bt <-bartlett.test(value ~ variable, data = melt_dt)

# Principal Components and Factor Analysis
fit <- princomp(dt[-1], cor=TRUE)

plot(fit,type="l") 

