library(gplots)
library(ggpubr)
library(psych)

# install.packages("ggpubr")



pth  = "/home/elias/Data/STATISTICS/papaioanou/"
file = "data_short.csv"

# Reading csv file
data_0     = read.csv( paste0(pth, file), header = TRUE, sep = ",", quote = "" )

dt<- data_0[,84:99]


par(mfrow=c(1,2))
boxplot(dt, 
        data=data_0, 
        col=c("plum","tan","grey30",
              "thistle", "lightslategray", 
              "lavender", "seashell",
              "yellow", "green", "brown",
              "black"),
        main="Θηκογράμματα", 
        xlab="",
        ylab="",
        names.arg = colnames( dt ),
        las=1, 
        cex.axis=0.4 ) 


par(mfrow=c(1,1),oma=c(2,1,1,1), mar=c(6,3,1,1) )
multi.hist(data_0[-1], 2, density=T, 
           bcol="lightyellow", dcol=c("white","black"), 
           main=c("111","11"), ylab="", yaxt='n')


ggboxplot(data_0, x = "ΗΛΙΚΙΑ", y = "factor_1", 
          color = "ΗΛΙΚΙΑ", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
          order = c("<30 ΧΡ","31-40 ΧΡ",">41 ΧΡ"),
          ylab = "factor_1", xlab = "Treatment")

barplot( height = as.numeric( d[1,] ), 
         col=c("plum","tan","grey30"), 
         names.arg = colnames(d),
         las = 1, border = NA,
         main = "Is",
         ylab = "Coolness Level",
         xlab = "ID #"
         )





