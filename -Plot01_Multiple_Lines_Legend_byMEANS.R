library(psych)
library(dplyr)
library(reshape)
library("ggplot2")

library(ggeffects)
library(effects)


pth="/home/"
file="data_r.csv"

file0="Group_0.csv"
file20="Group_20.csv"
file40="Group_40.csv"
file80="Group_80.csv"

# Reading csv file
data_0     = read.csv( paste0(pth, file), header = TRUE, sep = ",", quote = "" )
str(data_0)

data_g0     = read.csv( paste0(pth, file0), header = TRUE, sep = ",", quote = "" )
str(data_g0)
data_g20     = read.csv( paste0(pth, file20), header = TRUE, sep = ",", quote = "" )
str(data_g20)
data_g40     = read.csv( paste0(pth, file40), header = TRUE, sep = ",", quote = "" )
str(data_g40)
data_g80     = read.csv( paste0(pth, file80), header = TRUE, sep = ",", quote = "" )
str(data_g80)


bp<-
  ggplot(data=data_g0,aes(x = Time,y=data_g0$Nonprogressive_p_0))+
  geom_errorbar(data=data_g0,width=0.1,size=0.5,aes(ymin=Nonprogressive_p_0-sdNonprogressive_p_0,ymax=Nonprogressive_p_0+sdNonprogressive_p_0))+
  geom_line(aes(y=Nonprogressive_p_0,colour="Ομάδα Ελέγχου"))+
  
  geom_line(data = data_g20,aes(x=Time,y=data_g20$Nonprogressive_p_20,colour="Ομάδα 20"))+
  geom_errorbar(data=data_g20,width=0.1,size=0.5,aes(ymin=Nonprogressive_p_20-sdNonprogressive_p_20,ymax=Nonprogressive_p_20+sdNonprogressive_p_20),colour="red")+
  
  geom_line(data = data_g40,aes(x=Time,y=data_g40$Nonprogressive_p_40,colour="Ομάδα 40"))+
  geom_errorbar(data=data_g40,width=0.1,size=0.5,aes(ymin=Nonprogressive_p_40-sdNonprogressive_p_40,ymax=Nonprogressive_p_40+sdNonprogressive_p_40),colour="blue")+
  
  geom_line(data = data_g80,aes(x=Time,y=data_g80$Nonprogressive_p_80,colour="Ομάδα 80"))+
  geom_errorbar(data=data_g80,width=0.1,size=0.5,aes(ymin=Nonprogressive_p_80-sdNonprogressive_p_80,ymax=Nonprogressive_p_80+sdNonprogressive_p_80),colour="brown")+
  
  scale_x_continuous(limits=c(-0.1,2.1), breaks=c(0,1,2), labels=(paste(c('0H','1H','2H'),sep='')) )+
  labs(x = "Χρόνος για Συνολική Κινητικότητα", y = "",  x = "Time") +
  labs(fill='Legend Title') +
  theme_light()   

bp + theme(plot.margin = unit(c(2,2,2,2), "cm"), 
           axis.title.x = element_text(vjust=0),
           axis.title.y = element_text(angle=90, vjust=0),
           legend.spacing = unit(c(1), "cm"),
           legend.key.height = unit(c(1), "cm"),
           panel.grid.major = element_line(colour=rgb(0.87,0.87,0.87)),
           panel.grid.minor = element_blank(),
           plot.background = element_rect(fill = rgb(0.97,0.97,0.97), linetype=0) )

bp + theme(legend.position="bottom", legend.title = element_text(colour="black", size=12, face="bold")) +
  guides(color = guide_legend(title = "Ομάδες")) 



