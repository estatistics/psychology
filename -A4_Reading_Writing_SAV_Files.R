library(haven)

# Reading sav file
df_sav <- read_sav("/home/elias/Data/STATISTICS/Niki/data_pspp.sav", user_na=T)

# Reading csv file
pth="/home/elias/Data/STATISTICS/Niki/Data_For_Analysis/"
file="data_whole.csv"

data_0     = read.csv( paste0(pth, file), header = TRUE, sep = ",", quote = "" )

df_sav$Περιφέρεια <- trimws( data_0$Περιφέρεια )


write_sav(df_sav, "/home/elias/Data/STATISTICS/Niki/data_pspp_0.sav")

