
pth  = "/home/elias/Data/STATISTICS/elena lamp/"
file = "ct_empeiria.csv"

# Reading csv file
data_0       = read.csv( paste0(pth, file), header = F, sep = ",", quote = "" )

df_data      = data_0

which_lines <- which( gregexpr( fixed = T,  pattern = "Statistic", text = df_data[ ,1 ] )   == 1 )

df_data[which_lines+1,]
