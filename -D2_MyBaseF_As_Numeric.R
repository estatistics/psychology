#as numeric
ndata_0 <- lapply(data_0, function(x) as.numeric(as.character( x )) )
ndata_0 <- as.data.frame( ndata_0 )