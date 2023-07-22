library(tidyverse)

df <- data.frame(a = 1:5, b = c('a','b','c','d','e'), stringsAsFactors = TRUE)
type.convert(df)
str(df)

df2 <- data.frame(x = 1:3, y = I(list(1:2,1:3,1:4)))

dfm <- data.frame(x = 1:3, y = I(matrix(1:9, nrow = 3)))

df[3,'b']

mta <- matrix(1:9, nrow = 3)
colnames(mta) <- c('A','B','C')
mta[1:2,]
mta[]