library(tidyverse)

df <- data.frame(aiu = 1:5, aboap = c('auyt','b','cop','d','e'), stringsAsFactors = TRUE)
type.convert(df)
str(df)

df2 <- data.frame(x = 1:3, y = I(list(1:2,1:3,1:4)))

dfm <- data.frame(x = 1:3, y = I(matrix(1:9, nrow = 3)))

df[3,'b']

mta <- matrix(1:9, nrow = 3)
colnames(mta) <- c('A','B','C')
mta[1:2,]
mta[9]

vals <- outer(1:5, 1:5, FUN = "paste", sep = ",")

df2 <- outer(LETTERS[1:6],letters[1:6],FUN = 'paste', sep=':')

df$a

x <- 1:4
str(x[[5]])

df[] <- lapply(df, as.integer)

x <- c("m", "f", "u", "f", "f", "m", "m")
lookup <- c(m = "Male", f = "Female", u = NA)
unname(lookup[x])

grades <- c(1, 2, 2, 3, 1)
info <- data.frame(
  grade = 3:1,
  desc = c("Excellent", "Good", "Poor"), fail = c(F, F, T)
)

id <- match(grades,info$grade)
info[id,]

