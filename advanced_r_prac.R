library(tidyverse)
library(formatR)
library(readxl)
source("Score_Conv.R")
# options(error = browser)
options(error = NULL)
options(error = traceback)

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
str(x[5])

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


# section 1 ---------------------------------------------------------------

x <- sample(10)<4
which(x)
formals('pivot_longer')


a <- 0
j <- function() {
  if (!exists("a")) { a <- 1
  } else {
    a <- a + 1
  } 
  print(a) 
}
j() 

rm(j)
f <- function(a) g(a)
g <- function(b) h(b)
h <- function(c) i(c)

i <- function(d) "a" + d 

f(10)

x=1
y <- 'x/0' + 6
print('finished')
traceback()

fix_missing <- function(x) {
  x[x == 5] <- NA 
  x
}
df[] <- lapply(df, fix_missing)

dfNew <- expand.grid(type = c('A','B','C'), class = c('X','L','XL'))

x <- c('优','中','良','优','良','良','中')
x
x1 <- factor(x, levels = c('中','良','优'))
x1
sort(x1)

unique(x)

table(x1)

Age <- c(23,15,36,47,65,53,88)
cut(Age, breaks = c(0,18,45,100), labels = c('Yong','Middle','Old'))

Sex <- gl(2, 3, length = 12, labels = c("男","女"))

df = as_tibble(iris[,1:4])
output <- list('double',length(df))
for (i in seq_along(df)){
  output[[i]] = df[[i]]
}
output
output2 <- data.frame(matrix(flatten_dbl(output),ncol = 4))
output3 <- bind_cols(output)

Score_Conv(55)

mtcars %>%
  group_split(cyl) %>%
  map(~lm(mpg ~ wt, data = .x))

# ~ is needed for anonymous functions only in purrr context. 
str(df)
v2 <- data.frame(t(data.frame(c(1.2,2.3,4.4,6.1),c(8,8,8,8))))
names(v2) <- names(df)
DF <- add_row(df, v2, .before = 2)
DF2 <- rows_append(df,v2)
rownames(DF2) <- 1:nrow(DF2)

dt <- tribble(
  ~observation, ~A_count, ~B_count, ~A_dbh, ~B_dbh,
  "Richmond(Sam)",   7,       2,   100,   110,
  "Windsor(Ash)",   10,       5,   80,     87,
  "Bilpin(Jules)",   5,       8,   95,     90)
  knitr::kable(dt, align="c")

tidy_dt <- dt %>% 
  pivot_longer(-observation, names_to = c('Species','.value'),
               names_sep = '_') %>% 
  separate(observation, into = c('site','sureyor'))
  
 dff <- read_xlsx('/Users/fahuiliu/Desktop/untitled folder/数据集/配套数据/ExamDatas_NAs.xlsx') 
dff %>% select(where(is.numeric))

dff %>% 
  select(where(is.numeric)) %>% 
  select(where(~ sum(.x, na.rm = TRUE) >3000))

dff %>% relocate(where(is.numeric),.after = class) %>% 
  rename(Math = math)

dff %>% rename_with(~paste0('NEW-',.x),matches('m'))

dff %>% mutate(across(.cols=c(4,5),.fns = function(x) x+88), 
               across(.cols=c(6,7),.fns = ~.x/3))

DFF2 <- dff %>% mutate(chinese = case_when(chinese >= 80 ~ 'Perfect',
                                chinese >= 60 ~ 'good',
                                is.na(chinese) ~ ' ',
                                TRUE ~ 'unqualified'))

