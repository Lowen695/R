library(tidyverse)
library(formatR)
library(readxl)
library(svDialogs)
library(plotly)

source("Score_Conv.R")
# options(error = browser)
options(error = NULL)
options(error = recover)

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


set.seed(123)
df_dup <- dff %>% 
  slice_sample(n = 60, replace = TRUE)

df_dup %>% filter(between(math,70,80), sex == '男')

df_dup %>% filter(if_all(everything(),~!is.na(.x)))
df_dup %>% filter(if_any(everything(),~is.na(.x)))
df_dup %>%filter(if_any(where(~is.character(.x)), ~is.na(.x)))

dff %>% slice_max(math,n=3)

df_dup %>% distinct(sex, math,.keep_all = FALSE)

df_dup %>% drop_na()

dup_rows <- duplicated(df_dup) | duplicated(df_dup, fromLast = TRUE)
df_dup_all_duplicated <- df_dup[dup_rows, ]

df_dup_all_duplicated <- arrange(df_dup_all_duplicated, desc(name))

df_grouped <- dff %>% group_nest(sex)
test <- df_grouped[[2]][[1]]

df_grouped2 <- dff %>% group_by(sex) %>% group_map(~slice_head(.x,n=3)) # 3 separated dataframes
df_grouped2[1]
df_grouped3 <- dff %>% group_by(sex) %>% slice_head(n=3)     # one dataframe
df_grouped4 <- dff %>% group_by(sex) %>% mutate()



# group summarise ---------------------------------------------------------

dff %>% group_by(sex) %>% summarise(across(where(is.numeric),~nth(na.omit(.),n=1)))
dff %>% group_by(class) %>% summarise(across(where(is.numeric),c(sum=sum,mean=mean,min=min),na.rm=TRUE)) %>% 
  pivot_longer(-class, names_to = c('VARS','.value'),names_sep = '_')

dff %>% count(class,sex, sort = TRUE)
dff %>% group_by(math_level = cut(math,breaks = c(0,60,75,80,100),right = FALSE)) %>% tally()

dff %>% add_tally(sex)
dff %>% group_by(math_level = cut(math,breaks = c(0,60,75,80,100),right = FALSE)) %>% add_tally()

dfRowwise <- dff %>% 
  rowwise() %>% 
  mutate(total = sum(c_across(where(is.numeric)), na.rm = TRUE)) # SLOW

dfRowwise2 <- dff %>% mutate(total = rowSums(across(where(is.numeric)),na.rm = TRUE))

dfRowwise3 <- dff %>% mutate(total = pmap_dbl(select_if(., is.numeric), sum, na.rm=TRUE))


if (!require(svDialogs)) uninstall.packages("svDialogs")

options <- c("Option 1", "Option 2", "Option 3","Option 4")
res <- dlg_list(c(month.name,options), multiple = TRUE)
str_split(res$res, ' ')


# ggplot2 -----------------------------------------------------------------

best_in_class = mpg %>% # 选取每种车型hwy值最大的样本 group_by(class) %>%
  slice_max(hwy, n = 1)

p1 <- ggplot(mpg,aes(x = displ, y = hwy,color = cyl))+
  geom_point(size=3)+
  geom_point(shape = 21,size=3,color='black')+
  # scale_color_gradient(low = 'orange',high='blue')+
  scale_y_continuous(breaks = seq(15,55,by = 5))+
  labs(title = 'Ggplot2 test', x = 'X-TEST',y = 'Y-TEST', color = 'CYL')+
  coord_cartesian(ylim = c(10, 50))+
  scale_color_distiller(palette = 'Set2')+
  theme(legend.position = "right")+
  annotate(geom = 'text', x = c(2,4),y=48,label = c('a1','a2'),angle=90)+
  geom_label(data = best_in_class, aes(label = model))

ggplotly(p1)



# error handling ----------------------------------------------------------

div <- function(m, n){
  if(!is.numeric(m) | !is.numeric(n)){
    stop('error: inputs are not numeric!')
  }else if (n == 0){
    warning('n cannot be 0!')
    88
  }else{
    m/n
  }
}

tryCatch(div(3,0),
         error = function(err) err,
         warning = function(warn) cat(paste0(warn,'be carefull the inf!'))
         )






