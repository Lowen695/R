
library(tidyverse)
library(Rmisc)
library(patchwork)
library(ggplot2)
# dir.create('/Users/fahuiliu/Desktop/R_Practice/first')
setwd('/Users/fahuiliu/Desktop/R_Practice/first')
# save.image('testrdata2')

qq
setwd('/Users/fahuiliu/Desktop/R_Practice/first/')
g <- getwd()
sink('output')

pdf('test2.pdf')
mm <- installed.packages()

lm(mpg ~ wt, data = mtcars)
lmfit <- lm(mpg ~ wt, data = mtcars)

summary(lmfit)
plot(lmfit)

library('vcd')
help(package = 'vcd')
help("Arthritis")
Arthritis
example("Arthritis")
load('testrdata2')


df1 <- mtcars
df2 <- table(mtcars$cyl, mtcars$disp)

attach(mtcars)
mpg
plot(mpg, disp)

detach(mtcars)

with(mtcars, {
  plot(mpg, disp)
  print(summary(mpg))
})

help(package = 'ggplot2')

diabetes <- c('type1', 'type2', 'type1', 'type2')
sink()

l1 <- rep(c('type1','type2'),3)
diabetes <- factor(diabetes)
diabetes[2]
mydata <- data.frame(age = numeric(5),
                     gender = character(5),
                     weight = numeric(5))
mydata <- edit(mydata)

setwd('/Users/fahuiliu/Desktop/R_Practice/first')
opar <- par(no.readonly = TRUE)
par(lty = 6, pch = 17) # set plot line types 
png('testpng.png')
attach(mtcars)
plot(wt, wt)
abline(lm(mpg ~ wt))
title('TEST TILE FOR PLOTTING')
detach(mtcars)
par(opar)
dev.off()

dev.new()
plot(x = 1:10, y = 1:10)
dev.new()
hist(rnorm(100))

library(RColorBrewer)
n <- 12
mcolors <- brewer.pal(n, 'YlGnBu')
barplot(rep(1, 12), col = mcolors)
display.brewer.all()
display.brewer.pal(9,'BrBG')

n <- 20
mycolors <- rainbow(n)

mygrays <- gray(0:n / n)
pie(rep(1, n), labels = mygrays, col = mygrays)
par(
  font.lab = 1,
  cex.lab = 1,
  font.main = 1,
  cex.main = 1
)
windowsFonts(
  A = windowsFont("Arial Black"),
  B = windowsFont("Bookman Old Style"),
  C = windowsFont("Comic Sans MS")
)
pie(rep(1, n), labels = mycolors, col = mycolors)

manager <- c(1, 2, 3, 4, 5)
date <- c("10/24/08", "10/28/08", "10/1/08", "10/12/08", "5/1/09")
country <- c("US", "US", "UK", "UK", "UK")
gender <- c("M", "F", "F", "M", "F")
age <- c(32, 45, 25, 39, 99)
q1 <- c(5, 3, 3, 3, 2)
q2 <- c(4, 5, 5, 3, 2)
q3 <- c(5, 2, 5, 4, 1)
q4 <- c(5, 5, 5, NA, 2)
q5 <- c(5, 5, 5, NA, 1)
leadership <- data.frame(manager,
                         date,
                         country,
                         gender,
                         age,
                         q1,
                         q2,
                         q3,
                         q4,
                         q5,
                         stringsAsFactors = FALSE)

leadership <- within(leadership, {sumx9x <- q2 + q3
sum88 <- sumx9x-88})

leadership$age[leadership$age == 99] <- NA
leadership$agecat[leadership$age > 75] <- 'Elder'
leadership$agecat[leadership$age >= 35 &
                    leadership$age <= 75] <- 'Middle Aged'
leadership$agecat[leadership$age < 35] <- 'Young'

names(leadership)[2] <- 'testDate'

leadership <- rename(leadership, c('manager' = 'manaderID'))
a <- is.na(leadership[, ])
leadership <- within(leadership, {
  test <- NA
})

leadership3 <- within(leadership, {
  newCol1 <- q1 + q2
  newCol2 <- q1*q2
  newCol3 <- ifelse(q2 > 4, 'too high', ifelse(q2 >3,'very high', 'high'))
})


library(ggplot2)

# Create a data frame with example data
data <-
  data.frame(
    x = rnorm(50),
    y = rnorm(50),
    group = sample(c("A", "B"), 50, replace = TRUE)
  )

# Create the plot
ggplot(data, aes(x = x, y = y)) +
  geom_point(size = 3,
             aes(fill = "blue", color = "black"),
             alpha = 1)

newdata <- leadership[order(country,age), ]
newdata2 <- leadership[, c(6:10)]
P<-paste0("q", 1:5, "-")
leadership[['q1']]

newdata <- leadership[leadership$gender == 'M' &
                        leadership$age > 30, ]
attach(leadership)
newdata <- leadership[gender == 'F' & age < 30, ]
detach(leadership)

leadership$testDate <- as.Date(leadership$testDate, "%M/%d/%Y")
startdate <- as.Date('2009-01-01')
enddate <- as.Date("2009-10-31")

as.Date('Jul-31-2023',"%b-%d-%Y")

newdata <- leadership[(leadership$testDate >= startdate &
                         leadership$testDate <= enddate), ]


newdata4 <- subset(leadership, manager == 1| manager == 2,
                  select = c(manager:q3))
newdata4 <- newdata4%>%
  select(-q2) %>% 
  filter(q3>3)

mysample <-
  leadership[, sample(1:ncol(leadership), 3, set.seed(99))]

apply(leadership, 2, '[[',2)

x <- c("ab", "cde", "fghij")
nchar(x[2])

x <- 'QOTNCBHAGLIUTHEAD'

sub('QO', '88', x)

substr(x, 2, 6)

y <-
  c('AFDFDSFDS',
    'DFIOLJKAFJD2006573BT02CD9ADFAJDHIUH',
    'AgYE99HT9999',
    'ADFDFDSF')

grep(
  '\\d?[A-Z]?\\d{2}[A-Z]{2}\\d{1}',
  y,
  ignore.case = TRUE,
  fixed = FALSE,
  value = TRUE
)

y[str_detect(y,regex('[A-Z]?\\d{2}[a-z]{2}\\d{4}',ignore_case = TRUE))]

str_split(y,3)

# Define the input string
y <- 'DFIOLJKAFJD2003BT02CD9ADFAJDHIUH'

# Define the regular expression pattern
pattern <- '\\d{4}[A-Z]{2}\\d{2}[A-Z]{2}\\d{1}'

# Use regmatches to extract the matched substring from the input string
matched_substring <-
  regmatches(y, regexpr(pattern, y, ignore.case = TRUE))

# Print the matched substring
matched_substring


strsplit('adfadf78opadfd', '8', fixed = FALSE)

str_replace(y,'99','@@')
cat('Hello', 'Bob', '\n', 'How are you?',sep='@') # RETURN NULL
paste0('Hello', 'Bob', '\n', 'How are you?',sep='&')

mydata <- matrix(rnorm(30), nrow = 5)


options(digits = 2)
Student <- c(
  "John Davis",
  "Angela Williams",
  "Bullwinkle Moose",
  "David Jones",
  "Janice Markhammer",
  "Cheryl Cushing",
  "Reuven Ytzrhak",
  "Greg Knox",
  "Joel England",
  "Mary Rayburn"
)

Math <- c(502, 600, 412, 358, 495, 512, 410, 625, 573, 522)
Science <- c(95, 99, 80, 82, 75, 85, 80, 95, 89, 86)
English <- c(25, 22, 18, 15, 20, 28, 15, 30, 27, 18)

roster <- data.frame(Student, Math, Science, English,
                     stringsAsFactors = FALSE)
                  

z <- scale(roster[, 2:4])
score <- apply(z, 1, sum)
roster <- cbind(roster, score)
y <- quantile(score, c(.8, .6, .4, .2))

roster$grade[English >= 20] <- 'A' # think about this
roster$grade[score < y[1] & score >= y[2]] <- "B"
roster$grade[score < y[2] & score >= y[3]] <- "C"
roster$grade[score < y[3] & score >= y[4]] <- "D"
roster$grade[score < y[4]] <- "F"

roster$grade <- NULL # delete column

roster[English > 20, 'Math'] <- '888'
names(roster)[4:6] <- str_to_title(names(roster)[4:6])
type.convert(roster)
roster$Math <- as.numeric(roster[[2]])
roster <- mapply(c,roster,cbind(data.frame('Student'='Total'),transpose(data.frame(sapply(roster[2:4],sum)))))

tt <- data.frame(t(sapply(roster[2:4],sum)))

name <- strsplit(roster$Student, ' ')
name[1]
Lastname <- sapply(name, '[', 2)
Firstname <- sapply(name, '[', 1)
apply(roster[3], 2, max)


roster <- cbind(Firstname, Lastname, roster[, -1])
roster2 <- roster[order(Firstname), ]

mystats <- function(x,
                    parametric = TRUE,
                    print = FALSE) {
  if (parametric) {
    center <- mean(x)
    spread <- sd(x)
  } else {
    center <- median(x)
    spread <- mad(x)
  }
  
  if (print & parametric) {
    cat("Mean=", center, "\n", "SD=", spread, "\n")
  } else if (print & !parametric) {
    cat("Median=", center, "\n", "MAD=", spread, "\n")
  }
  result <- list(center = center, spread = spread)
  return(result)
}

set.seed(888)
x <-  rnorm(500)
y <- mystats(x)

mydate <-  function(type = 'long') {
  switch(
    type,
    long = format(Sys.time(), '%A %B %d %Y'),
    short = format(Sys.time(), '%m-%d-%Y'),
    cat(type, 'is not a recognized type\n')
  )
}

mydate('GOGOLE')


options(digits = 3)

attach(mtcars)
aggdata <- aggregate(mtcars,
                     by = list(cyl, gear),
                     FUN = mean,
                     na.rm = TRUE)

leadership[!complete.cases(leadership), ]

sum(is.na(leadership[4, ]))

newLeadership <- na.omit(leadership)

ggplot(data = mtcars, aes(x = wt, y = mpg)) +
  geom_point(size = mpg/5,  color = "yellow",alpha=0.4) +
  geom_point(size = mpg/5,  color = "darkgreen", pch = 21,alpha=0.4) +
  scale_fill_identity() +
  labs(title = 'TEST cars data using ggplot2', x = 'Weight', y = 'Miles per gallon')


data("mtcars")

mtcars$am <- factor(mtcars$am,
                    levels = c(1, 0),
                    labels = c('automatic', 'manual'))
mtcars$vs <- factor(
  mtcars$vs,
  levels = c(0, 1),
  labels = c("V-Engine", "Straight Engine")
)
mtcars$cyl <- factor(mtcars$cyl)
ggplot(data = mtcars, aes(
  x = hp,
  y = mpg,
  shape = cyl,
  color = cyl
)) +
  geom_point(size = 3, position = 'jitter') +
  geom_vline(xintercept = 210,
             linetype = 2,
             size = 0.2) +
  facet_grid(am ~ vs) +
  labs(title = "Automobile Data by Engine Type",
       x = "Horsepower", y = "Miles Per Gallon")

ggplot(mtcars, aes(x = wt, y = hp, color = as.factor(vs))) +
  scale_color_brewer(palette = 'Set1') + #-------------------------------------
  geom_boxplot() +
  geom_jitter(alpha = 0.7) +
  facet_grid(cyl ~ .) +
  labs(title = 'Car variables parameters per year')


library(RColorBrewer)
display.brewer.all()


ggplot(data = mpg) +
  geom_point(aes(x = displ, y = hwy, color = class)) +
  geom_boxplot(mapping = aes(x = displ, y = hwy, fill = class))

p4 <- ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut, fill = clarity), position = "fill") +
  coord_flip()
#scale_fill_brewer(palette = "Set1")
test <- select(mtcars, 'drat' | 'hp')
test2 <- filter(mtcars, row.names == 'Valiant')
test3 <- slice(test, 1:2)

(test4 <- mtcars['Merc 280', ])
near(sqrt(2) ^ 2, 2)

test5 <- filter(newdata2, !is.na(q4))
test6 <- arrange(leadership, desc(q1), age,q5)
test7 <- select(leadership, contains('der'))

newTest6 <- rename(test6,
                   'Region' = q1,
                   'City' = q2,
                   'Birth' = q3)
select(test5, num_range('q', 2:4))
select(newTest6, q4, everything())

paste0('q',1:8)

newTest7 <- mutate(newTest6, Gain = paste0(gender[4], country[4]))

sumTest <- group_by(newTest6, gender, country) %>%
  summarise(ave_age = mean(age, na.rm = TRUE))

last(test6$age)
first(test6$age)
nth(test6$age, 3)
min_rank(test6$age)

sum(is.na(test6$age))
n_distinct(test6$age)

count(test6, gender)
count(test6, country, wt = age)

newTest8 <- na.omit(test6)
newTest8$q2 <- newTest8$q3 <- NULL

newTest9 <- subset(test6, age > 30, select = 1:6)

newTest10 <- test6[order(test6$age), ]
strTest <- 'woshi34.tue8tt'
sub('.tu', '88', strTest)
substr(strTest, 3, 5)
grep('34.', strTest)

describe(newTest)
newTest11 <- as_tibble(test6)

newTest12 <- tibble('A' = 9,
                    'B' = paste('AGE-', 1:20),
                    'C' = 0)
tibbleTest <- tibble(
  a = lubridate::now() + runif(1e3) * 86400,
  b = lubridate::today() + runif(1e3) * 30,
  c = 1:1e3,
  d = runif(1e3),
  e = sample(letters, 1e3, replace = TRUE)
)

readCSV <- read_csv('test.csv', col_names = FALSE)
str(parse_datetime(c("10121979")))
x <- parse_number("1.23", locale = locale(grouping_mark = ','))
problems(x)
write_csv(readCSV, 'test2.csv')

newTest13  <- newTest10 %>%
  mutate(addC = n())

newTest14 <- newTest9 %>%
  separate(
    testDate,
    into = c('Year', 'Month', 'Day'),
    sep = c('-'),
    convert = TRUE
  )

newTest15 <- newTest14 %>%
  unite(newYEAR, Year,Month, Day, sep = '-')

stocks <- tibble(
  year = c(2015, 2015, 2015, 2015, 2016, 2016, 2016),
  qtr = c(1, 2, 3, 4, 2, 3, 4),
  return = c(1.88, 0.59, 0.35, NA, 0.92, 0.17, 2.66)
)


stocks2 <- stocks %>%
  spread(year, return) %>%
  gather(year, return, c('2015', '2016'), na.rm = TRUE)


stocks8 <- stocks2 %>%
  complete(year, qtr)

stocks2 %>%
  fill(return)

view(who)
who2 <- who %>%
  gather(code, value, new_sp_m014:newrel_f65, na.rm = TRUE) %>%
  mutate(code = sub('newrel', 'new_rel', code)) %>%
  separate(code, c('new', 'var', 'sexage'), sep = '_') %>%
  select(-new, -iso2, -iso3) %>%
  separate(sexage, c('sex', 'age'), sep = 1)

library(nycflights13)
view(airlines)

planes %>%
  count(tailnum) %>%
  filter(n > 1)


f3 <- flights %>%
  count(year, day)

view(flights)

flights2 <- flights %>%
  select(-origin, -dest, -hour, -(month:arr_delay)) %>%
  anti_join(airlines, by = 'carrier')



intersect(flights, flights2)

writeLines(bquote('Number VS' ~ Number ^ 2))

title <- expression("Plot with Superscript in Title: " ^ 2)

str_length('wo shi qian wenwu')

str_c('Average', "_", 1:5)
s <- c('AB', 'BU', NA)
str_c(str_replace_na(s), '@')

II <- str_c('Average', "_", 1:5, sep = '@', collapse = '---')

data.frame(str_extract_all('88 %89.87 gadsote 980.56 66', '\\d+'))[1,1]
 
str_view('ADKADFKLJHADFDFDFBAFQADF', 'DF')

str_sub('mamabmib5f', 3, 6)

l1 <- list()
l1 <- c(l1, '88')
l1 <- append(l1, '66')

d1 <- as.tibble(l1, c('A', 'B'))


str_detect(c('apple', 'banana', 'orange', 'app'), '^ap')

str_subset(c('apple', 'banana', 'orange', 'app'), '^ap')

x1 <- c("Dec", "Apr", "Jan", "Mar")
month_levels <- c("Jan",
                  "Feb",
                  "Mar",
                  "Apr",
                  "May",
                  "Jun",
                  "Jul",
                  "Aug",
                  "Sep",
                  "Oct",
                  "Nov",
                  "Dec")




y1 <- factor(x1, levels = month_levels)
sort(y1)

f1 <- factor(x1)
f1

library(forcats)


gss_cat %>%
  filter(marital != 'No answer') %>%
  ggplot(aes(x = marital, y = tvhours, color = race)) +
  geom_jitter() +
  scale_color_brewer(palette = 'Set1')+
  geom_boxplot() +
  # scale_color_brewer(palette = 'PuOr') +
  labs(title = expression('TEST TITLE' ~ pms))

p3 <- gss_cat %>%
  filter(marital == 'Divorced') %>%
  ggplot(aes(race, fill = relig)) +
  # geom_jitter()+
  geom_bar(position = 'dodge',
           color = 'black',
           width = 0.5) +
  scale_color_brewer(palette = 'PuOr') +
  labs(title = expression( ~ 'Count divorced'[by][' '][race])) +
  theme_update() +
  scale_x_discrete(drop = FALSE)

gss_cat %>%
  count(race)

relig <- gss_cat %>%
  group_by(relig) %>%
  summarize(
    age = mean(age, na.rm = TRUE),
    tvhours = mean(tvhours, na.rm = TRUE)
  )

p1 <- ggplot(relig, aes(tvhours, fct_reorder(relig, tvhours))) + geom_point()
print(p1)

by_age <- gss_cat %>%
  filter(!is.na(age)) %>%
  group_by(age, marital) %>%
  count() %>%
  mutate(prop = n / sum(n))
ggplot(by_age, aes(age, prop, color = marital)) +
  geom_line(na.rm = TRUE)
p2 <- ggplot(
  by_age,
  aes(age, prop, color = fct_reorder2(marital, age, prop)) )+
  geom_line() +
  labs(color = "marital")


# R 图像布局 -------------------------------------------------------


pdf('test.pdf',width = 8,height = 10,title = 'TEST PDF SIZE')

layout <- "
##BB
AA##
CCCC
"
(p4 +p2) - p3 + 
  plot_layout(nrow = 2,byrow = FALSE, guides = 'collect') + #design = layout
  inset_element(p2, left = 0.2, bottom = 0.6, right = 1, top = 1) +
  plot_annotation(
    title = 'The surprising truth about mtcars',
    subtitle = 'These 3 plots will reveal yet-untold secrets about our beloved data-set',
    caption = 'Disclaimer: None of these plots are insightful'
  )
dev.off()


# functions ---------------------------------------------------------------

devide_zero <- function(x,y){
  if (y == 0){
    stop("y cannot be 0",call. = FALSE)
  }else{
    x/y
  }
}

LL <- devide_zero(4,2)

str_c(c('A','B','C'), sep='-', collapse = '-')
example("str_c")

LETTERS


a <- NaN
is.na(a)

order

x <- list(1,2,3)
str(x[1:2])
str(x[[1:2]])

attributes(x)

attr(x,'test1') <- list(2,2,2)
attr(x,'test2') <- 'good2'

attributes(x)

tb <- tibble::tibble(x = 1:5,y = 5:1)
names(tb)
class(tb)
row.names(tb) <- LETTERS[1:5]

df <- tibble(
  a = rnorm(100),
  b = rnorm(100),
  c = rnorm(100),
  d = rnorm(100)
)

output <- vector(mode  = 'list', length = ncol(df))
for (i in seq_along(df)) {
  output[[i]] <- median(df[[i]]);
}



map_dbl(df,mean,trim=0.1)


for (i in df) {
  print(i)
}

models2 <- mtcars %>%
  split(.$cyl) %>%
  map(~lm(mpg ~ wt, data = .))

split(mtcars$vs)
mtcars %>%
  split(.$cyl) %>%
  do.call(rbind,.) %>%
  ggplot(aes(x = gear,y=wt))+
  geom_boxplot()
rnorm()
l1 <- list('a','bb','c')
seq_along(l1)
l2 <- list()
l2[['TEST']] <- l1
l2$TEST
models3[1]

l1 <- list('a','b','c','d')
l2 <- list('A','B','C')
l3 <- list(2,3,5,8)
l4 <- list(8,9,10,20)

rs <- Map(function (x,y) x+y, l3,l4)
m1 <- matrix(l3)
m2 <- matrix(l4)
mm <- matrix(as.numeric(m1) + as.numeric(m2))

ll <- c(l1,l2)
ll[[1]]

seq_along(l1)

paste0(l1,l2)
library(ggplot2)
plots <- mtcars %>%
  split(.$cyl) %>%
  map(~ggplot(.,aes(mpg,wt)) + geom_point())
print(plots)

paths <- stringr::str_c(names(plots), ".pdf")

pwalk(list(paths,plots),ggsave, path = '/Users/fahuiliu/Desktop/R_Practice/first') #-----------------------

y <- vector("double", 0) 
seq_along(y)
1:length(y)

dfs <- list(
  age = tibble(name = "John", age = 30),
  sex = tibble(name = c("John", "Mary"), sex = c("M", "F")),
  trt = tibble(name = "Mary", treatment = "A")
)

l1 <- c(x,y)
dfs7 <- l1 %>% reduce(rbind)

dfs2 <- dfs %>% reduce(append)%>%
  as.data.frame() %>%  gather(key = 'sex',value='Values',-c('name.1'))
dfs3 <- dfs2 %>%  separate(name.1, c('new1','new2'))

leadership6 <- gather(leadership2, 'gender','age',key = 'newColName', value='NewValue')
leadership7 <- spread(leadership6, key = newColName, value = NewValue)
leadership7 <- spread(leadership6,new1 = type, value=count)
x <- list('a','b','c')
y <- list('A','B','C')

z <- x %>% accumulate('paste')

# transform cannot use the varible just created. mutate can, but it cannot modify the variable again. within can do both.

x <- list(
  c(1, 2),
  c(3, 4),
  c(5, 6)
)

do.call(sum,x)
map(x,sum)
reduce(x,sum)

sink()
library(data.table)
rbindlist(x,y)
fread()
leadership

