#########################################################################
#########################################################################

#  Мастицкий С.Э., Шитиков В.К. (2014) Статистический анализ и визуализация 
#  данных с помощь R. (Адрес доступа: http://r-analytics.blogspot.com)

#########################################################################

#########################################################################
# Глава 2.	ОПИСАНИЕ ЯЗЫКА R
#########################################################################

# ----------------------------------------------------------------------
# К разделу 2.2.
# ----------------------------------------------------------------------	
# Примеры создания векторов:
my.vector <- c(1, 2, 3, 4, 5)
my.vector

S <- seq(1,7)
S

S <- seq(from = 1, to = 5, by = 0.5)
S

Text <- rep("test", 5)
Text

v1 <- c(1, 2, 3)
v2 <- c(4, 5, 6)
V <- c(v1, v2)
V

text.vect <- c("a", "b", "c")
new.vect <- c(v1, text.vect)
new.vect
mode(new.vect)

y <- c(5, 3, 2, 6, 1)
z <- c(0.5, 0.1, 0.6)
y[1]*z[3]
z[2] <- 0.3

# Сортировка векторов:
sort(z) # по умолчанию  decreasing = FALSE
sort(z, decreasing = TRUE)

# Примеры создания матриц:
my.mat <- matrix(seq(1, 16), nrow = 4, ncol = 4)
my.mat

my.mat <- matrix(seq(1, 16), nrow = 4, ncol = 4, byrow = TRUE)
my.mat

rownames(my.mat) <- c("A", "B", "C", "D")
my.mat

my.mat2 <- matrix(seq(1, 12), nrow = 4, ncol = 4, byrow = TRUE)
my.mat2

my.mat <- 1:16

# Задаем размерность 4x4 вектору my.mat:
dim(my.mat) <- c(4, 4)
my.mat
my.mat[2, 3]
my.mat[, 4]
my.mat[1, ]
t(my.mat)

a <- c(1, 2, 3, 4)
b <- c(5, 6, 7, 8)
d <- c(9, 10, 11, 12)
e <- c(13, 14, 15, 16)

# Объединим этим векторы при помощи функции cbind():
cbind(a, b, d, e)

# Объединим те же векторы при помощи функции rbind():
rbind(a, b, d, e)

#-----------------------------------------------------------------------	
#  К разделу 2.3.
#-----------------------------------------------------------------------	

# Примеры создания и конвертирования факторов:
treatment <- c(1, 1, 1, 1, 1, 1, 0, 0, 0, 0)
treatment <- factor(treatment, levels = c(0, 1))
levels(treatment) <- c("no", "yes")
as.numeric(treatment)

my.fac = gl(2, 8, labels = c("Control", "Treatment"))
my.fac

x <- c(1,2,3,4,5,2,3,4,5,6,7)
cut(x, breaks=3)
cut(x, breaks=3,  labels = letters[1:3])
cut(x,breaks=quantile(x,c(0, .25, .50, .75, 1)),
    labels=c("Q1","Q2","Q3","Q4"), include.lowest=TRUE)
 
#-----------------------------------------------------------------------	
#  К разделу 2.4.
#-----------------------------------------------------------------------	

# Cоздадим три разнотипных вектора - с текстовыми,
# числовыми и логическими значениями:
vector1 <- c("A", "B", "C")
vector2 <- seq(1, 3, 0.5)
vector3 <- c(FALSE, TRUE)
my.list <- list(Text=vector1, Number=vector2, Logic=vector3)

# Просмотрим содержимое созданного списка:
my.list
my.list$Text
my.list$Number
my.list$Logic
my.list$Number[3:5]
my.list[[1]]
my.list[[1]][2]
str(my.list)

city <- c("City1", "City1", "City2", "City2", "City3", "City3")
sex <- c("Male", "Female", "Male", "Female", "Male", "Female")
number <- c(12450, 10345, 5670, 5800, 25129, 26000)
CITY <- data.frame(City = city, Sex = sex, Number = number) 
CITY
CITY$Sex
CITY[,2]
CITY["Sex"]
CITY$Number[1:3] 

# Извлекаем все значения численности, превышающие 10000:
CITY$Number[CITY$Number > 10000] 

# Извлекаем все значения численности мужского населения:
CITY$Number[CITY$Sex == "Male"] 

# Повторяем те же команды, но с использованием []:
CITY[4, 3]
CITY[1:3, 3]
CITY[CITY$Number >10000, 3]
CITY[CITY$Sex == "Male", 3] 
str(CITY)
names(CITY)
head(CITY, n = 3)

# Сортировка таблиц:
DF <- data.frame(X1=c(1,15,1,3), X2=c(1,0,7,0), X3=c(1,0,1,2),
                 X4=c(7,4,41,0), X5=c(1,0,5,3))
row.names(DF) <- c("A","B","C","D")

#  DF1 - таблица, столбцы которой отсортированы 
#  по убыванию суммы значений:
DF1 <-  DF[ , rev(order(colSums(DF)))]

# DF2 - таблица, строки которой отсортированы в восходящем
#  порядке по 1 столбцу, затем в нисходящем по второму:
DF2 <- DF[order(DF$X1, -DF$X2), ]

# Объединение таблиц
DF1 <- matrix(c(
12,	22,	0,	1,	0,
12,	23,	1,	3,	0,
12,	24,	0,	0,	1),
nrow = 3, ncol = 5, byrow = TRUE)

colnames(DF1) <- c("Y",	"N",	"A",	"B",	"C")

DF2 <- matrix(c(
13,	22,	0,	1,	2,
13,	23,	0,	3,	0,
13,	24,	1,	0,	5),
nrow = 3, ncol = 5, byrow = TRUE)

colnames(DF2) <- c("Y",	"N",	"A",	"B",	"D")

DF1 <-as.data.frame(DF1)
DF2 <-as.data.frame(DF2)

cbind(DF1, DF2)

DF1[, names(DF2)[!(names(DF2) %in% names(DF1))]] <- NA
DF2[, names(DF1)[!(names(DF1) %in% names(DF2))]] <- NA
rbind(DF1, DF2)
merge(DF1, DF2, all = TRUE)

#-----------------------------------------------------------------------	
#  К разделу 2.6.
#-----------------------------------------------------------------------	

# Примеры  конвертирования переменных даты-времени:
Sys.time()
substr(as.character(Sys.time()), 1, 10)
substr(as.character(Sys.time()), 12, 19)
unclass(Sys.time())
date <- as.POSIXlt(Sys.time())
date$wday
date$yday
unlist(unclass(date))

t1 <- as.POSIXlt("2011-09-15")
t2 <- as.POSIXlt("2000-09-15") 
t1 - t2
t3<-as.POSIXlt("2010-09-22 08:30:30")
t4<-as.POSIXlt("2010-09-22 22:25:30")
t4-t3
difftime("2011-09-22", "2010-06-22")
as.numeric(difftime("2011-09-22", "2010-06-22"))

t1 <- proc.time()
for (x in 1:10000) y <- atan(x)
time.result <- proc.time() - t1
time.result["elapsed"]

dates.excel <- c("25/02/2008", "24/04/2009", 
                 "14/06/2009", "25/07/2010", "04/03/2011")
strptime(dates.excel, format = "%d/%m/%Y")
example2 <- c("1jan79", "2jan99", "31jan04", "30aug05")
strptime(example2, "%d%b%y") # не работает с русской локалью!

# Объект типа "временной ряд":
birth <- scan("http://robjhyndman.com/tsdldata/data/nybirths.dat")
head(birth)
birth.ts <- ts(birth, start = c(1946, 1), frequency = 12)
birth.ts
plot(birth.ts, xlab = "", ylab = "Рождаемость, тыс. чел.") 

#-----------------------------------------------------------------------	
#  К разделу 2.7.
#-----------------------------------------------------------------------	

# Примеры  создания собственных функций.

# Последовательность команд:
{aver <- mean(1:10); stdev <- sd(1:10); c(MEAN=aver, SD=stdev)}

# Функция:
stat_param <- function(x){
aver <- mean(x); stdev <- sd(x); c(MEAN=aver, SD=stdev)}
stat_param (1:10)

my_exampl <- function(n, func_trans){
        x <- runif(n) ; abs(func_trans(x))
}
my_exampl(5, log)

# Сравнение длин двух векторов:
compare <- function(x, y){ 
  nl <- length(x); n2 <- length(у)
  if(nl!= n2) {
    if(nl  > n2){
      z=(nl - n2)
      cat("Первый вектор имеет на ", z, " элементов 6ольше \n") } else {
        z=(n2 - nl)
        cat("Второй вектор имеет на ", z, " элементов 6ольше \n") } } else {
    cat("Количество элементов одинаково ", nl, "\n") } 
}

х <-  c(1:4)
у <-  c(1:9)
compare(х,  у)

# Вычисление доверительных интервалов среднего бутстрепом:
boot_np  <- function(data , Nboot = 5000) {
  boots <- numeric(Nboot) # Пустой вектор для хранения результатов
  for (i in 1:Nboot) { boots[i] <- mean(sample(data,replace=T)) }
  CI <-  quantile(boots, prob = c(0.025, 0.975)) 
  return (c(m = mean(data),CI))
}

x <-c(5, 5, 8, 10, 10, 10, 19, 20, 20, 20, 30, 40, 42, 50, 50)
boot_np(x)  

# Вычисление доверительных интервалов среднего через квантили t-распределения:
param_CI <- function(data) {
        n = length(data) ; m = mean(data) 
        SE = sd(data)/sqrt(n) ; E = qt(.975, df=n-1)*SE
        CI <- m + c(-E, E)
        return (c(m , CI))
}

param_CI(x)

#-----------------------------------------------------------------------	
#  К разделу 2.8.
#-----------------------------------------------------------------------

# Создадим обычную двумерную матрицу:
M <- matrix(seq(1, 16), 4, 4)

# Найдем минимальные значения в каждой строке матрицы
apply(M, 1, min)

# Найдем минимальные значения в каждом столбце матрицы
apply(M, 2, max)

# Пример с трехмерным массивом:
M <- array(seq(32), dim = c(4,4,2))

# Применим функцию sum()к кадому элементу M[*, ,],
# т.е. выполним суммирование по измерениями 2 и 3:
apply(M, 1, sum)

# Применим функцию sum() к каждому элементу  M[*, *, ],
#  - т.е. выполним суммирование по третьему измерению:
apply(M, c(1,2), sum)

x <- list(a = 1, b = 1:3, c = 10:100) 
# Выясним размер каждого компонента списка х
lapply(x, FUN = length) 

# Выполним суммирование элементов в каждом компоненте списка х:
lapply(x, FUN = sum) 

# Список из трех компонентов:
x <- list(a = 1, b = 1:3, c = 10:100)

# Выясним размер каждого компонента списка х:
sapply(x, FUN = length)

# Суммирование всех элементов в каждом компоненте списка х:
sapply(x, FUN = sum)   


# Доверительные интервалы медианы бутстреп-методом
boot_med <- function(x, y, N=100) {
        replicate(N, {
                indx <- sample.int(length(x), length(x), replace=T)
                indy <- sample.int(length(y), length(y), replace=T)
                median(x[indx]) - median(y[indy])
  })
}

Y <- rnorm(100, sd=4, mean=10)
X <- rnorm(100, sd=4, mean=10)

quantile(boot_med(Y,X,10000), probs = c(0.025, 0.975))

# Примеры модификаций apply:
mapply(sum, 1:5, 1:5, 1:5) 

myFun <- function(x){
    if (is.character(x)){
    return(paste(x,"!",sep=""))
    }
    else{
    return(x + 1)
    }
}

# Пример вложенного списка:
l <- list(a = list(a1 = "Boo", b1 = 2, c1 = "Eeek"), 
          b = 3, c = "Yikes", 
          d = list(a2 = 1, b2 = list(a3 = "Hey", b3 = 5)))

rapply(l,myFun)
rapply(l, myFun, how = "replace")

x <- sample(1:4, size=50,  replace=T)
gr <- as.factor(sample(c("A","B","C","D"),size=50, replace=T))
tapply(x, gr,sum) 


molluscs <- read.table("http://figshare.com/media/download/98923/97987",
                       header=TRUE, sep="\t", na.strings="NA",
                       dec=".", strip.white=TRUE)

by(molluscs[, 4:5], molluscs$Lake, colMeans)

# Примеры функции outer:
x <- 1:5;  y <- 1:5
outer(x, y)

x <- c("А", "В", "С", "D") 
y <- 1:10
outer(x,  y, paste, sep="")
