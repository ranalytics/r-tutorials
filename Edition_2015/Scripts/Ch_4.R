
#  Мастицкий С.Э., Шитиков В.К. (2015) Статистический анализ и визуализация 
#  данных с помощь R. Купить книгу можно здесь: http://bit.ly/1QopZ0A


#########################################################################
#       Глава 4. ОПИСАТЕЛЬНАЯ СТАТИСТИКА И ПОДГОНКА РАСПРЕДЕЛЕНИЙ
#########################################################################


# ------------------------- Раздел 4.1 --------------------------------
# Оценка выборочных характеристик:
data(mtcars)
head(mtcars)

# Среднее арифметическое:
mean(mtcars$mpg)

# Медиана:
median(mtcars$mpg)

# Дисперсия:
var(mtcars$mpg)

# Стандартное отклонение:
sd(mtcars$mpg)

# Минимальное значение:
min(mtcars$mpg)

# Максимальное значение:
max(mtcars$mpg)

# Стандартная ошибка среднего:
SEmpg = sd(mtcars$mpg)/sqrt(length(mtcars$mpg))

# Квантили:
quantile(mtcars$mpg)
quantile(mtcars$mpg, p = seq(0, 1, 0.1))

# Интерквантильный размах:
IQR(mtcars$mpg)

# Отсутствующие значения:
mtcars$mpg[3] <- NA

# Просмотрим результат:
head(mtcars$mpg)
mean(mtcars$mpg)
mean(mtcars$mpg, na.rm = TRUE)
length(mtcars$mpg)
sum(!is.na(mtcars$mpg))

# Использование функции which:
which.min(mtcars$mpg)
which.max(mtcars$mpg)
rownames(mtcars)[which.min(mtcars$mpg)]
rownames(mtcars)[which.max(mtcars$mpg)]

# Использование функции apply:
tapply(X = mtcars$disp, INDEX = mtcars$am, FUN = mean)
tapply(X = mtcars$disp, INDEX = list(mtcars$am, mtcars$vs), FUN = mean)
SE <- function(x) {sd(x)/sqrt(length(x))}
tapply(X = mtcars$disp, INDEX = mtcars$am, FUN = SE)

	
# ------------------------- Раздел  4.2 -------------------------------	
# Использование функции summary():
summary(mtcars)
summary(mtcars$mpg)
mtcars$vs <- as.factor(mtcars$vs)
mtcars$am <- as.factor(mtcars$am)

# Проверим, удалась ли конвертация:
is.factor(mtcars$vs)
is.factor(mtcars$am)
summary(mtcars)

# Использование функций других пакетов
library(moments)  #загрузка пакета moments
kurtosis(mtcars$mpg, na.rm = TRUE)
skewness(mtcars$mpg, na.rm = TRUE)

# Пакет Hmisc, функция describe():
library(Hmisc)
describe(mtcars)

# Пакет pastecs, функция stat.desc():
library(pastecs)
stat.desc(mtcars)

# Пакет psych, функция describe.by() - расчет параметров
# описательной статистики для каждого уровня некоторого фактора:
library(psych)
describe.by(mtcars, mtcars$am)

# Пакет doBy, функция summaryBy():
library(doBy)
summaryBy(mpg + wt ~ cyl + vs, data = mtcars,
          FUN = function(x) { c(m = mean(x), s = sd(x)) } )

	
# ------------------------- Раздел 4.3 ---------------------------------	
# Рис. 42:
Sparrows = read.table("http://bit.ly/1AOZbZs", sep = "\t", header = T)
dotchart(Sparrows$wingcrd, xlab = "Длина крыла (мм)",
         ylab = "Порядковый номер", lcolor = NA)

#  Преобразование Бокса-Кокса:
molluscs <-
        read.table("http://figshare.com/media/download/98923/97987",
                   header=TRUE, sep="\t", strip.white=TRUE)

# Поиск максимума функции правдоподобия и построние графика
# изменения параметра БК-трансформации для заданной модели:
library(car)
m.null <- lm(molluscs$CAnumber+1~1)
bc.null <- boxCox(m.null)
bc.null.opt <- bc.null$x[which.max(bc.null$y)]
paste("Оптимальная лямбда БК-преобразования:", bc.null.opt)
CAnumber_bc <- bcPower(molluscs$CAnumber+1, bc.null.opt)

# Рис. 44:
par(mfrow = c(2, 1))
boxplot(molluscs$CAnumber, horizontal = TRUE, col = "steelblue",
        xlab = "Численность инфузорий (экз)")
boxplot(CAnumber_bc, horizontal = TRUE, col = "coral",
        xlab = "Численность инфузорий (БК-преобразование)")

#  Тест Граббса:
library(outliers)
grubbs.test(molluscs$CAnumber, type = 10)
grubbs.test(CAnumber_bc, type = 10)

	
# -------------------------- Разделу 4.4 -------------------------------	
# Заполнение пропусков:
library(VIM)
data(sleep, package = "VIM")
head(sleep)

# Список строк, в которых нет пропущенных значений:
sleep[complete.cases(sleep), ]

# Список строк, в которых есть хотя бы одно пропущенное значение:
sleep[!complete.cases(sleep), ]
sum(is.na(sleep$Dream))


library(mice)
md.pattern(sleep)
matrixplot(sleep)

# Формируем матрицу со значениями 1 в местах пропусков:
x <- as.data.frame (abs (is.na(sleep)))
y <- x[, which(colSums(x) > 0)]
print(cor(y),4)
cor(sleep, x, use = "pairwise.complete.obs")

imp <- mice(sleep, seed = 1234)
fit <- with(imp, lm(Dream ~ Span + Gest))
pooled <- pool(fit)
summary(pooled)

# Заполнение пропусков и сохранение результата для использования
# в последующих примерах:
sleep_imp3 <- complete(imp, action = 3)
head(sleep_imp3)
sleep[!complete.cases(sleep_imp3), ]
save(sleep_imp3, file = "sleep_imp.Rdata")


# -------------------------- Разделу 4.5 ---------------------------
# Воспроизводимость случайных чисел:
example = data.frame(
        Factor = rep(c("A", "B", "C"), each = 300),
        Variable = c(rnorm(300, 5, 2), rnorm(300, 4, 3),
                     rnorm(300, 2, 1)))

tapply(example$Variable, example$Factor, summary)

# Рис. 46:
example1 = data.frame(
  Factor = rep(c("A", "B", "C"), each = 300),
  Variable = c(rnorm(300, 5, 2),
               rnorm(300, 4, 3),
               rnorm(300, 2, 1)))

example2 = data.frame(
  Factor = rep(c("A", "B", "C"), each = 300),
  Variable = c(rnorm(300, 5, 2),
               rnorm(300, 4, 3),
               rnorm(300, 2, 1)))

example3 = data.frame(
  Factor = rep(c("A", "B", "C"), each = 300),
  Variable = c(rnorm(300, 5, 2),
               rnorm(300, 4, 3),
               rnorm(300, 2, 1)))

example4 = data.frame(
  Factor = rep(c("A", "B", "C"), each = 300),
  Variable = c(rnorm(300, 5, 2),
               rnorm(300, 4, 3),
               rnorm(300, 2, 1)))

p1 = ggplot(example1, aes(Variable, group = Factor, fill = Factor)) +
        geom_density(alpha = 1/2)
p2 = ggplot(example2, aes(Variable, group = Factor, fill = Factor)) +
        geom_density(alpha = 1/2)
p3 = ggplot(example3, aes(Variable, group = Factor, fill = Factor)) +
        geom_density(alpha = 1/2)
p4 = ggplot(example4, aes(Variable, group = Factor, fill = Factor)) +
        geom_density(alpha = 1/2)

library(gridExtra) # для функции grid.arrange()
grid.arrange(p1, p2, p3, p4, ncol = 2)

# Рис. 47:
set.seed(1020)
example.fixed = data.frame(
  Factor = rep(c("A", "B", "C"), each = 300),
  Variable = c(rnorm(300, 5, 2),
               rnorm(300, 4, 3),
               rnorm(300, 2, 1)))

p = ggplot(example.fixed, aes(Variable, group = Factor, fill = Factor)) +
  geom_density(alpha = 1/2)

grid.arrange(p, p, p, p, ncol = 2)

	
# ------------------------ Разделу 4.6 -----------------------
#  Законы распределения вероятностей, реализованные в R:
dnorm(-1)
pnorm(-1)
qnorm(p = c(0.25, 0.75))
rnorm(10, mean = 0, sd = 1)

# Код для рис. 48:
dstr = rnorm(5000)
polyCurve <- function(x, y, from, to, n = 50, miny,
                      col = "red", border = col) {
        drawPoly <- function(fun, from, to, n = 50, miny, col, border) {
                Sq <- seq(from = from, to = to, length = n)
                polygon(x = c(Sq[1], Sq, Sq[n]),
                        y = c(miny, fun(Sq), miny),
                        col = col, border = border)
        }
        lf <- length(from)
        stopifnot(identical(lf, length(to)))
        if(length(col) != lf)
                col <- rep(col, length.out = lf)
        if(length(border) != lf)
                border <- rep(border, length.out = lf)
        if(missing(miny))
                miny <- min(y)
        interp <- approxfun(x = x, y = y)
        mapply(drawPoly, from = from, to = to, col = col, border = border,
               MoreArgs = list(fun = interp, n = n, miny = miny))
        invisible()
}
plot(sort(dstr), (1:5000)/5000, type = "l", pch = 19, col = "blue",
     panel.first = polyCurve(sort(dstr), (1:5000)/5000, 
                       from = -3, to = -1,
                       col = "red", border = "black"),
     ylab = "P", xlab = "x")
abline(v = -1, lty = 2)
abline(h = pnorm(-1), lty = 2)


# -------------------------- Раздел 4.8 --------------------------------
# Подбор закона и параметров распределения в R
library(MASS)
set.seed(0)
x.gam <- rgamma(200, rate = 0.5, shape = 3.5) 

# Аналитический путь:
med.gam <- mean(x.gam)                  # выборочное среднее 
var.gam<-var(x.gam)                     # выборочная дисперсия 
(l.est <- med.gam/var.gam)              ## -оценка лямбда
(g.est <- ((med.gam)^2)/var.gam) 	## -оценка гамма

# Общий подход:
library(rootSolve)
f1 <- function(x){c(F1 = x[1]/x[2] - med.gam, F2 = x[1]/x[2]^2 - var.gam)}
multiroot(f1, c(3, 0.6))

set.seed(1946)
x = sort(rweibull(100, 2, (1 + 1.21*rbinom(100, 1, 0.05)) ))
x  # Отсортированные значения 
summary(x)  # Выборочные характеристики

# Рис. 49:
hist(x, freq = FALSE, breaks = 8, col = "grey88", 
     main="Гистограмма и ядерная плотность")
lines(density(x), lwd = 2, col = "blue")

# Функция для вывода графиков теоретической и эмпирической КФР и ФПР:
graph_distr <- function(x, pc, pd, main_name = "")
{ 
        op <- par(mfrow = c(1, 1), pty = "s")
        par(mfrow = c(1, 2))
        mn <- paste(c("Эмпирическая КФР и ", main_name))
        plot(x,pc, type = "l", col = "red", lwd = 2, main = mn) 
        plot(ecdf(x), add = TRUE) 
        mn <- paste(c("Эмпирическая ФПР и ", main_name))
        plot(density(x), lwd = 2, col = "blue", main = mn) 
        lines(x, pd, col = "red", lwd = 2)
        par(op)
}

# Рис. 50:
# оценка параметров нормального распределения: 
(dof <- fitdistr(x,"normal"))
ep1 <- dof$estimate[1]; ep2 <- dof$estimate[2]
ks.test(x,pnorm, mean = ep1, sd = ep2)
graph_distr(x, pnorm(x, mean = ep1, sd = ep2),
            dnorm(x, mean = ep1, sd = ep2),
            "нормального распределения")

# оценка параметров лог-нормального распределения:
(dof <- fitdistr(x,"log-normal")) 
ep1 <- dof$estimate[1]; ep2 <- dof$estimate[2]
ks.test(x, plnorm, meanlog = ep1, sdlog = ep2)
graph_distr(x, plnorm(x, meanlog = ep1, sdlog = ep2),
            dlnorm(x, meanlog = ep1, sdlog = ep2),
            "логнормального распределения")

library(fitdistrplus)
(dof <- fitdist(x, "weibull"))
ep1 <- dof$estimate[1]; ep2 <- dof$estimate[2]
ks.test(x, pweibull, shape = ep1, scale = ep2)
graph_distr(x, pweibull(x, scale = ep1, shape = ep2),
            dweibull(x, scale = ep1, shape = ep2),
            "распределения Вейбулла")

# Рис. 51:
x <- c(12,20,19,19,18,10,19,30,16,10,8,11,10,11,16,3,7,6,5,11,
       8,14,9,8,10,11,14,17,2,7,17,19,9,15,9,8,4,8,11,8,5,3,10,
       14,22,11,8,7,3,5,8,11,14,2,13,9,12,6,19,21)

# Оценка параметров распределений нормального и Пуассона:
n <- length(x); p1 <- mean(x); p2 <- sqrt(var(x)*(n-1)/n)

# Создание векторов эмпирических и теоретических частот:
pr_obs <- as.vector(table(x)/n); nr <- length(pr_obs) 
pr_norm <- dnorm(1:nr, p1, p2) # Частоты нормального распр.
pr_pois <- dpois(1:nr, p1)     # Частоты распр. Пуассона
plot(pr_obs, type = "b", ylab = "Частоты") 
lines(1:nr, pr_pois , col = "red", lwd = 2) 
lines(1:nr, pr_norm, col = "blue", lwd = 2)
legend("topright", legend = c("Нормальное", "Пуассона"),
       lwd = 2, col = c("red", "blue"))

# Сравнение качества подгонки распределений

# Среднее абсолютное отклонение:
c(sum(abs(pr_obs - pr_norm))/nr, sum(abs(pr_obs - pr_pois))/nr)

# Среднеквадратичная ошибка:
c(sum((pr_obs - pr_norm)^2)/nr, sum((pr_obs - pr_pois)^2)/nr)

# Критерий согласия Колмогорова-Смирнова:
c(ks.test(pr_obs, pr_norm)$statistic, ks.test(pr_obs, pr_pois)$statistic)

library(vcd)   ##  Visualizing Categorical Data
gf <- goodfit(table(x), type = "poisson", method = "ML")

# Визуализация подогнанных данных гистограммой (рис. 52):
plot(gf, ylab = "Частота", xlab = "Число классов")



# ------------------------- Раздел 4.8 ---------------------------

# Рис. 53:
Sparrows <- read.table(file = "SparrowsElphick.txt", header = TRUE)

Sparrows$fMonth <- factor(Sparrows$Month,
                          levels = c(5, 6, 7, 8, 9, 10),
                          labels = c("May", "Июнь", "Июль", "Август",
                                     "Sept.", "Oct."))

Sparrows$I1 <- Sparrows$fMonth =="Июнь" |
        Sparrows$fMonth =="Июль" |
        Sparrows$fMonth =="Август"

p1 = ggplot(filter(Sparrows, I1 == TRUE), aes(wt)) + 
        geom_histogram(fill = "white", color = "black") +
        theme_bw() + xlab("Вес (г)") + ylab("Частота")

p2 = ggplot(filter(Sparrows, I1 == TRUE), aes(wt)) + 
        geom_histogram(fill = "white", color = "black") +
        facet_wrap(~fMonth, ncol = 1) + theme_bw() +
        xlab("Вес (г)") + ylab("Частота")

library(gridExtra)
grid.arrange(p1, p2, ncol = 2)


# Проверка на нормальность распределения методом огибающих
set.seed(1946)
x <- sort(rweibull(100, 2, (1 + 1.21*rbinom(100, 1, 0.05)) ))
c(mean(x), sd(x))

# Квантиль-квантильный график:
qqnorm(x); qqline(x)

# Определяем и рисуем огибающие (рис. 54)
# (подробности п. 4.2.4 в Davison & Hinkley (1997)
z <- (x - mean(x))/sqrt(var(x))  #  Стандартизация выборки
x.qq <- qqnorm(z, plot.it = FALSE)
x.qq <- lapply(x.qq, sort)
plot(x.qq, type = "n", ylim = c(-2, 5), 
     ylab = "Z-статистики выборки", 
     xlab = "Квантили НР")

library(boot)
# Генерация 999 бутстреп-выборок (т.е. случайных выборок из 
#  нормального распределения с параметрами выборки z):
x.gen <- function(dat, mle) rnorm(length(dat))
x.qqboot <- boot(z, sort, R = 999, 
                 sim = "parametric",ran.gen = x.gen)
sapply(1:999,function(i) lines(x.qq$x, x.qqboot$t[i,],
                               type = "l", col = "grey"))
points (x.qq, pch = 20)
lines(c(-3, 3), c(-3, 3), col = "red", lwd = 2)

# Рис. 55:
library(car)
qqPlot(x, dist = "norm", col = palette()[1], pch = 19,
       xlab="Квантили нормального распределения", 
       ylab="Наблюдаемые квантили", 
       main="Сравнение квантилей ЭР и НР")

# Рис. 56:
library(sm)
sm.density(x, model = "Normal", xlab = "Имитированная выборка",
           ylab = "Функция плотности распределения")

# Тесты на нормальность:
shapiro.test(x)
library(nortest)
ad.test(x)
cvm.test(x)
lillie.test(x)
sf.test(x)
