
#  Мастицкий С.Э., Шитиков В.К. (2015) Статистический анализ и визуализация 
#  данных с помощь R. Купить книгу можно здесь: http://bit.ly/1QopZ0A


#########################################################################
#         Глава 5. КЛАССИЧЕСКИЕ МЕТОДЫ И КРИТЕРИИ СТАТИСТИКИ
#########################################################################


# ------------------------- Раздел 5.1 --------------------------------
# Одновыборочный t-критерий  
d.intake <- c(5260, 5470, 5640, 6180, 6390, 6515,
              6805, 7515, 7515, 8230, 8770)
mean(d.intake)
t.test(d.intake, mu = 7725)

# Сравнение двух независимых выборок:
library(ISwR)
data(energy)
attach(energy)
head(energy)
tapply(expend, stature, mean)

t.test(expend ~ stature)
t.test(expend ~ stature, var.equal = TRUE)

# Сравнение двух зависимых выборок:
data(intake) # из пакета ISwR
attach(intake)
head(intake)
post - pre
mean(post - pre)

t.test(pre, post, paired = TRUE)


# ---------------------------- Раздел  5.2 -----------------------------
# Ранговый критерий Уилкоксона-Манна-Уитни

#   Одновыборочный критерий:
d.intake <- c(5260, 5470, 5640, 6180, 6390, 6515,
              6805, 7515, 7515, 8230, 8770)
wilcox.test(d.intake, mu = 7725)

# Сравнение двух независимых выборок:
library(ISwR)
data(energy)
attach(energy)

wilcox.test(expend ~ stature, paired = FALSE)

# Сравнение двух зависимых выборок:
data(intake) # из пакета ISwR
attach(intake)
wilcox.test(pre, post, paired = TRUE)
wilcox.test(pre, post, paired = TRUE, conf.int = TRUE)

	
# --------------------------- Раздел 5.3 -------------------------------	
# Построение графиков распределения критерия

# Функция, выполняющая пермутацию t-критерия заданное число раз
simP <- function(x, group, permutations = 5000) {
        n <- length(x)
        replicate(permutations, {
                xr <- sample(x, n)
                t.test(xr ~ group)$statistic  
        })
}
#  Функция, выполняющая бутстреп t-критерия заданное число раз
bootP <- function(x, group, boots=5000) {
        n <- length(x)
        replicate(boots, {
                ind <- sample.int(n, n, replace=T)
                t.test(x[ind] ~ group[ind])$statistic  
        })
}

#  Выполнение расчетов:
data(energy, package = "ISwR")
attach(energy)
t.emp <- t.test(expend ~ stature)$statistic
t.perm <- simP(expend , stature, 1000)
t.perm2 = simP(expend-100, stature, 1000)
t.boot <- bootP(expend , stature, 1000)
t.boot2 <- bootP(expend, stature, 1000)

mm <- range(c(t.emp, t.perm, t.boot))
mm # размах значений t:

# псевдо-P:
p <- (sum(abs(t.perm)- abs(t.emp) >= 0)+1) / (1000 + 1)


# Доверительный интервал:
CI <- quantile(t.boot, probs = c(0.025, 0.975))


plot(density(t.boot), col = "blue", lwd = 2,
     xlim = c(-10, 5), ylim = c(0, 0.4), main = "",
     xlab = "", ylab = "Плотность вероятности")
#  Заливаем полигон доверительной области
x1 <- min(which(dens$x >= CI[2]))  
x2 <- max(which(dens$x <  CI[1]))
with(dens, polygon(x = c(x[c(x1, x1:x2, x2)]), 
                   y= c(0, y[x1:x2], 0), col="gray"))
abline(v = t.emp,col = "green", lwd = 2 )
lines(density(t.perm), col = "red", lwd = 2)
lines(density(t.boot-5), col = "black", lwd = 2)
text(-8, 0.36, "Область H1")
text(-3.1, 0.36, "t(obs)")
text(2, 0.36, "Область Н0")


# Анализ статистической мощности

# Расчет минимального объема выборки:
power.t.test(delta = 3.0, sd = 1.8, sig.level = 0.05, power = 0.8)

#  Расчет мощности критерия:
power.t.test(n = 15, delta = 3.0, sd = 1.8, sig.level = 0.05)
power.t.test(delta = 3.0, sd = 1.8, sig.level = 0.05, 
             power = 0.8, type  = "paired")
power.t.test(delta = 3.0, sd = 1.8, sig.level = 0.05, 
             power = 0.8, type  = "one.sample")

	
# ---------------------------- Раздел 5.4 ------------------------------
#  Оценка однородности дисперсий в двух группах:
data(energy, package = "ISwR")
attach(energy)
var.test(expend ~ stature)

#  Оценка однородности дисперсий в нескольких группах:
data(InsectSprays)
library(car)
leveneTest(count ~ spray, data = InsectSprays)
leveneTest(count ~ spray, data = InsectSprays, center = mean)
bartlett.test(count ~ spray, data = InsectSprays)
fligner.test(count ~ spray, data = InsectSprays)

	
# ---------------------------- Раздел 5.5 ------------------------------
# Однофакторный дисперсионный анализ

# Создадим таблицу с данными:
tomato <- data.frame(weight =
                             c(1.5, 1.9, 1.3, 1.5, 2.4, 1.5, # water
                               1.5, 1.2, 1.2, 2.1, 2.9, 1.6, # nutrient
                               1.9, 1.6, 0.8, 1.15, 0.9, 1.6), # nutrient+24D
                     trt = rep(c("Water", "Nutrient", "Nutrient+24D"),
                               c(6, 6, 6)))

# Таблица со средними значениями:
Means <- data.frame(weight = as.numeric(tapply(tomato$weight,
                                               tomato$trt, mean)),
                    trt = rep("Means", 3))

# Добавляем таблицу Means к таблице tomato:
tomato <- rbind(tomato, Means)

# Изменяем базовый уровень фактора trt на Water:
tomato$trt <- relevel(tomato$trt, ref = "Water")

# Рис. 58:
# Рисуем исходную диаграмму (все точки из группы Means будут при этом
# автомтически залиты черным цветом):
stripchart(weight ~ trt, data = tomato, pch = 19,
           col = c("blue", "red", "black"),
           ylab = "Условия", xlab = "Вес (кг)")

# Добавляем поверх точек из группы Means точки нужного цвета
# (эти новые точки имеют координаты (средняя_1, 4), (средняя_2, 4)
# и (средняя_3, 4); их цвет прописываем "вручную"):
points(x = Means$weight, y = c(4, 4, 4), pch = 19,
       col = c("red", "black", "blue"))

# Рис. 59:
# Измененные данные
tomato2 <-
        data.frame(weight =
                           c(1.25, 1.9, 1.3, 1.5, 2.3, 1.45, # water
                             2.1, 2.2, 2.4, 2.9, 2.8, 3.1, # nutrient
                             0.9, 1.2, 0.8, 1.3, 0.7, 1.4), # nutrient+24D
                   trt = rep(c("Water", "Nutrient", "Nutrient+24D"),
                             c(6, 6, 6)))

# Таблица со средними значениями:
Means2 <- data.frame(weight = as.numeric(tapply(tomato2$weight,
                                                tomato2$trt, mean)),
                     trt = rep("Means", 3))

# Добавляем таблицу Means к таблице tomato:
tomato2 <- rbind(tomato2, Means2)

# Изменяем базовый уровень фактора trt на Water:
tomato2$trt <- relevel(tomato2$trt, ref = "Water")

# Рисуем исходную диаграмму (все точки из группы Means будут при этом
# автомтически залиты черным цветом):
stripchart(weight ~ trt, data = tomato2, pch = 19,
           col = c("blue", "red", "black"),
           ylab = "Условия", xlab = "Вес (кг)")

# Добавляем поверх точек из группы Means точки нужного цвета
# (эти новые точки имеют координаты (средняя_1, 4), (средняя_2, 4)
# и (средняя_3, 4); их цвет прописываем "вручную"):
points(x = Means2$weight, y = c(4, 4, 4), pch = 19,
       col = c("red", "black", "blue"))

# Рис. 60:
x = seq(0, 10, 0.1)
plot(x, df(x, 2, 15), type = "l")
abline(v = qf(0.95, 2, 15), lty = 2)

# Выполнение однофакторного дисперсионного анализа:
summary(aov(weight ~ trt, data = tomato))


# Двухфакторный дисперсионный анализ
library(HSAUR2)
data(weightgain)
str(weightgain)

library(ggplot2)
ggplot(data = weightgain, aes(x = type, y = weightgain)) + 
        geom_boxplot(aes(fill = source))

require(doBy)
summaryBy(weightgain ~ type + source, data = weightgain,
          FUN = c(mean, sd, length))

plot.design(weightgain) # рис. 62

# Рис. 63:
with(weightgain, interaction.plot(x.factor = type,
                                  trace.factor = source,
                                  response = weightgain))

# Двухфакторный дисперсионный анализ:
M1 <- aov(weightgain ~ source + type + source:type, 
          data = weightgain)
summary(M1)


# ------------------------ Раздел 5.6 ----------------------------------
# Оценка корреляции двух случайных величин:
dat <- read.delim("http://bit.ly/1wMGDOQ")
attach(dat)

# Параметрический коэффициент корреляции Пирсона:
cor.test(CAnumber, ZMlength)
cor.test(log(CAnumber+1), log(ZMlength))
shapiro.test(log(CAnumber+1))

# Непараметрические коэффициенты корреляции Спирмена и Кендалла:
cor.test(CAnumber, ZMlength, method = "spearman")
cor.test(log(CAnumber+1), log(ZMlength), method = "spearman")
cor.test(CAnumber, ZMlength, method = "kendall")

	
# --------------------------- Раздел 5.7 -------------------------------
# Критерий хи-квадрат:
qchisq(p = 0.95, df = 1)
mice <- matrix(c(13, 44, 25, 29), nrow = 2, byrow = TRUE)
mice # просмотр содержимого матрицы
chisq.test(mice) # тест хи-квадрат

light <- c(12, 40, 45)
dark <- c(87, 34, 75)
very.dark <- c(3, 8, 2)
color.data <- matrix(c(light, dark, very.dark), nrow = 3,
                     dimnames = list(c("Pop1", "Pop2", "Pop3"),
                                     c("Light", "Dark", "Very dark")))
color.data
chisq.test(color.data)

	
# -------------------------- Раздел 5.8 --------------------------------
# Точный тест Фишера:
(X <- matrix(c(1, 10, 8, 4), ncol = 2))
fisher.test(X)

# Критерий Мак-Немара:
data <- read.table(header = TRUE, text = '
 subject time result
       1  pre      0
       1 post      1
       2  pre      1
       2 post      1
       3  pre      0
       3 post      1
       4  pre      1
       4 post      0
       5  pre      1
       5 post      1
       6  pre      0
       6 post      1
       7  pre      0
       7 post      1
       8  pre      0
       8 post      1
       9  pre      0
       9 post      1
      10  pre      1
      10 post      1
      11  pre      0
      11 post      0
      12  pre      1
      12 post      1
      13  pre      0
      13 post      1
      14  pre      0
      14 post      0
      15  pre      0
      15 post      1
')

library(reshape2)

# Преобразуем данные в "широкий формат":
data.wide <- dcast(data, subject ~ time, value.var = "result")
data.wide
ct <- table( data.wide[, c("pre","post")])
ct
mcnemar.test(ct)
mcnemar.test(ct, correct = FALSE)
table(data[,c("time", "result")])

# Критерий Кохрана-Мантеля-Хензеля 
drug <-
  array(c(11, 10, 25, 27,
          16, 22, 4, 10,
          14, 7, 5, 12,
          2, 1, 14, 16,
          6, 0, 11, 12,
          1, 0, 10, 10,
          1, 1, 4, 8,
          4, 6, 2, 1),
        dim = c(2, 2, 8),
        dimnames = list(
                Group = c("Drug", "Control"),
                Response = c("Success", "Failure"),
                Center = c("1", "2", "3", "4", "5", "6", "7", "8")))
drug

mantelhaen.test(drug)
library(reshape) # для функции melt()

drug.df <- data.frame( melt(drug,
                            id=c("Center", "Group", "Response")))
# Рис. 64:
library(ggplot2)
p <- ggplot(data = drug.df, aes(x = Center, y = value, fill = Response)) +
        ylab("%")
p + geom_bar(stat = "identity", position = "fill") + 
        facet_grid(Group ~ .)

	
# --------------------------- Раздел 5.9 -------------------------------
# Мощность теста при сравнении долей:
votes <- matrix(c(28, 72, 20, 80), ncol = 2, byrow = T)
votes
res <- chisq.test(votes)
res
obs <- res$observed
exptd <- res$expected
obs <- obs/200 # здесь и ниже, 200 - общее число опрошенных
sqrt(sum((exptd - obs)^2/exptd))

library(pwr)
ES.w2(obs)

pwr.chisq.test(w = ES.w2(obs), df = 1, N = 200)
pwr.chisq.test(w = 0.15, N = NULL, df = 1, 
               sig.level = 0.05, power = 0.8)
               
# Оценка мощности теста методом имитаций               
prop.power <- function(n1, n2, p1, p2) {
        twobytwo=matrix(NA, nrow = 10000, ncol = 4)
        twobytwo[,1] = rbinom(n = 10000, size = n1, prob = p1)
        twobytwo[,2] = n1-twobytwo[,1]
        twobytwo[,3] = rbinom(n = 10000, size = n2, prob = p2)
        twobytwo[,4] = n1 - twobytwo[, 3]
        p = rep(NA, 10000)
        chisq.test.v = function(x) 
                as.numeric(chisq.test(matrix(x, ncol = 2), correct = FALSE)[3])
        p=apply(twobytwo, 1, chisq.test.v)
        power=sum(ifelse(p < 0.05, 1, 0))/10000
        return(power)
}

prop.power(100, 100, 0.28, 0.20)
