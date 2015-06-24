
#  Мастицкий С.Э., Шитиков В.К. (2015) Статистический анализ и визуализация 
#  данных с помощь R. Купить книгу можно здесь: http://bit.ly/1QopZ0A


#########################################################################
#               Глава 3. БАЗОВЫЕ ГРАФИЧЕСКИЕ ВОЗМОЖНОСТИ R
#########################################################################


# -------------------------- Раздел 3.1 --------------------------------	

#  Варианты использования функции plot():
data(Indometh)
names(Indometh)
attach(Indometh)
plot(time, conc) # рис. 11
(means <- tapply(conc, time, mean))
names(means)
indo.times <- as.numeric(names(means))
plot(indo.times, means, type = "b")
plot(indo.times, means, xlab = "Время", ylab = "Концентрация") # рис. 12
plot(indo.times, means, xlab = "Время", ylab = "Концентрация", xlim = c(0, 15))
plot(indo.times, means, xlab = "Время", ylab = "Концентрация", ylim = c(0, 5))
plot(indo.times, means, xlab = "Время", ylab = "Концентрация",
     axes = TRUE, ann = TRUE)
plot(indo.times, means, xlab = "Время", ylab = "Концентрация",
     axes = FALSE, ann = TRUE)
plot(indo.times, means, xlab = "Время", ylab = "Концентрация",
     axes = TRUE, ann = FALSE)
plot(indo.times, means, xlab = "Время", ylab = "Концентрация", log = "x")
plot(indo.times, means, xlab = "Время", ylab = "Концентрация", log = "y")
plot(indo.times, means, xlab = "Время", ylab = "Концентрация", log = "xy")
plot(indo.times, means, xlab = "Время", ylab = "Концентрация",
     main = "Скорость выведения индометацина", type = "o")
plot(indo.times, means, xlab = "Время", ylab = "Концентрация",
     main = "Скорость выведения индометацина", type = "o", pch = 2)
plot(indo.times, means, xlab = "Время", ylab = "Концентрация",
     main = "Скорость выведения индометацина", type = "o", pch = 169, font = 5)
plot(indo.times, means, xlab = "Время", ylab = "Концентрация",
     main = "Скорость выведения индометацина", type = "o", pch = "A")

# Код для рис. 13:
pch = 1:25
par(mar = c(5, 5, 1, 1))
plot(c(0.8, 1:5), c(0.8, 1:5), type = "n", axes = F, ylab = "", xlab = "")
abline(v = 1:5, lty = 2, col = "gray90")
abline(h = 1:5, lty = 2, col = "gray90")
points(rep(1:5, 5), rep(1:5, each = 5), pch = pch, cex = 3)
text(x = (1:5)-0.25, y = rep(1:5, each = 5), labels = 1:25)

# символы в виде треугольников синего цвета (pch = 2, col = "blue"):
plot(indo.times, means, xlab = "Время", ylab = "Концентрация",
     main = "Скорость выведения индометацина", type = "o",
     pch = 2, cex = 1.2, col = "blue")

# символы в виде ромбиков (pch = 5) красного цвета (col = 2):
plot(indo.times, means, xlab = "Время", ylab = "Концентрация",
     main = "Скорость выведения индометацина", type = "o",
     pch = 5, cex = 1.2, col = 2)

# заголовок графика синего цвета (col.main = "blue"):
plot(indo.times, means, xlab = "Время", ylab = "Концентрация",
     main = "Скорость выведения индометацина", type = "o", 
     col.main = "blue", cex = 1.2)

# названия осей выполнены красным цветом (col.lab = "red")
plot(indo.times, means, xlab = "Время", ylab = "Концентрация",
     main = "Скорость выведения индометацина", type = "o", 
     col.main = "blue", col.lab = "red", cex = 1.2)

# Код для рис. 15:
plot(indo.times, means, xlab = "Время", ylab = "Концентрация",
     main = "Скорость выведения индометацина", type = "o",
     pch = 21, cex = 1.2, bg = "red", lwd=2, col.main = "blue")

# Изменение ширины линий:
plot(indo.times, means, xlab = "Время", ylab = "Концентрация",
     main = "lwd = 2", type = "l", lwd = 2)
plot(indo.times, means, xlab = "Время", ylab = "Концентрация",
     main = "lwd = 5", type = "l", lwd = 5)
plot(indo.times, means, xlab = "Время", ylab = "Концентрация",
     main = "lwd = 10", type = "l", lwd = 10)

# Код для рис. 17:
plot(indo.times, means, xlab = "Время", ylab = "Концентрация",
     main = "", type = "l", lwd = 10, lty = 2, col = 6, bty = "L")


# ----------------------------- Раздел 3.2 -------------------------------	

#  Гистограмма:
X <- rnorm(n = 100, mean = 15, sd  = 5)
hist(X) # рис. 18
hist(X, breaks = 20, col = "lightblue") # рис. 19
hist(X, breaks = 20, freq = FALSE)

# Код для рис. 20:
hist(X, breaks = 20, freq = FALSE, col = "lightblue")
plot(density(X))
plot(density(X, bw = 0.8))
hist(X, breaks = 20, freq = FALSE, col = "lightblue",
     xlab = "Переменная X",
     ylab = "Плотность вероятности",
     main = "Гистограмма, совмещенная с кривой плотности")
lines(density(X), col = "red", lwd = 2)
lines(density(X, bw = 0.8), col = "blue", lwd = 2)

#  Кривые ядерной плотности (рис. 21):
data(InsectSprays)
head(InsectSprays)
attach(InsectSprays)
# Сравнение всех групп по кривым ядерной плотности:
library(sm)
sm.density.compare(count, spray, lwd = 2, xlab = "Число насекомых")
title(main = "Кривые ядерной плотности")
# Составляем вектор с кодами использованных цветов
Colfill <- c(2:(2 + length(levels(spray))))
# добавляем легенду туда, куда кликнем мышью:
legend(locator(1), levels(spray), fill = Colfill)


#  Двухмерное распределение (рис. 22):
data(Indometh)
attach(Indometh)
library(MASS)
f <- kde2d(time, conc) 
image(f, xlab="Время выведения", ylab="Концентрация индометацина")
contour(f, add = TRUE)

#  Примеры использования функции cdplot() (рис. 23):
library(HSAUR2)
data(plasma)
summary(plasma)
layout(matrix(1:2, ncol = 2))
cdplot(ESR ~ fibrinogen, data = plasma)
cdplot(ESR ~ globulin, data = plasma)

cdplot(ESR ~ fibrinogen, col = c("coral", "skyblue"), data = plasma)
cdplot(ESR ~ globulin, col = c("coral", "skyblue"), data = plasma)

cdplot(ESR ~ fibrinogen, col = c("coral", "skyblue"),
       yaxlabels = c("< 20 mm/h", "> 20 mm/h"), data = plasma)
cdplot(ESR ~ globulin, col = c("coral", "skyblue"),
       yaxlabels = c("< 20 mm/h", "> 20 mm/h"), data = plasma)

# Рис. 24:
cdplot(ESR ~ fibrinogen, col = c("coral", "skyblue"),
       yaxlabels = c("< 20 mm/h", "> 20 mm/h"), bw = 0.9, data = plasma)
cdplot(ESR ~ globulin, col = c("coral", "skyblue"),
       yaxlabels = c("< 20 mm/h", "> 20 mm/h"), bw = 0.9, data = plasma)

	
# --------------------------- Раздел 3.3 --------------------------------
#  Примеры "ящиков с усами"
data(InsectSprays)
head(InsectSprays)
attach(InsectSprays)
boxplot(count ~ spray, data = InsectSprays)
# Рис. 26:
boxplot(count ~ spray,
        xlab = "Инсектициды",
        ylab = "Количество выживших насекомых",
        main = "Эффективность инсектицидов",
        col = "coral", data = InsectSprays)
# Горизонтальное расположение "ящиков":
boxplot(count ~ spray,
        ylab = "Инсектициды",
        xlab = "Количество выживших насекомых",
        main = "Эффективность инсектицидов",
        col = "coral", horizontal = TRUE,
        data = InsectSprays)

#  Пример двухмерного "мешка с усами" (рис. 27):
data(Indometh); attach(Indometh)
library(aplpack)
bagplot(time, conc, xlab = "Время выведения",
        ylab = "Концентрация индометацина", main = "Мешок с усами")


# -------------------------- Раздел 3.4 --------------------------------
# Данные по явке избирателей:
percent.voted <- c(60, 40)

# Распределение голосов:
votes <- c(49.3, 19.2, 13.2, 11.7, 3.4, 1.0, 0.6)
names(percent.voted) <- c("Проголосовали", "Не явились")
names(votes) <- c("Единая Россия", "КПРФ", "Справедливая Россия", 
                  "ЛДПР", "Яблоко", "Патриоты России", "Правое дело")
par(mfrow = c(1, 2))
pie(percent.voted, radius = 0.9, cex = 0.6, main = "Явка")
pie(votes, cex = 0.6, radius = 0.9, init.angle = -10,
    main = "Распределение голосов")

# Рис. 28:
pie(percent.voted, radius = 0.9, cex = 0.6, main = "Явка", 
    col = c("black", "gray80"))
pie(votes, cex = 0.6, radius = 0.9, init.angle = -10,
    main = "Распределение голосов",    col = c(2:8))

# Примеры столбчатых диаграмм:
data(InsectSprays)
InsectSprays
attach(InsectSprays)
Means <- tapply(count, spray, mean)
Means

barplot(height = Means) # или просто barplot(Means)
barplot(Means, col = "steelblue",
        xlab = "Инсектицид",
        ylab = "Количество выживших насекомых")
# Рис. 29:
barplot(Means, col = "steelblue",
        xlab = "Инсектицид",
        ylab = "Количество выживших насекомых",
        border = "red", width = sqrt(Means))
# Рис. 30:
barplot(Means, density = 20, col = "red", horiz = T, las = 1,
        ylab = "Инсектицид", 
        xlab = "Количество выживших инсекомых")
barplot(Means, density = 20, angle = -45, space = 2,
        col = "red", horiz = TRUE, las = 1,
        ylab = "Инсектицид", 
        xlab = "Количество выживших инсекомых")

# Столбчатые диаграммы для сгруппированных данных:
library(MASS)
data(genotype)
head(genotype)
means = with(genotype, tapply(Wt, list(Litter, Mother), mean))
means
# Рис. 31:
barplot(means, beside = TRUE,
       col = topo.colors(4),
       legend.text = rownames(means),
       xlab = "Выводок", ylab = "Вес, г",
       ylim = c(0, 100))
barplot(means, beside = FALSE,
        col = topo.colors(4),
        xlab = "Выводок", ylab = "Вес, г")

# Рис. 32:
sds = with(genotype, tapply(Wt, list(Litter, Mother), sd))
sds

b <- barplot(means, ylim = c(min(pretty(means-sds)),
             max(pretty(means+sds))),
             col = topo.colors(4),
             beside = TRUE, xpd = FALSE,
             ylab = "Вес, г", xlab = "Выводок",
             legend.text=rownames(means))
# Добавляем "усы" (подробнее см. ?arrows):
arrows(b, means+sds, b, means-sds, angle = 90, code = 3, length = 0.05)

	
# ------------------------- Раздел 3.5 ---------------------------------
# Точечные диаграммы Кливленда
# Рис. 33:
data(mtcars)
mtcars
dotchart(mtcars$mpg, labels = row.names(mtcars),
         main="Экономия топлива у 32 моделей автомобилей",
         xlab="Миль/галлон", cex = 0.8)

# Рис. 34:
x <- mtcars[order(mtcars$mpg), ]
x$cyl <- factor(x$cyl)
x$color[x$cyl==4] <- 1
x$color[x$cyl==6] <- 2
x$color[x$cyl==8] <- 3
dotchart(x$mpg, labels = row.names(x),
         groups = x$cyl, gcolor = "blue", pch = 16,
         main="Экономичность двигателя у 32 моделей автомобилей",
         xlab="Миль/галлон", cex = 0.8, color = x$color)

# Одномерные диаграммы рассеяния:
data(InsectSprays)
names(InsectSprays)
stripchart(InsectSprays$count ~ InsectSprays$spray,
           xlab = "Количество выживших насекомых",
           ylab = "Инсектицид",
           method = "stack")
# Рис. 35:
stripchart(InsectSprays$count ~ InsectSprays$spray,
           ylab = "Количество выживших насекомых",
           xlab = "Инсектицид",
           vertical = TRUE,
           method = "stack")
# Рис. 36:
stripchart(InsectSprays$count ~ InsectSprays$spray,
           ylab = "Количество выживших насекомых",
           xlab = "Инсектицид",
           vertical = TRUE,
           method = "jitter",
           jitter = 0.1,
           pch = 1, col = "blue")

means <- tapply(InsectSprays$count, InsectSprays$spray, FUN = mean)
means

(SDs <- tapply(InsectSprays$count, InsectSprays$spray, FUN = sd))

for(i in 0:6){
segments(x0 = 0.8+i, y0 = means[1+i],
         x1 = 1+i,  y1 = means[1+i], lwd = 3, lend = "square")
}

arrows(c(0.9, 1.9, 2.9, 3.9, 4.9, 5.9), means + SDs,
       c(0.9, 1.9, 2.9, 3.9, 4.9, 5.9), means-SDs,
       angle = 90, code = 3, length = 0.05, lwd = 1, 
       lend = "square")

# bpxplot() в связке с stripchart():
boxplot(count ~ spray, 
        # outline=FALSE отключает изображение точек-выбросов
        outline = FALSE, xlab = "Инсектициды",
        ylab = "Количество выживших насекомых",
        main = "Эффективность инсектицидов",
        data = InsectSprays)
stripchart(count ~ spray, method="stack", 
           data = InsectSprays, add = TRUE,
           pch = 1, col = "gray60", vertical = TRUE)

# Рис. 37:
boxplot(count ~ spray, data = InsectSprays, jitter = 0.2,
        outline = FALSE, horizontal = TRUE,
        ylab = "Инсектициды",
        xlab = "Количество выживших насекомых",
        main = "Эффективность инсектицидов")
stripchart(count ~ spray, method="stack", 
           data = InsectSprays, add = TRUE,
           pch = 1, col = "gray60")



# ------------------------- Раздел 3.6 ---------------------------------
# Категоризованные графики:
density <- read.delim(file = "http://bit.ly/1GozfvU", header = TRUE)
str(density)

coplot(Density ~ Depth | Month, data = density,
       xlab = c("Depth", "Month"), ylab = "Density")
coplot(Density ~ Depth | Month, data = density,
       pch = 19, col = "blue",
       xlab = c("Depth", "Month"), ylab = "Density")
coplot(Density ~ Depth | Month, data = density,
       bar.bg = c(fac = "coral"), # fac - сокращение от "factor"
       pch = 19, col = "blue",
       xlab = c("Depth", "Month"), ylab = "Density")

# Рис. 38:
coplot(Density ~ Depth | Month, data = density,
       panel = function(x, y, ...) {
               panel.smooth(x, y, lty = 2, pch = 19, col = "blue",
                            span = 0.6)}, 
       # span регулирует кривизну сглаживающей кривой
       bar.bg = c(fac = "coral"), # fac - сокращение от "factor"
       pch = 19, col = "blue",
       xlab = c("Depth", "Month"), ylab = "Density")

coplot(log(Density + 1) ~ Depth | Month, data = density,
       panel = function(x, y, ...) {
               panel.smooth(x, y, lty = 2, pch = 19, col = "blue",
                            span = 0.6)}, 
       # span регулирует кривизну сглаживающей кривой
       bar.bg = c(fac = "coral"), # fac - сокращение от "factor"
       pch = 19, col = "blue",
       xlab = c("Depth", "Month"), ylab = "Density")

# Рис. 39:
coplot(log(Density + 1) ~ Depth | Month * Transect,
       panel = function(x, y, ...) {
               panel.smooth(x, y, lty = 2, pch = 19, 
                            col = "blue", span = 0.6)},
       bar.bg = c(fac = "coral"), xlab = c("Depth", "Month"), 
       ylab = c("Density", "Transect"), data = density)

# Рис. 40:
coplot(log(Density+1) ~ Month | Depth,
       bar.bg = c(num = "coral"), # num - сокращение от numeric
       pch = 19, col = "blue", xlab = c("Month", "Depth"), 
       ylab = c("Density", "Transect"), data = density)

# Рис. 41:
coplot(log(Density+1) ~ Month | Depth,
       number = 2, overlap = 0.1,
       bar.bg = c(num = "coral"), pch = 19, col = "blue",
       xlab = c("Month", "Depth"), 
       ylab = c("Density", "Transect"), data = density)
