#########################################################################

#  Мастицкий С.Э., Шитиков В.К. (2014) Статистический анализ и визуализация 
#  данных с помощь R. (Адрес доступа: http://r-analytics.blogspot.com)

#########################################################################

#########################################################################
# Глава 6.	ЛИНЕЙНЫЕ  МОДЕЛИ  В  ДИСПЕРСИОННОМ АНАЛИЗЕ
#########################################################################

#-----------------------------------------------------------------------
#  К разделу 6.1.
#-----------------------------------------------------------------------	

# Загрузка таблицы данных, сформированных в разделе 4.4:
load(file="sleep_imp.Rdata")

# Раскрашенная корреляционная матрица:
M <- cor(sleep_imp3)
library(corrplot)
col4 <- colorRampPalette(c("#7F0000", "red", "#FF7F00", "yellow",
                           "#7FFF7F", "cyan", "#007FFF", "blue","#00007F")) 
corrplot(M, method = "color", col = col4(20), cl.length = 21,
         order = "AOE", addCoef.col = "green")

# Фактор инфляции дисперсии:
library(car)
vif(lm(Sleep ~ BodyWgt + BrainWgt + Span + Gest + Pred + Exp + Danger,
       data = sleep_imp3))

#  Матричные диаграммы рассеяния:
cars <- mtcars[, 1:7]
pairs(cars, panel = panel.smooth)

# Функция для оформления панелей с коэффициентом корреляции:
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...) {
        usr <- par("usr"); on.exit(par(usr))
        par(usr = c(0, 1, 0, 1))
        r <- abs(cor(x, y, method = "spearman"))
        txt <- format(c(r, 0.123456789), digits=digits)[1]
        txt <- paste(prefix, txt, sep = "")
        # эта команда позволяет изменять размер шрифта 
        # в соответствии со значением коэффициента корреляции:
        if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
        text(0.5, 0.5, txt, cex = cex.cor * r)
}

# Строим еще один вариант графика:
pairs(cars, , panel = panel.smooth, lower.panel = panel.cor)

# Строим еще один вариант графика:
library(lattice)
splom(cars)

# Строим еще один вариант графика:
library(GGally)
ggpairs(cars, 
        upper = list(continuous = "density", combo = "box"), 
        lower = list(continuous = "points", combo = "dot"))

# Эффективные степени свободы EDF:
library(mgcv)
summary(gam(mpg ~ s(hp) + s(qsec), data = cars))

#  Вывод категориальной диаграммы

# Загрузка исходных данных из файла:
Sparrows <- read.table(file = "SparrowsElphick.txt", header = TRUE)

# Отбираем необходимый комплект данных: воробьи с длиной крыла >= 65
I1 <- Sparrows$SpeciesCode == 1 & Sparrows$Sex != "0" & Sparrows$wingcrd < 65
Wing1 <- Sparrows$wingcrd[I1]
Wei1 <- Sparrows$wt[I1]
Mon1 <- factor(Sparrows$Month[I1])
Sex1<- factor(Sparrows$Sex[I1])

# Определим месяц и пол как категориальные переменные:
fMonth1 <- factor(Mon1,levels = c(5, 6, 7, 8, 9),
                  labels=c("May", "Jun", "Jul", "Aug", "Sep"))
fSex1 <- factor(Sex1, levels = c(4, 5),
                labels=c("Male","Female"))
coplot(Wei1 ~ Wing1 | fMonth1 * fSex1, ylab = "Weight (g)",
       xlab = "Wing length (mm)",
       panel = function(x, y, ...) {
               tmp <- lm(y ~ x, na.action = na.omit)
               abline(tmp)
               points(x, y) })
#  Удаляем данные за май и сентябрь:
df <-data.frame(weight = Wei1, length = Wing1, sex = fSex1, month = fMonth1) 
df1 <- df[df$month != "May" & df$month != "Sep", ]

#  Строим линейную модель и получаем дисперсионную таблицу:
M1 <- lm(weight ~ length*month*sex, data = df1)
DT <- anova(M1)

#  Выводим результаты расчетов в таблицы файлов Word:
library(stargazer)
stargazer(M1, type = "html", out = "M1.doc")
stargazer(DT, type = "html", out = "DT.doc", summary = FALSE)

#  Графики автокорреляционной функции (АКФ)

# Загрузка данных и определение оси времени:
Waders <- read.table(file = "wader.txt", header = TRUE)
Time <- seq(1, 25)

# Построение четырех графиков в одном окне:
par(mfrow = c(2, 2), mar = c(5, 4, 3, 2))
plot(Time, Waders$C.fuscicolis, type = "l", 
     xlab = "Время (2 недели)", ylab = "C. fuscicollis abundance")
acf(Waders$C.fuscicolis, main = "C. fuscicollis ACF")
plot(Time, Waders$L.dominicanus, type = "l", 
     xlab = "Время (2 недели)", ylab = "L. dominicanus abundance")
acf(Waders$L.dominicanus, main = "L. dominicanus ACF")
   
#-----------------------------------------------------------------------	
#   К разделу  6.2.
#-----------------------------------------------------------------------	

# Создадим таблицу с данными:
tomato <- data.frame(weight = 
                             c(1.5, 1.9, 1.3, 1.5, 2.4, 1.5, # water
                               1.5, 1.2, 1.2, 2.1, 2.9, 1.6, # nutrient
                               1.9, 1.6, 0.8, 1.15, 0.9, 1.6), # nutrient+24D
                     trt = rep(c("Water", "Nutrient", "Nutrient+24D"),
                               c(6, 6, 6)))

# Автоматически R выберет Nutrient в качестве базового уровня:
levels(tomato$trt)

# Изменим базовый уровень на Water:
tomato$trt <- relevel(tomato$trt, ref = "Water")
levels(tomato$trt)

# Сохраним модель в виде объекта с именем М:
M <- lm(weight ~ trt, data = tomato)

# Просмотрим результаты вычислений:
summary(M)

# Сравним групповые средние:
tapply(tomato$weight, tomato$trt, mean)

# Таблица дисперсионного анализа:
anova(M)

# Матрица дизайна:
model.matrix(M)

#-----------------------------------------------------------------------	
#  К разделам 6.3 - 6.5
#-----------------------------------------------------------------------	

#  Построение линейной модели:
M <- lm(count ~ spray, data = InsectSprays)
summary(M)
str(M)

#  Анализ предсказанных значений и остатков:
M.res <- M$residuals
M.fit <- M$fitted.values
plot(M1.fit, M1.res, pch = 19, col = 4, 
     xlab = "Предсказанные значения", ylab = "Остатки")
cor.test(fitted(M2), InsectSprays$count)
shapiro.test(resid(M))
library(car) 


# Рисунок на стр. 195:
set.seed(202)
dat = data.frame(Group = rep(c("A", "B", "C"), each = 1000),
                 Value = c(
                         rnorm(n=1000, mean=5, sd=1.2),
                         rnorm(n=1000, mean=7, sd=1.5),
                         rnorm(n=1000, mean=15, sd=2)
                 ))

library(ggplot2)
p1 = ggplot(dat, aes(x = Value, fill = Group)) + 
        geom_density(alpha = 0.6) +
        xlab("Значение") + ylab("Плотность вероятности")

p2 = ggplot(dat, aes(x = Value)) + 
        geom_density(alpha = 0.6, fill = "blue") +
        xlab("Значение") + ylab("Плотность вероятности")

# function for grid arrangement of plots:
multiplot <- function(..., plotlist=NULL, cols) {
        require(grid)
        
        # Make a list from the ... arguments and plotlist
        plots <- c(list(...), plotlist)
        
        numPlots = length(plots)
        
        # Make the panel
        plotCols = cols                          # Number of columns of plots
        plotRows = ceiling(numPlots/plotCols) # Number of rows needed, calculated from # of cols
        
        # Set up the page
        grid.newpage()
        pushViewport(viewport(layout = grid.layout(plotRows, plotCols)))
        vplayout <- function(x, y)
                viewport(layout.pos.row = x, layout.pos.col = y)
        
        # Make each plot, in the correct location
        for (i in 1:numPlots) {
                curRow = ceiling(i/plotCols)
                curCol = (i-1) %% plotCols + 1
                print(plots[[i]], vp = vplayout(curRow, curCol ))
        }
        
}

multiplot(p1, p2, cols = 2)

# Рис. на стр. 196:
ggplot(InsectSprays, aes(x = count)) + geom_histogram() +
        facet_wrap(~spray) +
        xlab("Число насекомых") + ylab("Частота")

# Рисунки на стр. 197:
ggplot(InsectSprays, aes(sample = count)) + stat_qq() +
        facet_wrap(~spray, scales = "free_y") +
        xlab("Ожидаемые квантили") + ylab("Наблюдаемые значения")

M <- lm(count ~ spray, data = InsectSprays)
InsectSprays$resids = resid(M)

p3 = ggplot(InsectSprays, aes(x = resids)) + 
        geom_histogram(aes(y=..density..)) +
        geom_density(color = "red") + 
        xlab("Остатки") + ylab("Плотность вероятности")

p4 = ggplot(InsectSprays, aes(sample = resids)) + stat_qq() +
        xlab("Ожидаемые квантили") + ylab("Наблюдаемые значения")

multiplot(p3, p4, cols = 2)

# Рис. на стр. 198:

set.seed(202)
ggplot(InsectSprays, aes(x = spray, y = count)) + geom_boxplot() +
        geom_jitter(alpha = 0.5) +
        xlab("Инсектицид") + ylab("Число выживших насекомых")

# Рис. на стр. 199:
InsectSprays$fit = fitted(M)
ggplot(InsectSprays, aes(x = fit, y = resids)) + geom_point() +
        xlab("Предсказанные значения") + ylab("Остатки")

# Тест Левене:
leveneTest(InsectSprays$count, InsectSprays$spray)

#  Построение линейной модели по трансформированным данным:
M.log <- lm(log(count + 1) ~ spray, data = InsectSprays)
shapiro.test(resid(M.log))
leveneTest(log(InsectSprays$count + 1), InsectSprays$spray)

#  Дисперсионный анализ по Краскелу-Уоллису:
kruskal.test(count ~ spray, data = InsectSprays)

#-----------------------------------------------------------------------	
#  К разделу 6.6.
#-----------------------------------------------------------------------	

# Двухфакторный дисперсионный анализ 
library(HSAUR2)
data(weightgain)

M2 <- lm(weightgain ~ type*source, data = weightgain)
summary(M2)
anova(M2)

M3 <- lm(weightgain ~ source*type, data = weightgain)
anova(M3)

#  Удаление первых 6 наблюдений и последних 7 наблюдений:
weightgain2 <- weightgain[-c(1:6, 34:40), ]

# Модели с разным порядком указания предикторов:
M4 <- lm(weightgain ~ type*source, data = weightgain2)
summary(M4)

M5 <- lm(weightgain ~ source*type, data = weightgain2)
summary(M5)

# ANOVA-таблица для модели М4:
anova(M4)

# ANOVA-таблица для модели М5:
anova(M5)

plot.design(weightgain2)

#-----------------------------------------------------------------------	
#  К разделу 6.7.
#-----------------------------------------------------------------------	

#  Контрасты комбинаций условий (treatment contrasts):
boxplot(count ~ spray, data = InsectSprays, col = "coral",
        xlab = "Инсектицид",
        ylab = "Число выживших насекомых")

#  Контрасты сумм (sum contrasts):
contrasts(InsectSprays$spray) <- contr.sum(n = 6)
contrasts(InsectSprays$spray)

M3 <- lm(count ~ spray, data = InsectSprays)
summary(M3)
with(InsectSprays, mean(tapply(count, spray, mean)))

# изменение контрастов для фактора spray на контрасты Хелмерта:
contrasts(InsectSprays$spray) <- contr.helmert(n = 6)
contrasts(InsectSprays$spray)
mat <- contrasts(InsectSprays$spray)
sum(mat[, 1]*mat[, 2])
M4 <- lm(count ~ spray, data = InsectSprays)
summary(M4)

# коэффициенты контраста для ответа на второй вопрос:
con1 <- c(1, 1, 1, -1, -1, -1)

# коэффициенты контраста для ответа на третий вопрос:
con2 <- c(1, 1, -1, -1, -1, 1)

# объединение обоих векторов в матрицу контрастов:
con.matrix <- cbind(con1, con2)
con.matrix
contrasts(InsectSprays$spray) <- con.matrix
summary(M5, split = list(spray = list("Первые три против остальных" = 1,
                                      "ABF против CDE" = 2)))

#-----------------------------------------------------------------------	
#  К разделу 6.8.
#-----------------------------------------------------------------------	

#  Поправка Бонферрони
# Скорректированные р-значения:
p.adjust(c(0.01, 0.02, 0.005), method = "bonferroni")

# Какие из проверяемых гипотез следует отвергнуть?
alpha <- 0.05
p.adjust(c(0.01, 0.02, 0.005), method = "bonferroni") < alpha

# Метод Холма
# Скорректированные р-значения:
p.adjust(c(0.01, 0.02, 0.005), method = "holm")
# (обратите внимание: функция автоматически упорядочивает
# итоговые р-значения по убыванию)

# Какие из проверяемых гипотез следует отвергнуть?
alpha <- 0.05
p.adjust(c(0.01, 0.02, 0.005), method = "holm") < alpha

pvals <- c(0.0001, 0.0004, 0.0019, 0.0095,  0.0201,
           0.0278, 0.0298, 0.0344, 0.0459, 0.3240,
           0.4262, 0.5719, 0.6528, 0.7590, 1.000)

#  Метод Беньямини-Хохберга:
p.adjust(pvals, method = "BH")

#  Метод Беньямини-Йекутили:
p.adjust(pvals, "BY")

#-----------------------------------------------------------------------	
#  К разделу 6.9.
#-----------------------------------------------------------------------	

# Критерий Тьюки:
waterbodies <- data.frame(Water = rep(c("Grayson", "Beaver",
                                        "Angler", "Appletree",
                                        "Rock"), each = 6),
                          Sr = c(28.2, 33.2, 36.4, 34.6, 29.1, 31.0,
                                 39.6, 40.8, 37.9, 37.1, 43.6, 42.4,
                                 46.3, 42.1, 43.5, 48.8, 43.7, 40.1,
                                 41.0, 44.1, 46.4, 40.2, 38.6, 36.3,
                                 56.3, 54.1, 59.4, 62.7, 60.0, 57.3) )

M <- aov(Sr ~ Water, data = waterbodies)
summary(M)
TukeyHSD(M)
par(mar = c(4.5, 8, 4.5, 4.5))
plot(TukeyHSD(M), las = 1)

M  <- lm(Sr ~ Water, data = waterbodies)
summary(M)
coef(M)
vcov(M)

library(multcomp)
glht(M, linfct = mcp(Water = "Tukey"))
glht(M, linfct = mcp(Water = c(
        "Rock - Angler = 0",
        "Grayson - Appletree = 0",
        "Grayson - Beaver = 0"))
)

contr <- rbind("Rock - Angler" = c(-1, 0, 0, 0, 1),
               "Grayson - Appletree" = c(0, -1, 0, 1, 0),
               "Grayson - Beaver" = c(0, 0, -1, 1, 0) )
contr
glht(M, linfct = mcp(Water = contr))
summary(glht(M, linfct = mcp(Water = "Tukey")))
mult <- glht(M, linfct = mcp(Water = contr))
confint(mult, level = 0.95)
plot(confint(mult, level = 0.95))
