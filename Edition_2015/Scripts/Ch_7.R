
#  Мастицкий С.Э., Шитиков В.К. (2015) Статистический анализ и визуализация 
#  данных с помощь R. Купить книгу можно здесь: http://bit.ly/1QopZ0A


#################################################################################
#  Глава 7. РЕГРЕССИОННЫЕ МОДЕЛИ ЗАВИСИМОСТЕЙ МЕЖДУ КОЛИЧЕСТВЕННЫМИ ПЕРЕМЕННЫМИ
#################################################################################


# -------------------------- Раздел 7.1 --------------------------------
# Таблица с данными для построения модели:
y <- c(
        109.14, 117.55, 106.76, 115.26, 117.13, 125.39, 121.03,
        114.03, 124.83, 113.92, 122.04, 109.41, 131.61, 103.93,
        116.64, 117.06, 111.73, 120.41, 112.98, 101.20, 120.19,
        128.53, 120.14, 108.70, 130.77, 110.16, 129.07, 123.46,
        130.02, 130.31, 135.06, 129.17, 137.08, 107.62, 139.77,
        121.47, 130.95, 138.15, 114.31, 134.58, 135.86, 138.49,
        110.01, 127.80, 122.57, 136.99, 139.53, 127.34, 132.26,
        120.85, 124.99, 133.36, 142.46, 123.58, 145.05, 127.83,
        140.42, 149.64, 151.01, 135.69, 138.25, 127.24, 135.55,
        142.76, 146.67, 146.33, 137.00, 145.00, 143.98, 143.81,
        159.92, 160.97, 157.45, 145.68, 129.98, 137.45, 151.22,
        136.10, 150.60, 148.79, 167.93, 160.85, 146.28, 145.97,
        135.59, 156.62, 153.12, 165.96, 160.94, 168.87, 167.64,
        154.64, 152.46, 149.03, 159.56, 149.31, 153.56, 170.87,
        163.52, 150.97)

c(mean(y), sd(y)) # среднее значение и станд. отклонение

shapiro.test(y) # тест на нормальность распределения

# Рис. 84:
library(ggplot2)
ggplot(data = data.frame(y), aes(x = y)) + geom_histogram() + 
        ylab("Частота") + xlab("Давление, мм рт. ст.")


# Способы имитации данных

set.seed(101) # для воспроизводимости результата
y.new.1 <- rnorm(n = 100, mean = 135.16, sd = 16.96)

set.seed(101)
y.new.2 <- 135.16 + rnorm(n = 100, mean = 0, sd = 16.96)

# проверим, идентичны ли оба вектора?
all(y.new.1 == y.new.2)

# Оценка параметров линейной модели:
y.lm <- lm(y ~ 1) # формула для оценки только свободного члена
summary(y.lm)

# Генерация 5 повторностей линейной модели:
library(arm)
set.seed(102) # для воспроизводимости результата
y.sim <- sim(y.lm, 5)
# y.sim - объект класса S4, который содержит
# слоты coef (коэффициенты модели) и sigma 
# (станд. отклонения остатков модели):
str(y.sim)

# Извлекаем альтернативные реализации среднего из y.sim:
y.sim@coef

# Извлекаем альтернативные реализации ст.отклонений остатков:
y.sim@sigma

# Создание 1000 повторностей модели:
y.sim <- sim(y.lm, 1000)

# Инициализация пустой матрицы, в которой мы будем сохранять
# данные, сгенерированные на основе 1000 альтернативных 
# реализаций модели:
y.rep <- array(NA, c(1000, 100))

# Заполняем матрицу y.rep имитированными данными:
for(s in 1:1000){
        y.rep[s, ] <- rnorm(100, y.sim@coef[s], y.sim@sigma[s])
}

# гистограммы выборочных распределений первых 12 реализаций 
# нулевой модели (рис. 85):
par(mfrow = c(5, 4), mar = c(2, 2, 1, 1))
for(s in 1: 12){ hist(y.rep[s, ], xlab = "", ylab = "", breaks = 20, main = "")}

# Расчет ИКР для каждого из 1000 имитированных
# распределений значений кровяного давления:
test.IQR <- apply(y.rep, MARGIN = 1, FUN = IQR)

# гистограмма значений ИКР для 1000 имитаций (86):
hist(test.IQR, xlim = range(IQR(y), test.IQR),
     main = "ИКР", xlab = "", ylab = "Частота", breaks = 20)
lines(rep(IQR(y), 2), c(0, 100), col = "blue", lwd = 4)
 
# Добавляем один предиктор - возраст:
x <- rep(seq(16, 65, 1), each = 2)

# Объединяем значения возраста и давления крови в одну таблицу:
Data <- data.frame(Age = x, BP = y)
# Рис. 87:
ggplot(data = Data, aes(x = Age, BP)) + geom_point() + 
       geom_smooth(method = "lm", se = FALSE) +
       geom_rug(color = "gray70", sides = "tr") + 
       ylab("Частота") + xlab("Возраст, лет")

summary(lm(BP ~ Age, data = Data)) 

	
# -------------------------- Разделу 7.2 -------------------------------
# Создадим таблицу с данными и построим модель:
library(gamair)
data(hubble)
str(hubble)

M <- lm(y ~ x - 1, data = hubble)
summary(M)
anova(M)

(hub.const <- 76.581/3.09e19)
(age <- 1/hub.const)
age/(60^2*24*365)

# Оценка доверительных интервалов регрессии:
CPI.df <- cbind(predict(M,interval ="conf"), predict(M,interval ="pred"))  
CPI.df <- CPI.df[,-4] 
colnames(CPI.df) <- c("Y_fit","CI_l","CI_u","PI_l","PI_u")
head(CPI.df)
# Рис. 88:
matplot(hubble$x, CPI.df, type = "l", 
        lwd = c(2, 1, 1, 1, 1), col = c(1, 2, 2, 4, 4),
        ylab = "Скорость,км/с",xlab="Расстояние,Мпс")
with(hubble, matpoints(x, y, pch = 20))

# Оценка доверительных интервалов параметра:
beta <- summary(M)$coefficients[1]
SE <- summary(M)$coefficients[2]
ci.lower <- beta - qt(0.975, df = 23)*SE
ci.upper <- beta + qt(0.975, df = 23)*SE
c(ci.lower, ci.upper)
Uni.upper <- 1/(ci.lower*60^2*24*365.25/3.09e19)
Uni.lower <- 1/(ci.upper*60^2*24*365.25/3.09e19)
c(Uni.lower, Uni.upper)

# Рис. 89:
library(gamair)
data(hubble)

boots = vector(mode="list", length=6)

for(i in 1:6){
        boots[[i]] = hubble[sample(1:24, 24, replace = TRUE), 2:3]  
}

boots = do.call(rbind.data.frame, boots)
boots$reps = rep(c("A", "B", "C", "D", "E", "F"), each = 24)

ggplot(boots, aes(x, y)) + geom_point() + facet_wrap(~reps)


# Оценка доверительных интервалов параметра бутстрепом:
regr <- function(data, indices) {
# вектор indices будет формироваться функцией boot() 
        dat <- data[indices, ] 
        fit <- lm(y ~ -1 + x, data = dat)
        return(summary(fit)$coefficients[1])
}

library(boot)
results <- boot(data = hubble, statistic = regr, R = 1000)
plot(results) # Рис. 90
quantile(results$t, c(0.025, 0.975))
U.lower <- 1/(85.73249*60^2*24*365.25/3.09e19)
U.upper <- 1/(67.07360*60^2*24*365.25/3.09e19)
U.lower
U.upper
boot.ci(results, type = "bca")

# Оценка доверительных интервалов параметра методом имитаций:
library(arm)
simulations <- sim(M, 1000)
hist(simulations@coef, breaks = 30)
sd(simulations@coef)
quantile(simulations@coef, c(0.025, 0.975))

# Рис. 91:
M <- lm(y ~ x - 1, data = hubble)
hubble$fit = fitted(M)

p1 = ggplot(hubble, aes(x, y)) + geom_point() +
        geom_hline(aes(yintercept=mean(hubble$y)), color = "blue") +
        geom_segment(aes(x = x, y = y, xend = x, yend = mean(hubble$y))) +
        ggtitle("TSS")

p2 = ggplot(hubble, aes(x, y)) + geom_point() +
        geom_smooth(method = "lm", se = FALSE, color = "blue") +
        geom_segment(aes(x = x, y = y, xend = x, yend = fit)) +
        ggtitle("RSS")

library(gridExtra)
grid.arrange(p1, p2, ncol = 2)


# --------------------------- Раздел 7.3 -------------------------------
# Рис. 92:
par(mfrow = c(3, 3))
set.seed(101)
for (i in 1:9) plot(1:50, rnorm(50),
                    xlab = "Предсказанные значения",
                    ylab = "Остатки")

# Рис. 93:
par(mfrow = c(3, 3), mar = c(4.5, 4.4, 2, 2))
set.seed(101) # для воспроизводимости кода
for (i in 1:9) plot(1:50, (1:50)*rnorm(50),
                    xlab = "Предсказанные значения",
                    ylab = "Остатки")

# Рис. 94: 
par(mfrow = c(3, 3), mar = c(4.5, 4.4, 2, 2))
set.seed(101) # для воспроизводимости кода
for (i in 1:9) plot(1:50, sqrt((1:50))*rnorm(50),
                    xlab = "Предсказанные значения",
                    ylab = "Остатки")

# Рис. 95:
x = 1:50
set.seed(200)
y = rnorm(50, 0.1 + 0.2*x + 0.035*(x^2), 10)
plot(x, y)

# Простая модель для приведенных выше переменных X и Y:
M1 <- lm(y ~ x)
summary(M1)

# Рис. 96:
plot(x, y)
abline(coef(M1)[1], coef(M1)[2], col = "blue")

# Рис. 97:
plot(fi tted(M1), resid(M1),
     xlab = "Предсказанные значения",
     ylab = "Остатки")

# Полином 2-й степени:
M2 <- lm(y ~ x + I(x^2))
summary(M2)

# Рис. 98:
plot(fi tted(M2), resid(M2),
     xlab = "Предсказанные значения",
     ylab = "Остатки")

# Рис. 99:
par(mfrow = c(2, 2))
plot(M2)

# Рис. 100:
set.seed(604)
df = data.frame(x = rnorm(50, 10, 1.5), y = rnorm(50, 2, 0.1))

p = ggplot(data = df, aes(x, y))

p + stat_density2d(aes(fill = ..level..), geom="polygon") + 
        xlim(5.4, 14.8) + ylim(1.7, 2.2) +
        scale_fill_continuous(low = "red", high = "white", 
                              name = "Плотность\nвероятности") +
        geom_point(size = 3, col = "black") +
        geom_point(data = df[44, ], aes(x, y), color = "yellow", size = 4) +
        theme(legend.position = "top") + xlab('x1') + ylab('x2')

# Свойства матрицы проекции:
set.seed(604)
df = data.frame(x1 = rnorm(50, 10, 1.5),
                x2 = rnorm(50, 2, 0.1))
head(df)

# Добавляем зависимую переменную в df:
set.seed(101)
df$y = 2*df$x1 - 25*df$x2 + rnorm(50, 0, 2)

# Модель для y, x1 и x2:
M <- lm(y ~ x1 + x2, data = df)
summary(M)

# Матрица модели:
X = model.matrix(y ~ x1 + x2, data = df)
head(X)

# Расчет матрицы H:
H <- X %*% solve( t(X) %*% X ) %*% t(X)

# Предсказываемые значения, полученные на основе H:
Yhat <- H %*% df$y

# Расчет диагональных элементов матрицы:
hat(X) # Способ 1
diag(H) # Способ 2
Minf <- infl uence(M) # Способ 3
Minf$hat

# Cумма диагональных элементов матрицы проекции равна 
# числу коэффициентов регрессионного уравнения:
sum(hat(X))

# Рис. 101:
plot(hat(X), type = "h")
abline(h = 2*3/50, lty = 2, col = "blue")

# Рис. 102:
r <- residuals(M)/(summary(M)$sig*sqrt(1 - hat(X)))
plot(fi tted(M), r, ylim = c(-3, 3))
abline(h = 0, lty = 2)
abline(h = c(-2, 2), lty = 2, col = "blue")

# Проверка значимости "выборосов":
max(abs(rstudent(M)))
abs(qt(.05/(50*2), 50-3-1)) # с поправкой Бонферрони

# Рис. 103:
par(mfrow = c(1, 3))
plot(dfbetas(M)[, 1], ylab = "Изменение свободного члена",
     xlab = "Порядковый номер")
plot(dfbetas(M)[, 2], ylab = "Изменение коэффициента x1",
     xlab = "Порядковый номер")
plot(dfbetas(M)[, 3], ylab = "Изменение коэффициента x2",
     xlab = "Порядковый номер")

# Расстояния Кука:
head(cooks.distance(M), n = 3)

# Ковариационное отношение:
head(covratio(M), n = 3)

# Показатели влиятельности:
influence.measures(M)


# --------------------------- Раздел 7.4 -------------------------------
# Создадим таблицу с данными:
# Определяем исходные данные:  NDO - доля Diamesinae+Orthocladiinae
# T  -  температура воды при взятии гидробиологической пробы

NDO <- c(88.9,94.9,70.8,46.4,31.0,66.5,83.6,71.9,
         59.6,22.5,29.2,6.5,17.5)
T <- c(14.8,14.5,11.5,12.6,12,14.5,13.3,16.7,16.9,20.6,21.2,22.5,22.4)

Ex1 <- lm(NDO ~ T); Ex0 <-lm(NDO ~ 1); summary(Ex1)
anova(Ex1,  Ex0)

#  Модели II типа
library(lmodel2)

# Для RMA-модели вводим нормировку осей. Задаем 999 перестановок:
Ex2.res <- lmodel2(NDO ~ T, range.y = "relative",
                   range.x = "relative", nperm = 999)
Ex2.res

# Рис. 105:
op <- par(mfrow = c(1,2))
plot(Ex2.res, method = "OLS", conf = FALSE, centroid = TRUE, main = "",
     xlab = "Температура, град", ylab = "Доля диамезин", 
     col=1,pch = 22, bg = "blue") 
lines(Ex2.res, "SMA", col = 2, conf = FALSE) ; 
lines(Ex2.res, "RMA", col = 3, conf = FALSE)
lines(Ex2.res,  "MA", col = 4, conf = FALSE)   ; 
legend("topright", c("OLS", "SMA", "RMA", "MA"), col=1:4, lty=1)
plot(Ex2.res, "SMA", lwd=2)

# Робастная регрессия Хубера:
library(MASS)
summary(rlm(NDO ~ T))  

# Метод урезанных квадратов LTS
library(robustbase) 
summary(lmrob(NDO ~ T))  

# Расчеты характеристик регрессии Кендалла-Тейла:
source("Kendall_Theil_Regr.r") # Скрипт доступен в GitHub-репозитории книги
df1 <- data.frame(T, NDO)
Kendall(df1)

	
# -------------------------- Раздел 7.6 --------------------------------
# Полиномиальная регрессия

# Создадим таблицу с данными:
Y <- c(0.155, 0.15, 0.165, 0.115, 0.11, 0.16, 0.17, 0.355,
       0.43, 0.305, 0.315, 0.46, 0.545, 0.47, 0.45, 0.51, 0.525)
X <- c(1, 9, 11, 17, 18.5, 25, 36, 40.5, 42, 44, 45, 51, 54,
       55.5, 57, 61, 67)
lm.3 <- lm(Y ~ poly(X, 3, raw = TRUE))
summary(lm.3)

# Создаем список из объектов всех четырех моделей:
allModel <- lapply(1:4,function(k) lm(Y ~ poly(X, k, raw = TRUE)))
extract <- function(fit) {
        sigma <- summary(fit)$sigma  # среднеквадратичная ошибка
        R2.adj <- summary(fit)$adj.r.squared  # скорректиров. R2 
        aic <- AIC(fit)           #  АIC-критерий
        out <- data.frame(sigma = sigma, R2.adj = R2.adj, AIC = aic)
        return(out)    }
result <- lapply(allModel, extract) # Список результатов

# Преобразуем список результатов в таблицу:
result <- as.data.frame(matrix(unlist(result), nrow = 4, byrow = T))
colnames(result) <- c("M.sigma", "M.R2adj", "M.AIC")

# Вычисление среднеквадратичной ошибки кросс-проверки
M.ErCV <- sapply(1:4,function(k) {
        n <- length(X) ; Err_S  <- 0 
        for(i in 1:n)    { Xcv <- X[-i]
                           lm.temp <- lm( Y[-i] ~ poly(Xcv, k , raw = TRUE))
                           YR <- predict(lm.temp, newdata=data.frame(Xcv=X))[i]
                           Err_S <- Err_S + (Y[i] -  YR)^2  }  
        sqrt(Err_S/n) })
cbind(result, M.ErCV)

# Рис. 106:
plot(X, Y, type = "p", pch = 22, bg = "yellow", 
     xlab = "Расстояние", ylab = "Частота аллеля")
sapply(1:4, function(k) points(X,
                               predict(lm(Y ~ poly(X, k, raw = TRUE))),
                               type = "l", col = k, lwd = 2))
legend("topleft", c("k = 1", "k = 2","k = 3","k = 4"),
        col = 1:4, lwd = 2)


# Нелинейная логистическая модель:

# Выполним подгонку параметров модели для вида Tanytarsus kharaensis:
S1 <- c(3, 7.5,12.5,17.5,22.5,27.5)  
p1 <- c(50,23, 45 , 25, 2,  9)
log.ss1 <- nls(p1 ~ SSlogis(S1, phi1, phi2, phi3))
summary(log.ss1)

# Доверительные интервалы нелинейной модели:
Rsquared <- 1 - var(residuals(log.ss1))/var(p1)

# Рассчитанные по модели значения отклика:
x <- 0:max(S1)
pr1 = predict(log.ss1, data.frame(S1 = x))

## заметим, что независимая переменная в SSlogis имеет имя "S1", 
## и поэтому мы изменяем имя столбца в таблице предикторных значений
### вычисляем ошибку регрессии линейной аппроксимацией:
se.fit <- sqrt(apply(attr(pr1,"gradient"), 1, 
                     function(mat) sum(vcov(log.ss1)*outer(mat, mat))))
PCI <- pr1 + outer(se.fit, qnorm(c(.5, .025, .975)))

# Критические точки отклика:
a <- coef(log.ss1)[1]*c(0.05, 0.5, 0.95)

# Критические точки солености:
plx<-approx(pr1, x, xout = a)$y

# Рис. 107:
matplot(x, PCI,type="l", xlab = "Минерализация, г/л",
        ylab = "Доля встречаемости в пробе,%",
        lty = c(1,3,3), lwd = c(2,1,1), ylim = c(0,60))
matpoints(S1, p1, pch=20)
text(5, 5, bquote(R^2==.(round(Rsquared, 3))))
sapply(1:3, function(i)
lines (c(0, plx[i], plx[i]), c(a[i], a[i], 0), lty = 2))


#  Степенная модель нелинейной регрессии

# потребление кислорода (Q) в зависимости от массы тела 
# пескожила Arenicola marina:
W <- c(2.37,2.82,3.12,3.21,3.22,3.24,3.38,3.6,4,4.65,4.91,4.93,
       5.72,5.95,7.65,8.26,8.5,10.62,14.6,18.76)
Q <- c(105.33,192.63,177.13,221.84,95.9,107,202.5,105.4,182.38,
       198.2,167.45,195,108.8,233.13,190.17,254.35,191.88,
       416.87,472.75,538.34)
m.ppo <- nls(Q ~ a*W^k, start = list(a = 55, k = 0.8))
summary(m.ppo)

m.lpo <- lm(log(Q) ~ I(log(W)))
summary(m.lpo)
exp(coef(m.lpo)[1])

# Рис. 108:
plot(W, Q, type="p", pch = 22, bg  ="yellow", 
     xlab = "Масса тела, г", ylab = "Потребление кислорода")
lines (W, predict(m.ppo), lwd=2)
lines (W, exp(predict(m.lpo)), col=6, lwd=2)
legend ("topleft", c("Нелинейная", "Линеаризация"), 
        col = c(1,6), lwd = 2)


# --------------------------- Раздел 7.7 -------------------------------
# Загрузим таблицу с ранее сохраненными данными:
load(file = "sleep_imp.Rdata")
M <- lm(Sleep ~ BodyWgt + BrainWgt + Span + Gest + Pred + Exp + Danger,
        data = sleep_imp3)
summary(M)
AIC(M)

Mstep <- step(M, direction = "both")
summary(Mstep)
AIC(Mstep)

library(packfor)
forward.sel(sleep_imp3$Sleep, sleep_imp3[,-1], alpha = 0.2) 

# Функция построения "всех возможных моделей"
all.possible.regressions <- function(dat, k){
        n <- nrow(dat)
        regressors <- paste("x", 1:k, sep="")
        lst <- rep(list(c(T, F)), k)
        regMat <- expand.grid(lst);
        names(regMat) <- regressors
        formular <- apply(regMat, 1, function(x)
                as.character(paste(c("y ~ 1", regressors[x]), collapse="+")))
        allModelsList <- apply(regMat, 1, function(x)
                as.formula(paste(c("y ~ 1", regressors[x]), collapse=" + ")) )
        allModelsResults <- lapply(allModelsList,
                                   function(x, data) lm(x, data = data),
                                   data = dat)
        n.models <- length(allModelsResults)
        extract <- function(fit) {
                df.sse <- fit$df.residual
                p <- n - df.sse -1
                sigma <- summary(fit)$sigma
                MSE <- sigma^2
                R2 <- summary(fit)$r.squared
                R2.adj <- summary(fit)$adj.r.squared
                aic <- AIC(fit)
                sse <- MSE*df.sse
                bic <- n*log(sse) + log(n)*(p+2)
                out <- data.frame(df.sse = df.sse, p = p, SSE = sse, MSE = MSE,
                                  R2 = R2, R2.adj = R2.adj, AIC = aic, BIC = bic)
                return(out)
        }
        result <- lapply(allModelsResults, extract)
        result <- as.data.frame(matrix(unlist(result), nrow=n.models, byrow=T))
        result <- cbind(formular, result)
        rownames(result) <- NULL
        colnames(result) <- c("model", "df", "p", "SSE", "MSE", "R2",
                              "R2.adj", "AIC", "BIC")
        return(result)
}

#  Перестраиваем таблицу и переименовываем столбцы с исходными переменными:
ind.yx <- c(5, 1:2,6:10); 
names.xy <- c("y", paste("x", 1:7, sep=""))
yx <- sleep_imp3[, ind.yx]
data.frame(Old = colnames(yx), New = names.xy, Index = ind.yx)
colnames(yx) <- names.xy

# Получаем список из 128 всех возможных моделей:
all.mod <- all.possible.regressions(dat = yx, k = 7)
write.table(all.mod, "clipboard", sep = "\t")  # Вывод в буфер обмена
head(all.mod[order(all.mod$AIC), ], 10)

# Перебор всех возможных моделей при помощи regsubsets():
library(leaps)
leaps <- regsubsets(Sleep ~ BodyWgt + BrainWgt + Span + Gest +
                            Pred + Exp + Danger, data <- sleep_imp3, nbest = 7)
summary(leaps)
plot(leaps, scale = "adjr2") # рис. 109

#  Алгоритм "Forward selection" с использованием кросс-проверки:
library(FWDselect)
qob <- qselection(x = sleep_imp3[, -1], y = sleep_imp3[, 1],
                  qvector <- c(1:7), method = "lm", criterion = "variance")
plot(qob) # График зависимости внешнего критерия от сложности модели (рис. 110) 
selection(x = sleep_imp3[, -1], y = sleep_imp3[, 1], q = 5,
          criterion = "variance", method = "lm", family = "gaussian")


# --------------------------- Раздел 7.8 -------------------------------
# Сравнение моделей по дисперсии остатков:
anova(M, Mstep)
library(car)
Anova(M, Mstep)

# Сравнение по ошибке кросс-проверки:
library(DAAG)
(cvObjM <- cv.lm(df = sleep_imp3, M, m = 3, seed = 0)) 
(cvObjMstep <- cv.lm(df = sleep_imp3, Mstep, m = 3, seed = 0)) 

# Диагности допущений в отношении остатков
# Рис. 111:
par(mfrow = c(1, 2)) 
plot(Mstep, which = c(1, 2))

# Рис. 112:
library(sm)
sm.density(rstudent(Mstep), model = "normal")
shapiro.test(resid(M))

# Тест на наличие выбросов:
outlierTest(Mstep, cutoff = 0.05)

# Рис. 113:
par(mfrow = c(1, 2))
leveragePlot(Mstep, "Span")
leveragePlot(Mstep, "Danger")

#  Тест на автокорреляцию:
durbinWatsonTest(Mstep)


#  Доверительные интервалы бутстрепом (рис. 114):
library(boot)

# функция, возвращающая вектор коэффициентов
bootF <- function(data, indices){
        data <- data[indices,] 
        mod <- lm(Sleep ~ Span + Pred + Danger, data=data)
        coefficients(mod)
}

Sleep.boot <- boot(sleep_imp3, bootF, 1000) 

library(car)
dataEllipse(Sleep.boot$t[, 2], Sleep.boot$t[, 3],
            xlab = "Span", ylab = "Pred",
            cex = 0.3, levels = c(0.9, 0.95, 0.99), robust = T)

# Доверительный интервал для Span:
boot.ci(Sleep.boot, index = 2,  type =  "bca")

# Рис. 115:
library(car)
scatterplotMatrix(~Sleep + BodyWgt + BrainWgt + Span + 
                          Pred + Danger, data = sleep_imp3, diag = "boxplot")

# Таблица с трансформированными предикторами:
df_nl <- data.frame(ln_BodyWgt = log(sleep_imp3$BodyWgt),
                    ln_BrainWgt = log(sleep_imp3$BrainWgt),
                    Brain.Body = sleep_imp3$BrainWgt/sleep_imp3$BodyWgt,
                    rev_Span = 1/sleep_imp3$Span)
ind.yx <- c(5, 1:2, 6:10)
sleep_nl <- cbind(sleep_imp3[,ind.yx], df_nl)

M.nl1 <- lm(Sleep ~ ln_BodyWgt + BrainWgt + Span + Gest + Pred + Exp + Danger,
            data = sleep_nl)
(cvObjM.nl1 <- cv.lm(df = sleep_nl, M.nl1, m = 3, seed = 0)) 



# -------------------------- Раздел 7.9 -------------------------------
# Гребневая регрессия:
library(MASS)
M.ridge <- lm.ridge(Sleep ~ ., data = sleep_nl, lambda = seq(0,2,0.1))

plot(x = M.ridge$lambda, y = M.ridge$GCV, type = "o") # рис. 116

lambda <- M.ridge$GCV[which.min(M.ridge$GCV)]
(M.ridge1 <- lm.ridge(Sleep ~ ., data = sleep_nl, lambda = lambda))

# Коэффициенты модели
beta.M.ridge1 <- coef(M.ridge1)
m <- length(beta.M.ridge1)

# Остатки модели
resid.ridge <- sleep_nl$Sleep - beta.M.ridge1[1] - 
        as.matrix(sleep_nl[,2:m])%*%beta.M.ridge1[2:m]

# Найдем число степеней свободы гребневой регрессии:
d <- svd(as.matrix(sleep_nl[, 2:m]))$d
df <- nrow(sleep_nl) - sum(d^2/(lambda+d^2))

# Средний квадрат отклонений:
rss.ridge <- sum(resid.ridge^2)/df


# Лассо-регрессия:
library(lars)
Xmat <- as.matrix(sleep_nl[, -1])
M.las <- lars(Xmat, sleep_nl[, 1], type = "lasso")
# Рис. 117:
plot(M.las, plottype = "coefficients"); plot(M.las, plottype = "Cp")

# Параметр фракционирования S найдем перекрестной проверкой:
set.seed(0); r <- cv.lars(Xmat, sleep_nl[, 1]) 
(bestfrac <- r$index[which.min(r$cv)])

las.coef <- predict(M.las, Xmat, s = bestfrac, 
                    type = "coefficient", mode = "fraction")
las.coef  # Получение коэффициентов модели

# Остатки модели:
las.resid <- sleep_nl$Sleep - predict.lars(M.las, Xmat, s = bestfrac,
                                          type ="fit", mode = "fraction")$fit
rss.lasso <- sum(las.resid^2)/(nrow(sleep_nl) - 7)

# Регрессия на главные компоненты:
load(file = "sleep_imp.Rdata")
sleep_imp <- as.data.frame(scale(sleep_imp3))
Sleep.pca <- princomp(~ BodyWgt + BrainWgt + Span + Gest + 
                              Pred + Exp + Danger, data = sleep_imp)
summary(Sleep.pca)
loadings(Sleep.pca)[, 1:3]

# Оценка необходимого числа главных компонент
ev <- Sleep.pca$sdev 
ev[ev > mean(ev)]  # Критерий Кайзера-Гуттмана

# Столбиковая диаграмма собственных значений
barplot(ev, main = "Собственные значения", col = "bisque", las = 2)
abline(h = mean(ev), col = "red") 
legend("topright", "Средние собственные значения", lwd = 1)
       
T <- predict(Sleep.pca)[, 1:3]
sleep_PCA <- as.data.frame(cbind(Sleep = sleep_imp3[, 5], T))
M.PCA <- lm(Sleep ~ ., data = sleep_PCA)
summary(M.PCA)
AIC(M.PCA)

# Метод частных наименьших квадратов (Partial Least Squares):
library(pls)
M.pls <- plsr(Sleep ~ ., 4, data = sleep_imp3, method = "oscorespls")
summary(M.pls)
(beta.pls <- drop(coef(M.pls)))
resid.pls <- drop(M.pls$resid)[, 4]
rss.pls <- sum(resid.pls^2)/(nrow(sleep_imp3) - 5)
rss.pls

M.pls <- plsr(lpsa ~ .,3, data = train, method = "oscorespls")
summary(model.pls)

# Сравнение критерия RSS для оценки качества подгонки семи моделей:
c(rss.M = rss.ls <- sum(M$resid^2)/M$df.residual,
  rss.Mstep = rss.ls <- sum(Mstep$resid^2)/Mstep$df.residual,
  rss.M.nl1 = rss.ls <- sum(M.nl1$resid^2)/M.nl1$df.residual,
  rss.ridge = rss.ridge,
  rss.lasso = rss.lasso,
  rss.M.PCA = rss.ls <- sum(M.PCA$resid^2)/M.PCA$df.residual,
  rss.pls = rss.pls)



# --------------------------- Раздел 7.9 -------------------------------
# Установим начальный код запуска генератора случайных чисел,
# чтобы получать одни и те же данные:
set.seed(632)

# Создадим таблицу с данными обучающей выборки:
n = 50  #  Объем генерируемой выборки
x1 <- runif(n, 0, 1)
x2 <- x1 + rnorm(n, 0, 0.25)
x3 <- (x1 + x2)/2 + runif(n, 0, 0.1)
x4 <- runif(n, 0, 1)
x5 <- (2*x4 + rnorm(n, 0, 0.25))/2 + runif(n, 0, 0.1)
x6 <- runif(n, 0, 1)
y <- (3 + x1 + x2 + 0.5*x3 + 0.75*x4 + 0.5*x5 + 0.5*x6 + rnorm(n, 0, 1))

# Для последующих вычислений найдем средние и дисперсии:
x <- cbind(x1, x2, x3, x4, x5, x6)
mx <- apply(x, 2, mean)
varx <- apply(x, 2, var)

# Стандартизация всех Х-ов:
x <- scale(x)
trdata <- data.frame(cbind(y, x))
print(trdata, digits = 4)

# Создадим таблицу с данными экзаменационной выборки:
gx1 <- runif(5000, 0, 1)
gx2 <- gx1 + rnorm(5000, 0, 0.25)
gx3 <- (gx1 + gx2)/2 + runif(5000, 0, 0.1)
gx4 <- runif(5000, 0, 1)
gx5 <- (2*gx4 + rnorm(5000, 0, 0.25))/2 + runif(5000, 0, 0.1)
gx6 <- runif(5000, 0, 1)
gy <- (3 + gx1 + gx2 + 0.5*gx3 + 0.75*gx4 + 0.5*gx5 + 0.5*gx6 + rnorm(5000, 0, 1))
gx <- cbind(gx1, gx2, gx3, gx4, gx5, gx6)
for (i in 1:6) gx[,i] <- (gx[,i]-mx[i])/sqrt(varx[i])
gendata <- data.frame( cbind(gy, gx) )
names(gendata) <- c("y", "x1", "x2", "x3", "x4", "x5", "x6")
summary(gendata)

attach(trdata) 
library(Hmisc)

# Функция расчета корреляционной матрицы с р-значениями:
corsig <- function(x){  
        x <- as.matrix(x) ; R <- rcorr(x)$r ; p <- rcorr(x)$P
        ipa <- lower.tri(p, diag = FALSE) ; R[ipa] <- p[ipa] 
        return (R) 
}
round(corsig(trdata),4) 

#  Общая линейная модель и ее проверка на экзаменационной выборке:
ols1 <- lm(y ~ x1 + x2 + x3 + x4 + x5 + x6)
summary(ols1)
ols1.mspe <- mean( (gy - predict(ols1, newdata=gendata))^2 ) 

#  Выбор информативного комплекса предикторов:
ols2 <- step(ols1, direction = "both")
summary(ols2)
ols2.mspe <- mean( (gy - predict(ols2, newdata=gendata))^2 )

library(packfor)
forward.sel(trdata$y, trdata[, -1]) 
ols3 <- lm(y ~ x1 + x4, data = trdata)
ols3.mspe <- mean( (gy - predict(ols3, newdata = gendata))^2 ) 

library(FWDselect) 
qob = qselection(x = trdata[, -1], y = trdata$y, qvector = c(1:6),
                 method = "lm", criterion = "variance")
plot(qob)
selection(x = trdata[, -1], y = trdata$y, q = 1, criterion = "variance",
          method = "lm", family = "gaussian")
ols4 <- lm(y ~ x1, data=trdata)
ols4.mspe <- mean( (gy - predict(ols4, newdata=gendata))^2 )

# Гребневая регрессия:
library(MASS)
lmridge <- lm.ridge(y ~ x1 + x2 + x3 + x4 + x5 + x6, data=trdata, 
                    lambda = seq(0, 10, 1))
lmridge$kHKB   # lambda, полученная методом kHKB
lmridge$kLW    # lambda, полученная методом kLW
lmridge$GCV

# обобщенная перекрестная-проверка показала, что оптимальное значение 
# lambda лежит между  6 и 8
# Выполним несколько итераций для уточнения этого значения
lm.ridge(y ~ x1 + x2 + x3 + x4 + x5 + x6, data = trdata,
         lambda = seq(6, 8, 0.25))$GCV
lm.ridge(y ~ x1 + x2 + x3 + x4 + x5 + x6,
         data = trdata, lambda = seq(6.5, 7, 0.1))$GCV
lm.ridge(y ~ x1 + x2 + x3 + x4 + x5 + x6,
         data = trdata, lambda = seq(6.7, 6.9, 0.05))$GCV
lm.ridge(y ~ x1 + x2 + x3 + x4 + x5 + x6,
         data = trdata, lambda = seq(6.75, 6.85, 0.01))$GCV

# Построим четыре модели гребневой регрессии с различными значениями 
#  lambda: 2.7 (kHKB), 4.0 (kLW), 6.8 (GCV) и 0 (OLS).
(ridge1 <- lm.ridge(y ~ x1 + x2 + x3 + x4 + x5 + x6, 
                    data = trdata, lambda = 2.7))
(ridge2 <- lm.ridge(y ~ x1 + x2 + x3 + x4 + x5 + x6, 
                    lambda = 4.0))
(ridge3 <- lm.ridge(y ~ x1 + x2 + x3 + x4 + x5 + x6, 
                    data = trdata, lambda = 6.8))

# Модель с lambda = 0 сводится к полной модели OLS:
(ridge4 <- lm.ridge(y ~ x1 + x2 + x3 + x4 + x5 + x6,
                    data = trdata, lambda = 0))
ridge1.mspe <- mean( (gy - (gx %*% ridge1$coef + ridge1$ym))^2 )
ridge2.mspe <- mean( (gy - (gx %*% ridge2$coef + ridge2$ym))^2 )
ridge3.mspe <- mean( (gy - (gx %*% ridge3$coef + ridge3$ym))^2 )
ridge4.mspe <- mean( (gy - (gx %*% ridge4$coef + ridge4$ym))^2 )

# Лассо-регрессия:
library(lars)
M.lasso <- lars(as.matrix(trdata[, 2:7]), trdata[, 1], type = "lasso") 
plot(las, plottype = "coefficients"); plot(las, plottype = "Cp")

# Из графика для Cp получили параметр фракционирования S = 0.56
las.Cp <- predict(M.lasso, as.matrix(trdata[, 2:7]), s = 0.56,
                  type = "coefficient", mode = "fraction")

# А теперь попробуем рассчитать его значение перекрестной проверкой:
r <- cv.lars(as.matrix(trdata[, 2:7]), trdata[, 1]) 
(bestfrac <- r$index[which.min(r$cv)])     # Кросс-проверка
las.CV <- predict(M.lasso, as.matrix(trdata[, 2:7]), s = bestfrac,
                  type = "coefficient", mode = "fraction")
las.CV  # Получение коэффициентов модели

# При  s = 1 лассо-модель сводится к полной модели OLS1:
las.OLS <- predict(M.lasso, as.matrix(trdata[, 2:7]), s = 1,
                   type = "coefficients", mode = "fraction")
las.OLS
predlas <- predict.lars(M.lasso, newx = gx, type = "fit",
                        mode = "fraction", s = 0.434)
las1.mspe <- mean( (gy - predlas$fit)^2 )  
predlas <- predict.lars(M.lasso, newx = gx, type = "fit",
                        mode = "fraction", s = 0.56)
las2.mspe <- mean( (gy - predlas$fit)^2 )  
predlas <- predict.lars(M.lasso, newx = gx, type = "fit",
                        mode = "fraction", s = 1)
las3.mspe <- mean( (gy - predlas$fit)^2 )  

# Регрессия на главные компоненты:
tr.pca <- princomp(~ x1 + x2 + x3 + x4 + x5 + x6 , data = trdata)
summary(tr.pca); plot(tr.pca)
loadings(tr.pca)
pcomp.tr <- predict(tr.pca)
pc1 <- pcomp.tr[,1]
pc2 <- pcomp.tr[,2]
pc3 <- pcomp.tr[,3]
pc4 <- pcomp.tr[,4]
pc5 <- pcomp.tr[,5]
pc6 <- pcomp.tr[,6]
cor(pcomp.gen) 
pcr1 <- lm(y ~ pc1 + pc2 + pc3 + pc4 + pc5 + pc6)
summary(pcr1)

pcr2 <- step(pcr1)
summary(pcr2)
pcomp.gen <- predict(tr.pca, newdata = gendata)
genpcdata <- data.frame(pcomp.gen)
names(genpcdata) <- c("pc1", "pc2", "pc3", "pc4", "pc5", "pc6")
pcr1.mspe <- mean( (gy - predict(pcr1, newdata = genpcdata))^2 )
pcr2.mspe <- mean( (gy - predict(pcr2, newdata = genpcdata))^2 )

mspe.all <- c(
ols1.mspe,    ### OLS (все переменные)
ols2.mspe,  ### МНК (2 переменные, найденные включениями с исключениями)
ols3.mspe,    ### МНК (2 переменные, найденные полным перебором)
ols4.mspe,  ### МНК (1 переменная, найденная методом "forward selection")
ridge1.mspe,  ### гребневая модель (lambda получена HKB)
ridge2.mspe,  ### гребневая модель (lambda получена L-W)
ridge3.mspe,  ### гребневая модель (lambda получена обобщенной c-v)
ridge4.mspe,  ### гребневая модель с lambda= 0
las1.mspe,    ### лассо (фракция найдена кросс-проверкой c-v)
las2.mspe,    ### лассо (фракция найдена Cp)
las3.mspe,    ### лассо с фракцией = 1
pcr1.mspe,    ### PCA-регрессия  с полным набором главных компонент
pcr2.mspe)    ### PCA-регрессия (используются 3 главных компоненты)

sort(mspe.all)
