
#  Мастицкий С.Э., Шитиков В.К. (2015) Статистический анализ и визуализация 
#  данных с помощь R. Купить книгу можно здесь: http://bit.ly/1QopZ0A


#########################################################################
#      Глава 8. ОБОБЩЕННЫЕ, СТРУКТУРНЫЕ И ИНЫЕ МОДЕЛИ РЕГРЕССИИ
#########################################################################


# -------------------------- Разделу 8.1 -------------------------------
# Формирование таблицы данных для построения модели:
moscow.dat <- read.delim("TAVG_Moscow.txt", header = TRUE)
Year.list <- unique(moscow.dat$Year)
moscow.mat <- matrix(moscow.dat$Anomaly, nrow = 260, ncol = 12, byrow = TRUE)
Tbase <- c(-10.39,-9.28,-4.06,4.80,11.96,16.16,
           17.76,16.25,10.77,4.37,-1.86,-6.65)
moscow.nmat <- t(sapply(1:260,
                        function(i) moscow.mat[i,]+Tbase))
dimnames(moscow.nmat) = list(Year.list, 1:12)
head(moscow.nmat)

# Сохраним матрицу данных для возможного последующего использования:
save(moscow.nmat, Year.list, file = "TAVG_Moscow.RData")

load(file = "TAVG_Moscow.RData")

# График "ступенчатой" сглаживающей линии:

# Средние летние температуры:
T.summer <- apply(moscow.nmat[, 6:8], 1, mean)
summary(T.summer)
moscow.mat10 <- matrix(T.summer, nrow = 26, ncol = 10, byrow = TRUE)

# Рис. 122:
plot(seq(1753, 2003, by = 10), rowMeans(moscow.mat10), 
     type = "s", ylab = "Cредняя температура, град.С",
     xlab = NA, col = "blue", lwd = 1.5, las = 2, 
     cex.axis = 0.8, main = "Москва, июнь-август")
abline(h = mean(T.summer), col = 1, lty = 2, lwd = 1.2)
text(1830, 16.85, adj = 0, "Общее среднее", cex = 0.75)

# Метод локальной регрессии 

# Создаем список значений параметра сглаживания:
spanlist <- c(0.15, 0.50, 0.8, 1.0)

# Рис. 123:
plot(Year.list, T.summer, col = "grey", type = "l", xlab = "",
     ylab = "Cредняя температура, град.С")
for (i in 1:length(spanlist)) {
        T.loess <- loess(T.summer ~ Year.list, span = spanlist[i])
        T.predict <- predict(T.loess) 
        lines(Year.list, T.predict, col = i, lwd = 2) 
}
legend ("topleft", c(paste("span =", 
                           formatC(spanlist, digits = 2,
                                   format = "f"))), 
        col = 1:length(spanlist), lwd = 2, bty = "n")


# Ядерная модель сглаживания

# Функция оценки оптимальной ширины окна кросс-проверкой:
bandcrossval<- function(x, y, nstep = 20, bmax = 0, L1 = TRUE){
        if (bmax == 0){bmax <- 1.5*sqrt(var(x))}
        bstep<-  bmax/nstep ; n <- length(x); SSE<- c(1:nstep)*0 
        for (i in 1:nstep){ 
                for (k in 2:(n-1)){
                        xx<- c(x[1:(k-1)], x[(k+1):n]) 
                        yy<- c(y[1:(k-1)], y[(k+1):n])
                        kss<- ksmooth(xx, yy, "normal", ban=(i*bstep),
                                      x.points=x[k]) 
                        if(L1 == FALSE) {SSE[i]<- SSE[i]+(y[k]-kss$y )^2 }
                        if(L1 == TRUE) {SSE[i]<- SSE[i]+ abs(y[k]-kss$y )}
                }}
        k<- c(1:nstep)*bstep 
        return(k[SSE==min(SSE,na.rm = TRUE)])
}

#  Сглаживание ядерной функцией и вывод графика

# Средние зимние температуры:
T.winter <- apply(moscow.nmat[, c(1,2,12)], 1, mean)
summary(T.winter)

#  Квадрат ошибки при кросс-проверке:
bandcrossval(Year.list, T.winter, L1 = FALSE) 

#  Абсолютная ошибка при кросс-проверке:
bandcrossval(Year.list, T.winter)

# Рис. 126:
plot(Year.list, T.winter, ylab = "Cредняя температура, град. С",
     xlab = "", col = "grey", type = "l",
     main = "Москва: январь, февраль, декабрь")
lines(ksmooth(Year.list, T.winter, "normal", bandwidth = 5), col = "gold")
lines(ksmooth(Year.list, T.winter, "normal", bandwidth = 15), col = "red")
lines(ksmooth(Year.list, T.winter, "normal", bandwidth = 25), col = "green")
lines(ksmooth(Year.list, T.winter, "normal",
              bandwidth = 50.8), col = "blue", lwd = 2)
lines(ksmooth(Year.list, T.winter, "normal", bandwidth = 67.7), col = 1, lwd = 2)
legend ("bottomright", lwd = c(1,1,1,2,2),
        c("band = 5", "band = 15", "band = 25", "band = 50.8",
          "band = 67.7"), col = c("gold", "red", "green", "blue", 1))

# Аппроксимация сплайнами
T.delta <- T.summer - T.winter

# Средняя разность лентих и зимних температур:
summary(T.delta)

# Выводим кривые сглаживания сплайнами 
# для различных значений параметра spar (рис. 127):
plot(Year.list, T.delta, col = "grey", type = "l", xlab = "",
     ylab = "Cредняя разность температур, град. С")
abline(lm(T.delta ~ Year.list), col = "grey55", lwd = 2)
lines(smooth.spline(Year.list, T.delta, spar = 0.3), col = "gold")
lines(smooth.spline(Year.list, T.delta, spar = 0.5), col = "red")
lines(smooth.spline(Year.list, T.delta, spar = 0.75), col = "green", lwd = 2)
lines(smooth.spline(Year.list,T.delta, spar=1.0), col = "blue", lwd = 2) 
T.spline <- smooth.spline(Year.list, T.delta, cv = TRUE) 
T.spline$spar  # Параметр сглаживания найден кросс-проверкой 
lines(T.spline, lwd = 2) # Оптимальная кривая
legend("bottomleft", c("spar = 0.3", "spar = 0.5", "spar = 0.75",
                       "spar = 1", "spar = 1.5"), lwd = c(1,1,2,2,2),
       col = c("gold", "red", "green", "blue", 1))



# -------------------------- Раздел 8.3 -------------------------------
# По вероятностям вычисляем квантили, по квантилям - вероятности:
p = c(0.05, 0.1, 0.25, 0.4, 0.5, 0.6, 0.75, 0.9, 0.95) 
q = qnorm(p)
rbind(q,p = pnorm(q))

# Пробит-регрессия для моделирование зависимости "доза-эффект" 
# Загружаем данные из файла:
df <- read.table(file = "Гадюки.txt", header = TRUE, sep = "\t")
head(df)
levels(df$местооб)

# Строим модель на всем имеющемся материале:
dead <- cbind(df$погибло, df$выжило)
M1 <- glm(dead ~ доза, family = binomial(link = "probit"), data = df)
summary(M1)

M0 <- glm(dead ~ 1, family = binomial(link = "probit"), data = df)
anova(M1,M0, test = "Chisq")

df.plot = data.frame(df, p = M1$fit, пробит = predict(M1, type =  "link"),
                     эффект = df$погибло/(df$погибло+df$выжило))
df.plot = df.plot[order(df.plot$доза), ]

plot(df.plot$доза, df.plot$p, type="l", lwd = 2, xlab="Доза", ylab="Доля умерших")
points(df.plot$доза, df.plot$эффект, cex = 0.7, pch = 23 +
               as.numeric(df.plot$пол), bg = 1 + as.numeric(df.plot$пол)) 
legend("bottomright",c("Самки","Самцы"), pt.bg = 2:3, pch = 24:25, cex = 0.7)

# Включаем в модель пол животного:
M2b <- glm(dead ~ доза * пол, family = binomial(link = "probit"), data = df)
summary(M2b)
anova(M1b, M2b, test = "Chisq")

M2a <- glm(dead ~ пол/доза - 1, family = binomial(link = "probit"), data = df)
summary(M2a)

library(MASS)  # Длпя расчета LD50

# На всем имеющемся материале:
dose.p(M1b, p = 0.5)

# Отдельно для самок и самцов:
dose.p(M2a, c(1, 3))           ## Самки
dose.p(M2a, c(2, 4))           ## Самцы

# Включаем в модель местообитание змей:
M3b <- glm(dead ~ доза * местооб, family = binomial(link = "probit"), data = df)
summary(M3b)
anova(M1,M3b, test = "Chisq")

M3a <- glm(dead ~ местооб/доза - 1, family = binomial(link = "probit"), data = df)
summary(M3a)
lf = length(levels(df$местооб))

# Линии регрессии в координатах доза-пробит (рис. 129):
plot(df.plot$доза, df.plot$пробит, type = "l", lwd = 2, 
     xlab = "Доза", ylab = "Пробит доли умерших")
for (i in 1:lf)  abline(coef(M3a)[i], coef(M3a)[lf+i], col = i+1)
legend("bottomright", c("Все", levels(df$местооб)),
       col = 1:8, lwd = c(2,rep(1,7)))

# Рассчитаем региональные величины DL50 для яда гадюки:
df.dl = data.frame(region =levels(df$местооб), 
                   LD50 = rep(0, lf), SE = rep(0, lf))
for (i in 1:lf)  {
        a = dose.p(M3a, c(i,lf+i))
        df.dl[i, 2] = a[1]
        df.dl[i, 3]  = as.numeric(attr(a, "SE")) 
}
df.dl

# Модель логистической регрессии:
infection <- read.table(file = "dreissena_infection.txt",
                        header = TRUE, sep = "\t")
str(infection)
n.total <- infection$Infected + infection$Noninfected
prop.inf <- infection$Infected/n.total
inf.tbl <- cbind(infection$Infected, infection$Noninfected)

M1 <- glm(inf.tbl ~ Day + Depth,
          family = binomial(link = "logit"), data = infection)
summary(M1)

M2 <- glm(prop.inf ~ Day + Depth, weights = n.total, 
          family = binomial(link = "logit"), data = infection)
summary(M2) 
anova(M2, test = "Chisq")

M0 <- glm(prop.inf ~ 1, family = binomial(link = "logit"), data = infection)

anova(M2, M0, test = "Chisq")

inf.raw <- read.table(file = "dreissena_infection_raw_data.txt",
                      header = TRUE, sep = "\t")
head(inf.raw)

M3 <- glm(EchinoPresence ~ Day + Depth,
          family = binomial(link = "logit"), data = inf.raw)
summary(M3)

# Включаем в модель длину моллюска:
M4 <- glm(EchinoPresence ~ Length + Day + Depth,
          family = binomial(link = "logit"), data = inf.raw)
anova(M4, M3, test = "Chisq")

# Выводим предсказанные значения инвазированности:
mp <- predict(M3, type = "response") 
classified <- ifelse(mp > (1-mp), 1, 0)

# Расчет ошибки предсказания:
mean(inf.raw$EchinoPresence != classified)
table(Факт = inf.raw$EchinoPresence, Прогноз = classified)

	
# ------------------------- Раздел 8.4 --------------------------------- 
require(vegan); require (mgcv)

# Определяем исходные данные:
SpecEnv <- read.delim("Elton_TolInt.txt", header = TRUE)
gr <-  SpecEnv$S   # Действующий фактор - минерализация

# Логарифмируем численность вида Palpomyia sp. (см. vegan):
amblog.species <- decostand(SpecEnv$CePal_sp, "log")

# Создадим бинарную переменную встречаемости:
bin.species <- SpecEnv$CePal_sp
bin.species[bin.species > 0] <- 1
data.frame(gr, Nat = SpecEnv$CePal_sp, NatLog = amblog.species, Bin = bin.species)

#  Определим предварительно три вспомогательные функции

# 1 Возвращает объект "лучшей" модели GLM:
model.GLM <- function (part.species, gr, family = 'gaussian') {
        glm.1<-glm(part.species ~ poly(gr, 1), family = family)
        glm.2<-glm(part.species ~ poly(gr, 2), family = family)
        glm.3<-glm(part.species ~ poly(gr, 3), family = family)
        allAIC <- c(extractAIC (glm.1)[2], 
                    extractAIC (glm.2)[2], extractAIC (glm.3)[2])
        switch (c(1,2,3)[allAIC == min(allAIC)],
{glm.GLM <- glm.1; GLM.deg <- 1},
{glm.GLM <- glm.2; GLM.deg <- 2},
{glm.GLM <- glm.3; GLM.deg <- 3})
return (glm.GLM)
}

# 2 Возвращает объект "лучшей" модели GAM:
model.GAM <- function (part.species, gr, family = 'gaussian') {
        gam.3<-gam(part.species ~ s(gr, k = 3), family = family)
        gam.4<-gam(part.species ~ s(gr, k = 4), family = family)
        gam.5<-gam(part.species ~ s(gr, k = 5), family = family)
        allAIC <- c(extractAIC (gam.3)[2], 
                    extractAIC (gam.4)[2], extractAIC(gam.5)[2])
        switch (c(1,2,3)[allAIC == min(allAIC)],
{gam.GAM <- gam.3; GAM.deg <- 3},
{gam.GAM <- gam.4; GAM.deg <- 4},
{gam.GAM <- gam.5; GAM.deg <- 5})
return(gam.GAM)
}

# 3 Возвращает характеристики модели, необходимые для вывода графика:
fit.model <- function (M, axx) {
        fit <- predict(M, list(gr = axx), type = 'response')
        Sopt <- axx[fit == max(fit)][1]
        auc <- 80; whole.auc <- sum (fit)
        part.auc <- whole.auc * (auc/100)
        for (l in seq(max(fit)[1], min(fit)[1], length = 100))
        {  if (sum (fit[fit >= l]) >= part.auc) 
        {last.l <- l; break ()}
        }
        amplitude.prob <- last.l
        amplitude.range <- range(axx[fit >= amplitude.prob])
        return (list (fit, Sopt, amplitude.range))
}

# Выполнение расчетов

#  Модели прологарифмированного отклика:
GLM.amb <- model.GLM(amblog.species, gr)
summary(GLM.amb)
summary(lm(amblog.species ~ poly(gr, 3)))

GAM.amb <- model.GAM(amblog.species, gr)
summary(GAM.amb)

axx <- seq(range(gr)[1], range(gr)[2], len = 100)
l.GLM <- fit.model(GLM.amb, axx)
plot(gr, amblog.species, type = "p",xlab = "Минерализация S, г/л",
     ylab = "Численность ln(N)",  pch = 20)
lines(axx, l.GLM[[1]], lwd = 2)
lines(axx, fit.model(GAM.amb, axx)[[1]], lwd = 2, lty = 4)
abline(v=l.GLM[[2]], lty = 2)
abline(v=l.GLM[[3]][1], lty = 2)
abline(v=l.GLM[[3]][2], lty = 2)
text (23.8, 0, "Smin", font = 4); text (32.8, 0, "Sopt", font = 4) 
text (39, 0, "Smax", font = 4) 
legend("topleft", lty = c(1, 4), lwd = 2, c("GLM", "GAM"))

#  Модели бинарного отклика:
GLM.gaus <- glm(bin.species ~ gr + I(gr^2), family = poisson)
summary(GLM.gaus)
GLM.bin <- model.GLM(bin.species, gr, family = 'binomial')
anova(GLM.bin, test = "Chisq")
GAM.bin <- model.GAM(bin.species, gr, family = 'binomial')

GAUS.fun <- function(p, x)  #  Прогнозируемые значения GAUSS
{ exp(p[1] + p[2]*x + p[3]*(x^2)) }
p.glm.gaus <- coef(GLM.gaus)
fit.GAUS <- GAUS.fun(p.glm.gaus, axx)
#  Для других моделей используем ранее определенную функцию
l.GLM <- fit.model(GLM.bin, axx)
l.GLM[[2]]    #  Экологический оптимум для GLM 
l.GAM <- fit.model(GAM.bin, axx)
l.GAM[[2]]    #  Экологический оптимум для GAM
plot(gr,bin.species, type = "p", xlab = "Минерализация S, г/л",
     ylab = "Вероятность, р", ylim = c(0, 1.2), pch = 20)
lines(axx,l.GLM[[1]], col = "blue", lwd = 2)
abline(v=l.GLM[[2]], col = "blue", lty = 2)
lines(axx,l.GAM[[1]], col ="green", lwd = 2)
abline(v=l.GAM[[2]], col = "green", lty = 2)
lines(axx,fit.GAUS, col = "red", lwd = 2)
legend ("topleft", col = c("red", "blue", "green"), lwd = 2,
        c("GAUSS", "GLM", "GAM"))

#-----------------------------------------------------------------------	
#   К разделу  8.5. Ковариационный анализ
#-----------------------------------------------------------------------	

# Подготовим таблицу с данными:
data(hellung, package = "ISwR")
hellung$glucose = as.factor(hellung$glucose)
head(hellung)

# Рис. 132:
library(ggplot2)
ggplot(hellung, aes(x = conc, y = diameter, color = glucose)) +
        geom_point() + geom_smooth()

# Строим линейную модель:
(lm.nogluc <- lm(log10(diameter) ~ log10(conc),
                 data = hellung, subset = glucose == 2))

# С глюкозой:
(lm.gluc <- lm(log10(diameter) ~ log10(conc), 
               data = hellung, subset = glucose == 1))

# Рис. 133:
# Выводим распределение точек для преобразованных данных:
p_symb = c(17, 1)
plot(hellung$conc, hellung$diameter,
     pch = p_symb[as.numeric(hellung$glucose)], log = "xy",
     xlab = "cons", ylab = "diameter")
# Добавляем регрессионные линии для каждой группы:
abline(lm.nogluc, col = "blue", lwd = 2)
abline(lm.gluc, col = "green", lwd = 2)
# Линия регрессии без учета влияния глюкозы:
abline(lm(log10(diameter)~ log10(conc), data = hellung), col = "red", lty = 2)
legend("topright", bty = "n", legend = c("без глюкозы",
                                         "с глюкозой", "обе группы"), 
       lwd = c(2, 2, 1),
       lty = c(1, 1, 2), col = c("blue", "green", "red"))

# Модели ковариационного анализа:
AN1  <- lm(log10(diameter) ~ log10(conc)*glucose)
summary(AN1)
AN2  <- lm(log10(diameter) ~ log10(conc) + glucose) 
summary(AN2)
anova(AN2)

	
# ----------------------------- Раздел 8.6 -----------------------------
# Загрузим таблицу с данными:
RIKZ <- read.table(file = "RIKZ.txt", header = TRUE, dec = ".")
head(RIKZ) 
ExposureBeach <- c("a","a","b","b","a","b","b","a","a")

# 1. Двухэтапный анализ:
Beta <- vector(length = 9) #  Первый этап
for (i in 1:9){
        tmpout<-summary(lm(Richness ~ NAP,
                           subset = (Beach == i), data = RIKZ))
        Beta[i] <- tmpout$coefficients[2, 1]
}
print(Beta, 3)

#  Второй этап
tmp2 <- lm(Beta ~ factor(ExposureBeach),data=RIKZ)
summary(tmp2)

# 2. Модель со случайным свободным членом:
library(nlme)
RIKZ$fBeach <- factor(RIKZ$Beach)
Mlme1 <- lme(Richness ~ NAP, random = ~1 | fBeach, data = RIKZ)
summary(Mlme1)
F0 <- fitted(Mlme1, level = 0)
F1 <- fitted(Mlme1, level = 1)
I <- order(RIKZ$NAP); NAPs <- sort(RIKZ$NAP)
# Рис. 134:
plot(NAPs, F0[I], lwd = 2, col = "blue", type = "l", yim = c(0,22),
     ylab = "Число видов", xlab = "NAP")
for (i in 1:9){
        x1 <- RIKZ$NAP[RIKZ$Beach == i]
        y1 <- F1[RIKZ$Beach == i]; K <- order(x1)
        lines(sort(x1), y1[K])
}
text(RIKZ$NAP, RIKZ$Richness, RIKZ$Beach, cex = 0.9)

# 3. Модель со случайными свободным членом и коэффициентом угла наклона: 
Mlme2 <- lme(Richness ~ NAP, method = "REML",
             random = ~1 + NAP | fBeach, data = RIKZ)
summary(Mlme2)

F0 <- fitted(Mlme2, level = 0)
F1 <- fitted(Mlme2, level = 1)
I <- order(RIKZ$NAP)
NAPs <- sort(RIKZ$NAP)

plot(NAPs, F0[I], lwd = 2, col = "blue", type = "l", ylim = c(0, 22),
     ylab = "Число видов", xlab = "NAP")
for (i in 1:9){
        x1 <- RIKZ$NAP[RIKZ$Beach == i]
        y1 <- F1[RIKZ$Beach == i]
        K <- order(x1)
        lines(sort(x1), y1[K])
}
text(RIKZ$NAP, RIKZ$Richness, RIKZ$Beach, cex = 0.9)

# 4. Смешанные модели с использованием всех предикторов:
RIKZ$fExp <- RIKZ$Exposure
RIKZ$fExp[RIKZ$fExp == 8] <- 10
RIKZ$fExp <- factor(RIKZ$fExp, levels = c(10, 11))

Mlme4 <- lme(Richness ~1 + NAP * fExp, random = ~1 + NAP | fBeach, data = RIKZ)
summary(Mlme4)

Mlme5 <- lme(Richness ~1 + NAP + fExp, random = ~1 + NAP | fBeach, data = RIKZ)
summary(Mlme5)

Mlme6 <- lme(Richness ~1 + NAP + fExp, data = RIKZ, random = ~1 | fBeach, 
             method = "REML")
summary(Mlme6)
anova(Mlme4, Mlme5, Mlme6)

	
# --------------------------- Раздел 8.7 -------------------------------
X <- state.x77[, c(1:3, 5:8, 4)] 
colnames(X)[8] = "Life.Exp"
colnames(X)[5] = "HS.Grad"
M <- ncol(X); N <- nrow(X)
names(X)

FormulList <- c(
        "Y~X1","Y~X2", "Y~X1+X2", "Y~X1+X2+I(X1*X2)",
        "Y~X1+X2+I(X1*X1)","Y~X1+X2+I(X2*X2)",
        "Y~X1+X2+I(X1*X2)+I(X1*X1)","Y~X1+X2+I(X1*X2)+I(X2*X2)",
        "Y~X1+X2+I(X1*X2)+I(X1*X1)+I(X2*X2)")
as.data.frame(FormulList)

library(cvTools)

set.seed(1234)
folds <- cvFolds(N, K = 10, R = 5)

#  Определим предварительно две функции
NEML <- function (Y, X1, X2) {
        allAIC <- sapply(1:length(FormulList), function(k) 
                AIC(lm(as.formula(FormulList[k]))))
        Iopt <- which.min(allAIC)
        Mopt <- lm(as.formula(FormulList[Iopt]))
        cvFitLm <- cvLm(Mopt, cost = rtmspe, folds = folds, trim = 0.1)
        return (c(Iopt, cvFitLm$cv))
}

PRED.NEML <- function (Y, X1, X2, Iopt) {
        Mopt <- lm(as.formula(FormulList[Iopt]))
        return (Mopt)
}

# Реализуем вычисления по многорядному алгоритму МГУА:
eps <- 0.001   # Малое число для оценки стабилизации L

AllModel <- list()  # Список для складирования коэффициентов
for (h in 1:(M-1)) {   #  Наращивание рядов селекции
        res1 <- matrix(0, ncol = 6, nrow = (M-1)*(M-2)/2)
        colnames(res1) <- c("H","пп","I", "J", "Nmod", "CVcrit")
        a <- 0
        for (i in 1:(M-2)) {   # Перебор всех пар предикторов
                for (j in (i+1):(M-1))   {
                        a <- a + 1
                        res1[a, 1:4] = c(h, 0, i, j)
                        res1[a, 5:6] = NEML(X[, M], X[, i], X[, j]) 
                }
        }
        
# Получили таблицу частных описаний для одного ряда селекции.
# Сортируем ее по величине критерия самоорганизации:
res1 <- res1[order(res1[,6]), ]
res1[, 2] <- 1:nrow(res1)
if (h == 1) {			#  Первый шаг селекции 
        CVglob <- res1[1, 6]   	#  Лучшая модель в ряду
        result <- res1[1:(M-1), ] 
} else {
        #  Если шаг селекции не первый, проверяем сходимость процесса
        if((CVglob-res1[1, 6]) < eps) break
        CVglob <- res1[1, 6]
        result <- rbind(result, res1[1:(M-1), ])
}
listMod1 <- lapply(1:(M-1), function(k) PRED.NEML(X[, M],
                                                  X[, res1[k, 3]],
                                                  X[, res1[k, 4]], res1[k, 5]))
# Для (M-1)-лучших моделей извлекаем коэффициенты и прогнозы
listCoef1 <- lapply(listMod1, function(fit) fit$coef)
XP <- sapply(listMod1, function(fit) fit$fit) 
AllModel <- c(AllModel, listCoef1)
# Заменяем исходную таблицу на подогнанные значения Y (!)
X[, 1:(M-1)] <- XP  
}
result  	# Получаем результаты моделирования
AllModel[[15]]  # X2 = V7
AllModel[[14]]  # X1 = U1  X2 = U5
AllModel[[1]]   # X1 = Population   X2 = Murder 
AllModel[[5]]   # X1 = Murder    X2 = HS.Grad
sd(X[,1]-X[,M]) # Стандартное отклонение разностей 
                # (остатков без учета числа степеней свободы регрессии!!!)

#  Построение полной линейной модели
fitLm <- lm(Life.Exp ~ ., data = X)
summary(fitLm)
# Кросс-проверка полной линейной модели
cvLm(fitLm, cost = rtmspe, folds = folds, trim = 0.1)

	
# --------------------------- Раздел 8.8 ------------------------------- 

# Пример 1 - разведение свиней

# Подготовим таблицу с данными:
Ddf <- as.data.frame(matrix(
        c(-3, 0.6, -200, 3, -1, -0.4, -200, -1,2, -0.2, 0, -1, -1,
          0.6, 100, 6,3, -0.6, 300, -7), 
        nrow = 5, ncol = 4, byrow = TRUE,
        dimnames = list(c("1990", "1991", "1992", "1993", "1994"),
                        c("Y1", "Y2", "X1", "X2"))))

Ddf # Данные представлены в виде абсолютных отклонений от среднего.
    # Находим коэффициенты регрессии для приведенной модели:
(E1 <- lm(Y1 ~ X1 + X2, data = Ddf)$coef)
(E2 <- lm(Y2 ~ X1 + X2, data = Ddf)$coef)

# Пересчитываем коэффициенты для структурной модели: 
Coef <- matrix(NA, nrow = 2, ncol = 2)
Coef[1, 1] = E1[3]/E2[3]; Coef[1, 2] = E1[2] - Coef[1, 1]*E2[2]
Coef[2, 1] = E2[2]/E1[2]; Coef[2, 2] = E2[3] - Coef[2, 1]*E1[3]
Coef

library(sem)
summary(tsls(Y1 ~ Y2 + X1 + X2, # 1-е уравнение системы
			instruments ~ X1 + X2), data = Ddf)
summary(tsls(Y2 ~ Y1 + X2, # 2-е уравнение системы
			instruments ~ X1 + X2, data = Ddf))

# Пример 2 - Модель Клейна

Klein$P.lag <- with(Klein, c(NA, P[-length(P)]))
Klein$X.lag <- with(Klein, c(NA, X[-length(X)]))
Klein$A <- Klein$Year - 1931
head(Klein)

# Подгонка структурных моделей Клейна методом 2SLS.
# Определим формулу для списка инструментальных переменных:
InstFor <- as.formula(~ G + T + Wg + A + P.lag + K.lag + X.lag)
eqn.1 <- tsls(C ~ P + P.lag + I(Wp + Wg), InstFor, data = Klein)
summary(eqn.1)  # Модель 1 потребительской функции
anova(eqn.1, tsls(C ~ 1, InstFor, data = Klein))

# Аналогичным образом выполним подгонку и проверку
# адекватности остальных двух уравнений:
eqn.2 <- tsls(I ~ P + P.lag + K.lag, InstFor, data = Klein)
eqn.3 <- tsls(Wp ~ X + X.lag + A, InstFor, data = Klein)

# Пример 3 - Демократизация политической жизни

library(lavaan) 
head(PoliticalDemocracy)
model <- '
# вычисляемые компоненты
ind60 =~ x1 + x2 + x3
dem60 =~ y1 + y2 + y3 + y4
dem65 =~ y5 + y6 + y7 + y8
# регрессия
dem60 ~ ind60
dem65 ~ ind60 + dem60
# корреляции остатков
y1 ~~ y5
y2 ~~ y4 + y6
y3 ~~ y7
y4 ~~ y8
y6 ~~ y8
'
fit <- sem(model, data = PoliticalDemocracy)
parameterEstimates(fit)
fitMeasures(fit, c("chisq", "df", "pvalue",  "aic",
                   "bic", "cfi", "rmsea", "rmsea.pvalue" ))

library(semPlot)

# классическая форма диаграммы (рис. 137):
semPaths(fit, "model", "est", style = "lisrel")

# Привет от Св. Валентина (рис. 138):
semPaths(fit, "std", "hide", sizeLat = 15, shapeLat = "star",
         shapeMan = "heart", col = list(man = "pink", lat = "yellow"),
         residuals = FALSE, borders = FALSE, edge.color = "purple",
         XKCD = TRUE, edge.width = 2, rotation = 2, layout = "tree2",
         fixedStyle = 1, mar = c(1, 3, 1, 3))
