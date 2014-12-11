#########################################################################

#  Мастицкий С.Э., Шитиков В.К. (2014) Статистический анализ и визуализация 
#  данных с помощь R. (Адрес доступа: http://r-analytics.blogspot.com)

#########################################################################

#########################################################################
# Глава 9.	 ПРОСТРАНСТВЕННЫЙ АНАЛИЗ И СОЗДАНИЕ ГЕОГРАФИЧЕСКИХ КАРТОГРАММ
#########################################################################

#-----------------------------------------------------------------------
#  К разделу 9.1.Простая карта: использование растрового рисунка и подсчет расстояний
#-----------------------------------------------------------------------	

# Загрузка растровой карты
library(jpeg)
image <- readJPEG("maps.yandex.ru.JPG")
Xras = 1015; Yras = 635
Reg <- c("Вологда","Пермь","Казань","Самара","Пенза","Липецк")
par(mar = c(0, 0, 0, 0))
plot(1,xlim = c(0, Xras), ylim = c(0, Yras), xlab = "", ylab = "")
lim <- par()
rasterImage(image, lim$usr[1], lim$usr[3], lim$usr[2], lim$usr[4])

# Щелкните мышкой 6 раз по карте в точках, где находятся города Reg:
xy <- locator(6)
xy
PA <- c(21.7,19.1,16.4,16.7,9.4,6.2)
mycex = ((PA - min(PA))^2/max(PA) + 2)
colpts = rgb(0.2, 0.5, 0.4, alpha = 0.6)
points(xy$x, xy$y,cex = mycex, col = 1, pch = 21, bg = colpts)

# Связь расстояний на растровой карте с морфологическими признаками:
df <- data.frame(xy)
dist(df)
Rmat <- matrix(c(
0,		0.87763393,	1.37359643,	1.07033575,	1.26727796,	1.62654352,
0.87763393,	0,		1.03383327,	1.01546645,	1.12793326,	1.6236099,
1.37359643,	1.03383327,	0,		1.22476184,	1.09088194,	1.39409792,
1.07033575,	1.01546645,	1.22476184,	0,		1.18241739,	1.87482882,
1.26727796,	1.12793326,	1.09088194,	1.18241739,	0,		1.57430208,
1.62654352,	1.6236099,	1.39409792,	1.87482882,	1.57430208,		 0),
nrow = 6)

Rdist <- as.dist(Rmat)

#  Тест Мантеля 
#  "Морфометрия гадюк"  ~ "Пространственное расположение"
library(vegan)
mantel(Rdist, dist(df)) 

# Прорисовка барьеров:
library(adegenet)
cn <- chooseCN(df, type = 1)
mon <- monmonier(df, Rdist, cn, threshold = 1.2, nrun = 2)
pdf("Berus.pdf")
plot(mon, method = "greylevel", add.arr = TRUE, col = "red")
points(df, pch = 21, cex = 1.2, bg = "green")
text(df, Reg, adj = c(0, 0))
graphics.off()
shell.exec("Berus.pdf") # Открываем файл со схемой

# Расчет расстояний на острове Мадагаскар:
library(sp)
library(maptools)
library(ggplot2)
library(rgeos)
load(url("http://biogeo.ucdavis.edu/data/gadm2/R/MDG_adm0.RData"))
madagascar <- fortify(gadm)
m <- ggplot() + geom_map(data = madagascar, 
                         aes(map_id = id), 
                         map = madagascar,
                         fill = "white", color = "black") +
        expand_limits(x = madagascar$long, y = madagascar$lat) +
        coord_map("mercator") + 
        xlab("Долгота") + ylab("Широта") + theme_bw()
mp <- m + geom_point(data = data.frame(Lon = c(45.0, 47.2,
                                               45.5, 48.5),
                                       Lat = c(-25.0, -21.0, -17.0, -15.0)),
                     aes(Lon, Lat), color = I("red"), size = 3)
print(mp)


library(geosphere)
# делим на 1000 для выражения расстояния в км:
distHaversine(c(45.0, -25.0), c(48.5, -15.0))/1000 

coords <- cbind(c(45.0, 47.2, 45.5, 48.5),
                c(-25.0, -21.0, -17.0, -15.0))
coords

Dist <- apply(coords, 1, 
              FUN = function(eachPoint) distHaversine(eachPoint, coords)/1000)
Dist

Dist <- Dist[lower.tri(Dist)]
Dist

mean(Dist)

#-----------------------------------------------------------------------	
#   К разделу  9.2.	Анализ пространственного размещения точек
#-----------------------------------------------------------------------	

library(spatstat)

# Загружаем текстовый файл с данными:  
plot_13 <- read.table(file = "13.txt", header = T, sep = "\t")

# Вычислим размеры площадки по осям Х и Y:
xmin <- floor(min(plot_13$x))
xmax <- ceiling(max(plot_13$x))
ymin <- floor(min(plot_13$y))
ymax <- ceiling(max(plot_13$y))

# Создадим объект ppp (point pattern):
ppp_object <- ppp(x = plot_13$x, y = plot_13$y, 
                  marks = data.frame(age = plot_13$age,
                                     blossoms = plot_13$blossoms),     # марки 
                  window = owin(c(xmin, xmax), c(ymin, ymax),  # Окно
                                unitname = c("metre","metres")))   # единицы измерения

# площадь площадки:
A <- area.owin(ppp_object$window)

# число точек:
N <- ppp_object$n

# сторона одной ячейки, чтобы в них попадало в среднем 2 точки:
L <- sqrt(2*A/N)

# сколько ячеек уместится в сторону площадки 10 x 10 м:
quadnum <- round(10/L)

# построим сетку ячеек:
ppp_quadrats <- quadrats(ppp_object, nx = quadnum, ny = quadnum)

# посчитаем число точек в ячейках, округлим до 1 десятичного знака:
quadcount <- quadratcount(ppp_object, tess = ppp_quadrats)
quadcount <- round(quadcount, 1)

# Oтобразим число точек в квадратах

# вычислим центры ячеек:
xgrid <- quadrats(ppp_object, nx = quadnum, ny = quadnum)$xgrid
ygrid <- quadrats(ppp_object, nx = quadnum, ny = quadnum)$ygrid
image(xgrid, ygrid,
      t(quadcount[order(1:quadnum, decreasing = T), ]),
      col = colorRampPalette(c("white", "green"))(15),
      axes=F, asp=1, xlab="", ylab="",
      main="Число точек в квадратах")
plot(quadcount, add=T, cex=0.7)      
axis(2, las = 1, pos = 0, at = seq(0, 10, 2), cex.axis=1)
axis(1, las = 1, pos = 0, at = seq(0, 10, 2), cex.axis=1)
axis(3, pos = 10, at = seq(0, 10, 2), labels = F, cex.axis=1)
axis(4, pos = 10, at = seq(0, 10, 2), labels = F, cex.axis=1)

# добавим точки:
plot(ppp_object, which.marks = "age",
           chars = c(19, 24), cex = 0.7, add = T)

# Хи-квадрат тест CSR с использованием численности точек в квадратах:
quadrat_test_result <- quadrat.test(ppp_object, nx = 9, ny = 9)
round(quadrat_test_result$expected, 2) # Частоты Пуассона
quadrat_test_result
kstest(ppp_object, "x")  #  Тест Колмогорова-Смирнова по оси Х
kstest(ppp_object, "y")  #  Тест Колмогорова-Смирнова по оси Y

# выделим генеративные особи:
gene_ppp <- ppp_object[ppp_object$marks$age=="gene"]

# вычислим размер скользящего окна, bandwidth по правилу Silverman:
sigma<-(sd(gene_ppp$x) + sd(gene_ppp$y))/2 
iqr<-(IQR(gene_ppp$x) + IQR(gene_ppp$y))/2 
bandwidth <- 0.9*min(sigma, iqr)*gene_ppp$n^(-1/5)
gene_intensity <- density.ppp(gene_ppp, sigma = bandwidth)
plot(gene_intensity,   main = "Плотность gene, экз/кв.м")
points(gene_ppp, pch = 19, cex = 0.6)

# выделим коды возрастов:
for_relrisk_example <- ppp_object # создадим копию ppp объекта
marks(for_relrisk_example) <- ppp_object$marks$age

# вычислим вероятность появления особей возраста pre:
p <- relrisk(for_relrisk_example, 0.5) 
plot(p, main = "Доля группы pre", col = colorRampPalette(
        c("antiquewhite", "aquamarine3","navyblue"))(100))

# добавим изолинии:
contour(p, nlevels = 5, lwd = seq(from = 0.1, to = 3, length.out = 5), add = T)


#-----------------------------------------------------------------------	
#   К разделу  9.3.  Использование сервисов картографической системы Google Maps
#-----------------------------------------------------------------------	

library(googleVis)

# Создадим таблицу с данными:
Dikero <- data.frame(Coords = c("52.2:30.6", "53.1:30.1", "53.7:30.3"),
                     Location = c("д. Холмеч", "г. Рогачев", "д. Стайки"))

Map <- gvisMap(Dikero, "Coords", "Location",
               Options = list(showTip=TRUE, mapType='normal',
                              enableScrollWheel = TRUE),
               chartid = "Dikerogammarus")
plot(Map)
print(Map, tag = "chart")

# Создание интерактивной веб-графики:
data(Fruits)
Fruits

M <- gvisMotionChart(Fruits, idvar = "Fruit", timevar = "Year")
print(M, tag = "chart")

#-----------------------------------------------------------------------	
#   К разделу  9.4.	Создание картограмм при помощи R
#-----------------------------------------------------------------------	

library(maptools)

# Загрузка шейп-файла в формате R:
load(url("http://biogeo.ucdavis.edu/data/gadm2/R/BLR_adm1.RData"))
Regions <- gadm
rm(gadm)

slotNames(Regions)
Regions@data
str(Regions@polygons)
Regions@data$NAME_1

# Вывод на экран картограмм по областям респ. Беларусь:
spplot(Regions,
       "NAME_1", # отображаемая переменная
       scales = list(draw = T), # отображение координатных осей
       col.regions = rainbow(n = 6) ) # опция, задающая заливку цветом

# Численность мужчин по областям респ. Беларусь
# Порядок чисел важен! Он должен совпадать с порядком,
# в котором в таблице data перечислены соответствующие области:
Regions@data$Population = c(186716, 152169, 131817, 105417, 253427, 135348)
mypalette <- colorRampPalette(c("seagreen", "whitesmoke"))

# Объект mypalette - это функция:
mypalette
spplot(Regions, "Population",
       col.regions = mypalette(20), # определение цветовой шкалы
       col = "transparent", # отключение контурных линий на карте
       par.settings = list(axis.line = list(col = NA)))

# Вывод на экран картограмм по районам респ. Беларусь:
load(url("http://biogeo.ucdavis.edu/data/gadm2/R/BLR_adm2.RData"))
Counties <- gadm

# Используем генератор случайных чисел:
set.seed(1234) # для воспроизводимости результата
Counties@data$Fake <- rnorm(dim(Counties@data)[1], 100, 35)

require(classInt)
brks.eq = classIntervals(Counties$Fake, n = 6, style = "equal")
brks.eq
plotcol <- brewer.pal(6,"RdBu")

# Рисуем карту:
spplot(Counties, "Fake", col.regions = plotcol,
       at = brks.eq$brks, # задает границы классов
       par.settings = list(axis.line = list(col = NA)))

library(ggplot2); library(rgeos); library(maptools)
counties <- fortify(Counties, region = "NAME_2")
str(counties)

fake_data <- as.data.frame(Counties@data)
fake_data$Value <- rnorm(nrow(fake_data))

# Теперь отобразим данные из столбца Value таблицы fake_data на картограмме:
ggplot() + geom_map(data = fake_data,
                    aes(map_id = NAME_2, fill = Value), 
                    map = counties) + 
        expand_limits(x = counties$long, y = counties$lat) +
        coord_map("polyconic")

# В другой цветовой шкале:
library(scales) # для функции muted (см. ниже)
ggplot() + geom_map(data = fake_data,
                    aes(map_id = NAME_2, fill = Value),
                    colour = "gray",
                    map = counties) + 
        expand_limits(x = counties$long, y = counties$lat) +
        scale_fill_gradient2(low = muted("blue"), 
                             midpoint = 0,
                             mid = "white",
                             high = muted("red"),
                             limits = c(min(fake_data$Value),
                                        max(fake_data$Value))) +
        coord_map("polyconic")
