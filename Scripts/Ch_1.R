#########################################################################

#  Мастицкий С.Э., Шитиков В.К. (2014) Статистический анализ и визуализация 
#  данных с помощь R. (Адрес доступа: http://r-analytics.blogspot.com)

#########################################################################

#########################################################################
# Глава 1.	ОСНОВНЫЕ КОМПОНЕНТЫ СТАТИСТИЧЕСКОЙ СРЕДЫ R
#########################################################################

# Раздел 1.2
#-----------------------------------------------------------------------	
load(url("http://biogeo.ucdavis.edu/data/gadm2/R/BLR_adm1.RData"))

Regions <- gadm
rm(gadm)

Regions@data$NAME_1 <- as.factor(c("Брест","Гомель","Гродно","Могилев",
                                   "Минск", "Витебск"))
library(sp)
spplot(Regions, "NAME_1",
       col.regions = terrain.colors(6), 
       scales = list(draw = T))

#-----------------------------------------------------------------------	
# Раздел 1.3
#-----------------------------------------------------------------------	

# library(Rcmdr)
# Commander()
# Выполним из консоли команды, подготовленные R Commander:
molluscs <- read.table("http://figshare.com/media/download/98923/97987",
                       header=TRUE, sep="\t", na.strings="NA",
                       dec=".", strip.white=TRUE)
cor.test(molluscs$CAnumber, molluscs$ZMlength,
         alternative="two.sided", method="pearson") 

library(car)
scatterplot(CAnumber~ ZMlength | Lake, reg.line=lm, smooth=TRUE, spread=TRUE, 
            boxplots='xy', span=0.5, ylab="Численность инфузорий",
            xlab="Длина раковины", by.groups=FALSE, data=molluscs)

#-----------------------------------------------------------------------	
# Раздел 1.4
#-----------------------------------------------------------------------	

library(help=Matrix)
# Список установленных пакетов:
library()
installed.packages(priority = "base")
installed.packages(priority = "recommended")
packlist <- rownames(installed.packages())

# Вывод информации в буфер обмена в формате для Excel:
write.table(packlist,"clipboard",sep="\t", col.names=NA)

# Функция, инсталлирующая и загружающая пакеты:
instant_pkgs <- function(pkgs) { 
	pkgs_miss <- pkgs[which(!pkgs %in% installed.packages()[, 1])]
	# Инсталлируем пакеты, не подготовленные к загрузке:
	if (length(pkgs_miss) > 0) {
	install.packages(pkgs_miss)
	}
	# Загружаем пакеты, которые еще не загружены:
	attached <- search()
	attached_pkgs <- attached[grepl("package", attached)]
	need_to_attach <- pkgs[which(!pkgs %in% gsub("package:", "", attached_pkgs))]
	if (length(need_to_attach) > 0) {
	   for (i in 1:length(need_to_attach))
	require(need_to_attach[i], character.only = TRUE)
	}
}

# Пример вызова:
instant_pkgs(c("base", "jpeg", "vegan"))

#  Функции, методы, устройства:
library(vegan)
sessionInfo()
ls(pos = "package:vegan")

#  Аргументы функции:
args(lm)
lm
predict
help(Devices)
methods("predict")

#  Список устройств:
help(Devices)
