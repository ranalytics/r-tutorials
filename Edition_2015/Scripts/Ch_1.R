
#  Мастицкий С.Э., Шитиков В.К. (2015) Статистический анализ и визуализация 
#  данных с помощь R. Купить книгу можно здесь: http://bit.ly/1QopZ0A


#########################################################################
#         Глава 1. ОСНОВНЫЕ КОМПОНЕНТЫ СТАТИСТИЧЕСКОЙ СРЕДЫ R
#########################################################################


# ------------------------- Раздел 1.1 --------------------------------

# Пример содержимого служебного файла Rprofile.site
# Все, что следует за символом комментария "#", средой игнорируется
# options(papersize="a4")
# options(editor="notepad")
# options(pager="internal")
# установить тип отображения справочной информации
# options(help_type="text")
options(help_type="html")
# установить место расположения локальной библиотеки
# .Library.site <- fi le.path(chartr("\\", "/", R.home()), "site-library")
# При загрузке среды запустить меню R Commander
# Поставить знаки #, если запуск Rcmdr не нужен
local({
        old <- getOption("defaultPackages")
        options(defaultPackages = c(old, "Rcmdr"))
})
# Определить зеркало CRAN
local({r <- getOption("repos")
       r["CRAN"] <- "http://cran.gis-lab"
       options(repos = r)})
# Определить путь к рабочему каталогу (любой иной на вашем компьютере)
setwd("D:/R/Process/Resampling")


# --------------------------- Раздел 1.2 -------------------------------

# Код к рис. 2:
load(url("http://biogeo.ucdavis.edu/data/gadm2/R/BLR_adm1.RData"))

Regions <- gadm
rm(gadm)

Regions@data$NAME_1 <- as.factor(c("Брест","Гомель","Гродно","Могилев",
                                   "Минск", "Витебск"))
library(sp)
spplot(Regions, "NAME_1",
       col.regions = rainbow(6), scales = list(draw = T))

	
# ----------------------------- Раздел 1.3 -----------------------------	

# library(Rcmdr)
# Commander()
# Выполним из консоли команды, подготовленные R Commander:
mussel <- read.table("http://bit.ly/1wMGDOQ", header = TRUE, sep = "\t",
                     na.strings = "NA", dec = ".", strip.white = TRUE)
cor.test(mussel$CAnumber, mussel$ZMlength,
         alternative = "two.sided", method = "pearson")
library(car)
scatterplot(CAnumber ~ ZMlength | Lake, reg.line = lm,
            smooth = TRUE, spread = TRUE, boxplots = "xy", span = 0.5,
            ylab = "Численность инфузорий",
            xlab = "Длина раковины",
            by.groups = FALSE, data = mussel)


# ---------------------------- Раздел 1.4 ------------------------------	

library(help = Matrix)
# Список установленных пакетов:
library()
installed.packages(priority = "base")
installed.packages(priority = "recommended")
packlist <- rownames(installed.packages())

# Пример вывода информации в буфер обмена (например, для дальнейшей вставки в Excel):
write.table(packlist, "clipboard", sep="\t", col.names = NA)

# Функция, автоматически инсталлирующая и загружающая необходимые пакеты
# (оригинал здесь: http://bit.ly/1TIyJxO):
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
