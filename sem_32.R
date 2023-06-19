# Семинар 32: Пространственная эконометрика
# Установка пакетов для анализа пространственных данных
install.packages("spdep")
install.packages("rgdal")
install.packages("rgeos")

# Подключение библиотек
library(spdep) #load library spdep, make weights matrix (nb type)
library(rgdal) # reading Shapefile (географические объекты)
library(rgeos)
library(spatialreg) # SLX model

# Установка рабочей директории
setwd("/Users/polinapogorelova/Downloads/R_Spatial_Regression1")

# Импорт пространственных данных
spat.data = readOGR(dsn = ".", layer = "NCVACO") # dsn-директория
names(spat.data) # названия переменных
summary(spat.data) # описательные статистики

# Карты для визуального анализа
spplot(spat.data, "DUI1802") # построим карту для переменной DUI1802
spplot(spat.data, "SALESPC") # построим карту для переменной SALESPC
spplot(spat.data, "COLLENRP") # построим карту для переменной COLLENRP
spplot(spat.data, "BKGRTOABC") # построим карту для переменной BKGRTOABC
spplot(spat.data, "BAPTISTSP") # построим карту для переменной BAPTISTSP
spplot(spat.data, "BKGRTOMIX") # построим карту для переменной BKGRTOMIX
spplot(spat.data, "BAPTISTSP") # построим карту для переменной BAPTISTSP
spplot(spat.data, "ENTRECP") # построим карту для переменной ENTRECP


# Построение весовой матрицы (наличие общей границы)
queen.nb = poly2nb(spat.data) # зададим весовую матрицу
queen.listw = nb2listw(queen.nb) # конверируем nb в listw
listw1 = queen.listw # сохраним весовую матрицу

# Зададим спецификацию модели регрессии
model_spec = DUI1802 ~ SALESPC + COLLENRP + BKGRTOABC + BAPTISTSP + BKGRTOMIX + ENTRECP

# Оценим 4 модели: обычная регрессия, SLX, SAR, SEM

# 1. Обычная модель множественной регрессии
reg = lm(model_spec, data = spat.data)
summary(reg)

# Рассчитаем глобальный индекс I Морана и проверим его значимость
lm.morantest(reg, listw1)

# Тестирование необходимости учета пространственной корреляции с помощью LM-теста
lm.LMtests(reg, listw1, test = c("LMerr", "LMlag"))
lm.LMtests(reg, listw1, test = c("RLMerr", "RLMlag"))

# p = rho, T = theta, L = lambda
# 2. SLX Spatially Lagged X y=Xß+WXT+e
reg_slx = lmSLX(model_spec, data = spat.data, listw = listw1, Durbin = TRUE)
summary(reg_slx) # результаты оценивания SLX
impacts(reg_slx, listw = listw1) # рассчитаем предельные эффекты

# 3. SAR Spatial Lag (Autoregressive) Model y=pWy+XB+e
reg_sar = lagsarlm(model_spec, data = spat.data, listw1)
summary(reg_sar) # результаты оценивания SAR
impacts(reg_sar, listw = listw1) # расчет предельных эффектов для SAR (в SAR модели преедлные эффекты выше оценок коэффициентов!)

# 4. SEM Spatial Error Model  y=XB+u,   u=LWu+e
reg_sem = errorsarlm(model_spec, data = spat.data, listw = listw1)
summary(reg_sem) # результаты оценивания SEM

# Spatial Hausman Test: SEM vs. OLS
Hausman.test(reg_sem)
