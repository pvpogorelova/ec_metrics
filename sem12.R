library(memisc) # две и более регрессий в одной табличке
library(skimr) # описательные статистики (вместо psych в видеолекциях)
library(lmtest) # тестирование гипотез в линейных моделях
library(rio) # загрузка данных в разных форматах (вместо foreign в видеолекциях)
library(vcd) # мозаичные графики
library(hexbin) # графики
library(sjPlot) # визуализация результатов регрессии
library(tidyverse) # графики и манипуляции с данными, подключаются пакеты dplyr, ggplot2, etc

# загружаем данные по стоимости квартир в Москве

flats = import("/Users/polinapogorelova/Desktop/Метрика_ИП/Данные/dataflats.xlsx")
flats = mutate(flats, price_sq = price/totsp)

glimpse(flats)  # краткое содержимое таблички flats

# Построим графики зависимости цены квартир от объясняющих факторов.
kit = ggplot(flats) + geom_point(aes(x = kitsp, y = price)) +
  labs(x = "Площадь кухни, кв.м", y = "Цена 1 кв.м. квартиры, $1000",
       title = "Стоимость квартир в Москве")
live = ggplot(flats) + geom_point(aes(x = livesp, y = price)) +
  labs(x = "Жилая площадь, кв.м", y = "Цена 1 кв.м. квартиры, $1000",
       title = "Стоимость квартир в Москве")
dist = ggplot(flats) + geom_point(aes(x = dist, y = price)) +
  labs(x = "Расстояние до центра, м", y = "Цена 1 кв.м. квартиры, $1000",
       title = "Стоимость квартир в Москве")
metrdist = ggplot(flats) + geom_point(aes(x = metrdist, y = price)) +
  labs(x = "Расстояние до метро, м", y = "Цена 1 кв.м. квартиры, $1000",
       title = "Стоимость квартир в Москве")
ggarrange(kit, live, dist, metrdist, ncol = 2, nrow = 2) # из графиков видно, что возможно гетероскедастичность присутствует. В частности, подозрительны переменные `livesp` и `kitsp`.

# преобразуем дамми переменные walk, brick, floor в факторные
flats = mutate_at(flats, vars(walk, brick, floor), factor)
glimpse(flats)  # краткое содержимое таблички flats

# оценим три модели
model_1 = lm(data = flats, log(price) ~ log(totsp) + brick)
model_2 = lm(data = flats, log(price) ~ log(totsp) + brick + log(dist))
model_3 = lm(data = flats, log(price) ~ log(totsp) + brick + brick:log(totsp) + log(dist)) # двоеточие эквивалентно умножению переменных
AIC(model_1)
AIC(model_2)
AIC(model_3)
# выведем результаты оценивания трех моделей в виде единой таблицы
mtable(model_1, model_2, model_3)

# проведем тест Рамсея для каждой из трех моделей
# H0: спецификация верная (нет пропущенных переменных)
# H1: спецификация неверная (есть пропущенные переменные)
resettest(model_1) # на уровне значимости 5% гипотеза H0 не отвергается
resettest(model_2) # на уровне значимости 5% гипотеза H0 не отвергается
resettest(model_3) # на уровне значимости 5% гипотеза H0 не отвергается


flats = mutate(flats, log_metrdist_w = log(metrdist)*as.numeric(walk),
               log_metrdist_nw = log(metrdist)*(1-as.numeric(walk)))

reg = lm_robust(data = flats, log(price) ~ log(totsp) + brick + log(dist) + log_metrdist_w + log_metrdist_nw, se_type = "HC0")
summary(reg)

# тест Рамсея
resettest(reg)
