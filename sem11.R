library(rio) # импорт и экспорт данных в разных форматах
library(dplyr) # манипуляции с данными
library(lmtest) # тест Бройша-Пагана
library(sandwich) # оценка дисперсии при гетероскедастичности
library(UStatBookABSC) # WLS
library(estimatr) # получение робастных оценок
library(ggpubr) # для графиков
library(skimr) # для описательных статистик


flats = import("/Users/polinapogorelova/Desktop/Метрика_ИП/Данные/dataflats.xlsx")
flats = mutate(flats, price_sq = price/totsp)

# Рассмотрим описательные статистики загруженного датасета
skim(flats)

# Проверим наличие гетероскедастичности визуально
# Построим графики зависимости цены квартир от объясняющих факторов.
kit = ggplot(flats) + geom_point(aes(x = kitsp, y = price)) +
  labs(x = "Площадь кухни, кв.м", y = "Цена квартиры, $1000",
       title = "Стоимость квартир в Москве")
live = ggplot(flats) + geom_point(aes(x = livesp, y = price)) +
  labs(x = "Жилая площадь, кв.м", y = "Цена квартиры, $1000",
       title = "Стоимость квартир в Москве")
dist = ggplot(flats) + geom_point(aes(x = dist, y = price)) +
  labs(x = "Расстояние до центра, м", y = "Цена квартиры, $1000",
       title = "Стоимость квартир в Москве")
metrdist = ggplot(flats) + geom_point(aes(x = metrdist, y = price)) +
  labs(x = "Расстояние до метро, м", y = "Цена квартиры, $1000",
       title = "Стоимость квартир в Москве")
ggarrange(kit, live, dist, metrdist, ncol = 2, nrow = 2) # из графиков видно, что возможно гетероскедастичность присутствует. В частности, подозрительны переменные `livesp` и `kitsp`.

# Построим простую линейную регрессионную модель, на которой будем проверять гетероскедастичность
reg = lm(price ~ 1 + livesp + kitsp + dist + metrdist, data = flats)
summary(reg)

# Сохраним прогнозные значения
flats$price_pred = fitted(reg)
# Сохраним остатки модели
flats$resid = residuals(reg)
# и создадим переменную resid2 = resid^2
flats$resid2 = resid^2

# Построим график зависимости остатков модели от прогнозных значений
ggplot(flats) + geom_point(aes(x = price_pred, y = resid)) +
  labs(x = "Остатки модели", y = "Прогноз цены квартиры, $1000",
       title = "Остатки-прогноз")

# Проверим наличие гетероскедастичности с помощью тестов.

# Начнём с теста Уайта.
# Реализуем данный тест вручнуюxw
# Построим регрессию квадрата остатков на объясняющие переменные, их квадраты и их попарные произведения
reg_white = lm(resid2 ~ 1 + (livesp + kitsp + dist + metrdist)^2 + I(livesp ^ 2) + I(kitsp ^ 2) + I(dist ^ 2) + I(metrdist ^ 2), data = flats)
summary(reg_white) # так как в целом модель значима (p-value=0 < 0.05), то гипотеза H1 не отвергается, то есть есть гетероскедастичность

# либо через встроенную команду:
bptest(reg, varformula = ~ (livesp + kitsp + dist + metrdist)^2 + I(livesp ^ 2) + I(kitsp ^ 2) + I(dist ^ 2) + I(metrdist ^ 2),
       data = flats) # здесь надо смотреть тоже на p-value. Так как p-value=0 < 0.05, то гипотеза H1 не отвергается, то есть есть гетероскедастичность


# Проведём тест Бройша - Пагана.
# Классическая версия Бройша - Пагана реализуется в R по команде:
bptest(reg, studentize = FALSE)

# Модифицированная версия теста Бройша - Пагана реализуется по команде:
bptest(reg)

# Причем, если отдельно не указать спецификацию вспомогательной регрессии, то `bptest()` возьмёт все регрессоры исходной модели.
# В обеих версиях теста Бройша - Пагана гетероскедастичность обнаружена.

# Тест Голдфелда - Квандта.
# Предположим, что дисперсии случайных ошибок растут с ростом площади кухни, `kitsp`.

flats_ordered = arrange(flats, kitsp)
reg_gqtest = lm(price ~ livesp + kitsp + dist + metrdist,
                data = flats_ordered)
gqtest(reg_gqtest, fraction = 0.3) # посередине отсортированного ряда лежит 30% наблюдений


# Будет также полезным познакомиться с методами борьбы с гетероскедастичностью.
# Способ 1. Взвешенный МНК.
reg_wls = lm(data = flats,
             price ~ livesp + kitsp + dist + metrdist,
             weights = flats$kitsp)
summary(reg_wls)

reg_w = lm(abs(residuals(reg)) ~ kitsp, data = flats)
summary(reg_w)
fitted(reg_w)

reg_wls = lm(data = flats,
             price ~ livesp + kitsp + dist + metrdist,
             weights = fitted(lm(abs(residuals(reg)) ~ kitsp)) ^ 2)
summary(reg_wls)

# Способ 2. Робастные ошибки в форме Уайта.
reg_hc = lm_robust(data = flats,
                   price ~ livesp + kitsp + dist + metrdist, se_type = "HC0")
summary(reg_hc)

# Робастные оценки коэффициентов регрессии получаются состоятельными.
