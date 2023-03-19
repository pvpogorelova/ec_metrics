# Семинар 24.
# ETS-модели.

library(tidyverse) # обработка данных
library(fpp3) # работа с рядами
library(lubridate) # работа с рядами
library(rio) # импорт данных
library(ggrepel) # симпатичные подписи
library(ggplot2) # графики
library(patchwork) # расположение графиков

setwd('/Users/polinapogorelova/Desktop/АВР') # установка рабочей директории

# Задание 1. Модели экспоненциального сглаживания.
unemp = import("unemployments.csv")

unemp = mutate(unemp,
               date = yearmonth(date))

unemp = as_tsibble(unemp, index = date,
                       key = c('code', 'name'))


unemp_rf = filter(unemp, code == 643)

unemp_rf %>% autoplot()


unemp_rf = filter(unemp_rf,
                  date < ymd('2020-04-01'))

unemp_rf = mutate(unemp_rf,
                  total = total/1000)


unemp_rf %>% gg_tsdisplay(total, plot_type = "partial")


unemp_train = filter(unemp_rf,
                     date < ymd('2018-04-01'))

unemp_test =  filter(unemp_rf,
                     (date >= ymd('2018-04-01')))


mod1 = model(unemp_train,
                    snaive = SNAIVE(total),
                    ets_ann = ETS(total ~ error('A') + trend('N') + season('N')),
                    ets_aan = ETS(total ~ error('A') + trend('A') + season('N')),
                    ets_ana = ETS(total ~ error('A') + trend('N') + season('A')),
                    ets_aaa = ETS(total ~ error('A') + trend('A') + season('A')))

mod1

report(select(mod1, ets_aan))
report(select(mod1, ets_aaa))


fcst1 = forecast(mod1,
                 unemp_test)

accuracy(fcst1, unemp_rf) %>%
         select(.model, RMSE, MAE, MAPE, MASE) %>%
         arrange(RMSE)

autoplot(fcst1,
  filter(unemp_rf, year(date) > 2015)) +
  labs(title = "Прогноз числа безработных в РФ (тыс.чел.)", y = "")

comp = model(unemp_train,
             ets_ana = ETS(total ~ error('A') + trend('N') + season('A'))) %>% components()

comp
tail(comp)

# Задание 2. Создание моделей на базе существующих
# Преобразование данных

# Исходный ряд
p1 = unemp_rf %>%
     autoplot(total)

# Логарифмическое преобразование
p2 = unemp_rf %>%
     autoplot(log(total))

# Преобразование Бокса-Кокса
lambda = unemp_rf %>%
    features(total, features = guerrero) %>%
    pull(lambda_guerrero)

lambda

unemp_rf %>%
    autoplot(box_cox(total, lambda))

p3 = unemp_rf %>%
  autoplot(box_cox(total, lambda))

p1 / (p2 + p3) + plot_annotation(title = "Исходный и преобразованные ряды безработицы в РФ" )

mod2 = model(unemp_train,
                    snaive = SNAIVE(total),
                    ets_aaa = ETS(total ~ error('A') + trend('A') + season('A')),
                    ets_ana = ETS(total ~ error('A') + trend('N') + season('A')),
                    ets_aaa_bc = ETS(box_cox(total, lambda) ~ error('A') + trend('A') + season('A')),
                    ets_ana_bc = ETS(box_cox(total, lambda) ~ error('A') + trend('N') + season('A')))

mod2

fcst2 = forecast(mod2, unemp_test)

autoplot(fcst2,
         filter(unemp_rf, year(date) > 2015))

accuracy(fcst2, unemp_rf) %>%
        select(.model, RMSE, MAE, MAPE) %>%
        arrange(RMSE)

# Усреднение моделей
mod3 = mutate(mod2,
              top2 = (ets_ana + ets_ana_bc) / 2,
              top3 = (ets_ana + ets_ana_bc + snaive) / 3)

fcst3 = forecast(mod3, h = '2 years')
fcst3

accuracy(fcst3, unemp_rf) %>%
       select(.model, RMSE, MAE, MAPE) %>%
       arrange(RMSE)


# Задание 3. Кросс-валидация
# Кросс-валидация скользящим окном: наблюдений много и мы подозреваем, что зависимость изменяется
unemp_slide = slide_tsibble(unemp_rf,
                            .size = 120, .step = 2)
unemp_slide

mod4 = model(unemp_slide,
             snaive = SNAIVE(total),
             ets_aaa = ETS(total ~ error('A') + trend('A') + season('A')),
             ets_ana = ETS(total ~ error('A') + trend('N') + season('A')),
             ets_aaa_bc = ETS(box_cox(total, lambda) ~ error('A') + trend('A') + season('A')),
             ets_ana_bc = ETS(box_cox(total, lambda) ~ error('A') + trend('N') + season('A'))
)

mod4

fcst4 = forecast(mod4, h = 1)

accuracy(fcst4, unemp_rf)  %>%
  arrange(RMSE)

# Кросс-валидация растущим окном: наблюдений мало и мы подозреваем, что зависимость меняется во времени
unemp_stretch = stretch_tsibble(unemp_rf,
                               .init = 120, .step = 2)

unemp_stretch

mod5 = model(unemp_stretch,
                    snaive = SNAIVE(total),
                    ets_aaa = ETS(total ~ error('A') + trend('A') + season('A')),
                    ets_ana = ETS(total ~ error('A') + trend('N') + season('A')),
                    ets_aaa_bc = ETS(box_cox(total, lambda) ~ error('A') + trend('A') + season('A')),
                    ets_ana_bc = ETS(box_cox(total, lambda) ~ error('A') + trend('N') + season('A'))
)

mod5

fcst5 = forecast(mod5, h = 1)

accuracy(fcst5, unemp_rf)  %>%
  arrange(RMSE)
