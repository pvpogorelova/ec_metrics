# Установка пакетов
install.packages("tidyverse")
install.packages("fpp3")
install.packages("lubridate")
install.packages("rio")
install.packages("ggplot2")
install.packages("patchwork")

# Подключение библиотек
library(tidyverse) # обработка данных
library(fpp3) # все и сразу для рядов
library(lubridate) # все и сразу для рядов
library(rio) # импорт данных
library(ggplot2) # графики
library(patchwork) # склейка графиков

setwd('/Users/polinapogorelova/Desktop/АВР') # установка рабочей директории

# Задание 1. Тестирование автокорреляции
us_change

# Оценим регрессию потребления на доход
model = lm(Consumption ~ Income, data = us_change)


# Тест Дарбина-Уотсона
# H0: нет автокорреляции 1-го порядка
library(car)
durbinWatsonTest(model)

# Тест Бройша-Годфри
# H0: нет автокорреляции порядка p
library(lmtest)
bgtest(Consumption ~ Income, data = us_change, order = 3)

library(tidyverse) # обработка данных
library(fpp3) # куча плюшек для рядов

# создаём ряд с нуля!
n_obs = 100
set.seed(777)
ts = tsibble(date = yearmonth(ymd('2010-01-01') + months(0:(n_obs - 1))),
              wn = rnorm(n_obs, mean = 10, sd = 2), # процесс белого шума: y_t = eps_t
              iid = rnorm(n_obs, mean = 0, sd = 4), # процесс независимых наблюдений: y_t = mu + eps_t
              rwalk = 10 + cumsum(rnorm(n_obs, mean = 0, sd = 1)), # процесс случайного блуждания (Random Walk) с дрейфом
              index = date)
ts
p1 = autoplot(ts, wn)
p2 = autoplot(ts, iid)
p3 = autoplot(ts, rwalk)

(p1 + p2) / p3 # изобразим на одном графике

gg_season(ts, rwalk)
gg_subseries(ts, rwalk)

# зависимость текущего значени от лагированных
gg_lag(ts, rwalk)
gg_lag(ts, iid)

# несколько графиков сразу
gg_tsdisplay(ts, rwalk, plot_type = 'season')

# Задание 2. MA(q)-процессы

n_obs = 2000

# MA(1): x_t = u_t + 0.9*u_(t-1)
# MA(2): y_t = u_t - 0.6*u_(t-1) + 0.8*u_(t-2)
data_ma = tibble(x = 50 + arima.sim(n = n_obs, model = list(ma = 0.9), sd = 10))
data_ma = tibble(x = arima.sim(n = n_obs, model = list(ma = 0.9)),
                 y = arima.sim(n = n_obs, model = list(ma = c(-0.6, 0.8))))

data_ma$date = yearweek( ymd('1980-01-01') + weeks(0:(n_obs-1)))
data_ma = as_tsibble(data_ma, index = date)
data_ma %>% autoplot(x)

p1 = data_ma %>% autoplot(x) +
  labs(title = "Процесс MA(1)")

p2 = data_ma %>% autoplot(y) +
  labs(title = "Процесс MA(2)")

p1 / p2

gg_tsdisplay(data_ma, x, plot_type = 'partial')
gg_tsdisplay(data_ma, y, plot_type = 'partial')

# Задание 3. AR(p)-процессы
n_obs = 20

set.seed(1000)

# x: AR(1): x_t = 4 + 0.5*x_(t-1) + u_t, u_t ~ WN(0, 4)
# y: AR(2): y_t = 0.5*y_(t-1) + 0.06*y_(t-2) + u_t, u_t ~ WN(0, 1)

data_ar = tibble(x = 4 + arima.sim(n = n_obs,
                                   model = list(ar = 0.5, sd = 2)),
                 y = arima.sim(n = n_obs,
                               model = list(ar = c(0.5, 0.06))))

date = seq(as.Date("1996-01-01"), as.Date("2020-12-31"), by = "1 quarter")
data_ar$date = yearquarter(date)

data_ar = as_tsibble(data_ar, index = date)

gg_tsdisplay(data_ar, x, plot_type = 'partial')
gg_tsdisplay(data_ar, y, plot_type = 'partial')
