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
library(rvest) # сбор данных
library(urca) # тесты на единичный корень

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
bgtest(Consumption ~ Income, data = us_change, order = 4)


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

n_obs = 500

# MA(1): x_t = u_t + 0.9*u_(t-1)
# MA(2): y_t = u_t - 0.6*u_(t-1) + 0.8*u_(t-2)
help(arima.sim)
data_ma = tibble(x = arima.sim(n = n_obs, model = list(ma = 0.9)),
                 y = arima.sim(n = n_obs, model = list(ma = c(-0.6, 0.8))))

data_ma$date = yearweek( ymd('1980-01-01') + weeks(0:(n_obs-1)))
data_ma = as_tsibble(data_ma, index = date)
data_ma %>% autoplot(x)

p1 = data_ma %>% autoplot(x) +
  labs(title = "Процесс MA(1)")

p2 = data_ma %>% autoplot(y) +
  labs(title = "Процесс MA(2)")

(p1 + p1) / (p2 + p2)

gg_tsdisplay(data_ma, x, plot_type = 'partial')
gg_tsdisplay(data_ma, y, plot_type = 'partial')

# Задание 3. AR(p)-процессы
n_obs = 50

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

# Задание 4. ARMA(p,q)-процессы
# x: ARMA(1,1): x_t = 0.7*x_(t-1) + u_t - 0.4*u_(t-1), u_t ~ WN(0, 1)
# y: ARMA(2,1): y_t = 0.6*y_(t-1) + 0.3*y_(t-2) + u_t + 0.7*u_(t-1), u_t ~ WN(0, 1)

data_arma = tibble(x = arima.sim(n = n_obs,
                                 model = list(ar = 0.7, ma = -0.4)),
                   y = arima.sim(n = n_obs,
                                 model = list(ar = c(0.6, 0.3), ma = 0.7)))

data_arma$date = yearmonth(ymd('2010-01-01') + months(0:(n_obs-1)))
data_arma = as_tsibble(data_arma, index = date)

gg_tsdisplay(data_arma, x, plot_type = 'partial')
gg_tsdisplay(data_arma, y, plot_type = 'partial')


train = filter(data_arma, year(date) < 2017)
test = filter(data_arma, year(date) >= 2017)


models = model(train,
               naive = NAIVE(x),
               ar1 = ARIMA(x ~ pdq(1,0,0) + PDQ(0,0,0)),
               ar2 = ARIMA(x ~ pdq(2,0,0) + PDQ(0,0,0)),
               arma = ARIMA(x ~ pdq(1,0,1:2) + PDQ(0,0,0))
)

models

report(models$arma[[1]])

fcst = forecast(models, test)
autoplot(fcst) +
  autolayer(data_arma, x)

# Задание 5. Пример на ежемесячных данных о среднедушевом потреблении
d = import('/Users/polinapogorelova/Desktop/АВР/income.xlsx')
d = mutate(d, date = yearmonth(ymd('1994-01-01') + months(0:263)) )
d = as_tsibble(d, index = date)

d %>% autoplot(income)

d = filter(d, year(date) > 2004 )
p1 = d %>% autoplot(income)
p2 = d %>% autoplot(log(income))

# Box-Cox
lambda = d %>%
  features(income, features = guerrero) %>%
  pull(lambda_guerrero)

lambda # при lambda = 0 получаем логарифмическое преобразование

d = mutate(d, income_bc = box_cox(income, lambda))

p3 = autoplot(d, income_bc)

p1 / p2 / p3

# ADF с трендом
# H0: ts = ARIMA(p, 1, q) + quadratic trend (нестационарный ряд - DS)
# Ha: ts = ARIMA(p, 0, q) + trend (стационарный ряд после удаление тренда - TS)
summary(ur.df(d$income, type = '',
              selectlags = 'AIC'))  # H0 не отвергается на 5% уровне значимости


summary(ur.df(d$income_bc, type = 'trend',
              selectlags = 'AIC'))  # H0 не отвергается на 5% уровне значимости

d = mutate(d, t = seq(1,132))
d = mutate(d, t2 = t^2)

reg = d %>%
  model(TSLM(income ~ trend()))
report(reg)

reg = d %>%
  model(TSLM(income_bc ~ t + t2 ))
res = residuals(reg)
res2 = res$.resid
d = mutate(d, res = res2)
d %>% autoplot(res)

summary(ur.df(d$res, type = 'none',
              selectlags = 'AIC'))  # H0 не отвергается на 5% уровне значимости

gg_tsdisplay(d,res2, plot_type = "partial", lag_max = 50)

# Разделим данные на train и test
train = filter(d, year(date) < 2015)
test = filter(d, year(date) >= 2015)


models = model(train,
               naive = NAIVE(res2),
              # model1 = ARIMA(res2 ~ pdq(1,0,1) + PDQ(1,0,0)),
               auto = ARIMA(res2)
)

models

report(models$model1[[1]])
report(models$auto[[1]])

fcst = forecast(models, test)

accuracy(fcst,d) %>%
  arrange(MAE)

autoplot(fcst) +
  autolayer(d,res2)


# Задание 6. Индекс цен производителей промышленных товаров
url = 'http://sophist.hse.ru/hse/1/tables/PPI_EA_M.htm'
xml_tree = read_html(url)

p = html_table(xml_tree)
p1 = html_table(xml_tree)[[1]]
colnames(p1) = c('year', 'PPI')
p2 = tail(p1, -2)
p3 = head(p2, -4)
p4 = mutate(p3, PPI = gsub(",", ".", PPI) , date = yearmonth(ymd('1997-01-01') + months(0:287)))
p5 = select(p4, date, PPI)
p6 = mutate(p5, PPI = as.numeric(PPI))

ppi = as_tsibble(p6, index = date)
autoplot(ppi, PPI)

gg_tsdisplay(ppi, PPI, plot_type = 'partial')

#3. ADF с константой
# H0: ts = ARIMA(p, 1, q) + trend (нестационарный ряд)
# Ha: ts = ARIMA(p, 0, q) + const (стационарный ряд)
summary(ur.df(ppi$PPI, type = 'drift',
              selectlags = 'AIC'))  # H0 не отвергается на 5% уровне значимости


train = filter(ppi, year(date) < 2019)
test = filter(ppi, year(date) >= 2019)


models = model(train,
               naive = NAIVE(PPI),
               ma1 = ARIMA(PPI ~ pdq(0,0,1) + PDQ(0,0,0)),
               ar2 = ARIMA(PPI ~ pdq(2,0,0) + PDQ(0,0,0)),
               arma21 = ARIMA(PPI ~ pdq(1,0,1) + PDQ(0,0,0)),
               auto = ARIMA(PPI)
)

models

report(models$ma1[[1]])
report(models$arma21[[1]])
report(models$auto[[1]])

fcst = forecast(models, test)

accuracy(models) %>%
  arrange(MAE)

autoplot(fcst, filter(ppi, year(date) > 2010)) +
  autolayer(ppi, PPI)


