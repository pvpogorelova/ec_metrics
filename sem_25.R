# Семинар 25. ARDL, коинтеграция
library(tidyverse) # обработка данных
library(fpp3) # куча плюшек для рядов
library(rio) # импорт данных
library(urca) # тесты
library(lmtest) # тесты (включая тест на причинность)
library(patchwork)
library(rvest) # импорт данных
library(dLagM) # adl
library(ARDL) # ardl

# Часть 1. ADL/ARDL модели
# Набор данных "seaLevelTempSOI" состоит из следующих  ежемесячных рядов с июля 1880 года по май 2013 года:
# GMSL - глобальный средний уровень моря
# LandOcean - аномалии средней температуры суши и океана по GISS, NASA
# SOI - индекс южных колебаний (SOI показывает отклонение разности атмосферного давления от среднего между островом Пасхи (или островом Таити) и городом Дарвин в Австралии)
# Ряды GMSL и температурных аномалий сглажены и скорректированы с учетом сезонных колебаний.

data(seaLevelTempSOI)
d = seaLevelTempSOI

# добавим переменную date
n_obs = nrow(d)
d = mutate(d, date = yearmonth(ymd('1880-07-01') + months(0:(n_obs-1))))
d = as_tsibble(d, index = date)

# Построим графики рядов GMSL и LandOcean
p1 = autoplot(d, GMSL)
p2 = autoplot(d, LandOcean)
p1 / p2

# Протестируем ряды на стационарность с помощью ADF теста
# H0: ряд нестационарен
# H1: ряд стационарен
# Если ADF < DF_cr, то H0 отвергается
summary(ur.df(d$GMSL, type = "drift"))
summary(ur.df(d$LandOcean, type = "drift"))

# Тест на причинность по Грейнджеру (не причинно-следственная связь!!!)
# H0: LandOcean не является причиной по Грейнджеру для GMSL
# H1: LandOcean является причиной по Грейнджеру для GMSL
grangertest(GMSL ~ LandOcean, order = 3, data = d)
grangertest(GMSL ~ LandOcean, order = 1, data = d)

# H0: GMSL не является причиной по Грейнджеру для LandOcean
# H1: GMSL является причиной по Грейнджеру для LandOcean
grangertest(LandOcean ~ GMSL, order = 3, data = d)
grangertest(LandOcean ~ GMSL, order = 2, data = d)

# Модель полиномиальных лагов (метод Алмон)
model.poly = polyDlm(x = d$LandOcean, y = d$GMSL,
                     q = 4 , k = 2 , show.beta = TRUE) # q - количество лагов x в исходной модели; k - степень полинома.
summary(model.poly) # результаты оценивания модели
residuals(model.poly) # ряд остатков
fitted(model.poly) #ряд модельных значений переменной y (GMSL)

# Модель геометрических лагов (метод Койка)
model.koyck = koyckDlm(x = d$LandOcean,
                       y = d$GMSL)
summary(model.koyck, diagnostics = TRUE) # результаты оценивания и связь с лекцией (слайды 12-14):
                                         # phi - это параметр lambda в лекции,
                                         # alpha - это параметр delta в лекции,
                                         # beta - это параметр beta в лекции.
residuals(model.koyck)
fitted(model.koyck)

# ARDL-модель
ardl_11 = ardl(GMSL ~ LandOcean, data = d, order = c(1,1)) # в order указываем количесттво лагов для зависимой (GSML) и объясняющей переменной (LandOcean)
summary(ardl_11)

mult_ardl_sr = multipliers(ardl_11, type = "sr") # действительно ли это мгновенный эффект изменения?
mult_ardl_sr

mult_ardl_lr = multipliers(ardl_11, type = "lr") # долгосрочный эффект
mult_ardl_lr

# Представим модель ARDL(1,1) в виде ECM
uecm_11 = uecm(ardl_11)
summary(uecm_11)

mult_uecm_sr = multipliers(uecm_11, type = "sr")
mult_uecm_sr

mult_uecm_lr = multipliers(uecm_11, type = "lr")
mult_uecm_lr

all.equal(mult_ardl_sr, mult_uecm_sr)
all.equal(mult_ardl_lr, mult_uecm_lr)

# Автоматический подбор параметров ARDL-модели
auto = auto_ardl(GMSL ~ LandOcean, data = d, selection = "AIC", max_order = c(3,3))
summary(auto$best_model)
auto$top_orders

# Часть 2. Коинтеграция
# Пример на сгенерированных данных (пример из учебника Энгла и Грейнджера)
set.seed(1000) # для воспроизводимости результатов

# Зададим объем выборки и два параметра для генерации коинтегрированных рядов
n_obs = 10000
a = -0.4
b = 1.5

# X_t + bY_t = u_t, где u_t = u_(t-1) + eps_1t - нестационарный процесс случайного блуждания, то есть u(t) ~ I(1)
# X_t + aY_t = v_t, где v_t = rho*v_(t-1) + eps_2t, |rho|<1 - стационарный AR(1) процесс, то есть v(t) ~ I(0)
# eps_1t и eps_2t - некоррелированные между собой белые шумы

data = tsibble(date = yearmonth(ymd('2013-01-01') + months(0:(n_obs - 1))),
               u = cumsum(rnorm(n_obs, mean = 0, sd = 1)), # случайное блуждание
               v = arima.sim(list(order = c(1,0,0), ar = .7), n = n_obs), # стационарный AR(1) процесс
               index = date)

data = mutate(data, x = a/(a-b)*u - b/(a-b)*v, # I(1)
                    y = -1/(a-b)*u + 1/(a-b)*v) # I(1)

p1 = autoplot(data, x)
p2 = autoplot(data, y)
p1 / p2


# Что будет, если попробовать оценить параметры a с помощью МНК?
model = lm(x ~ 0 + y, data = data) # оценка коэффициента a!
summary(model)

# Основной вывод: если оба ряда I(1), но коинтегрированы, то оценка МНК коэффициентов регрессии становится состоятельной.
# И можно, несмотря на нестационарность переменных, применять регрессионные методы.
plot.ts(model$residuals)
summary(ur.df(model$residuals, type = "drift")) # стационарный ряд остатков X_t - 0.394*Y_t - коинтегрирующее соотношение
