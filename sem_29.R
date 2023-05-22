# Семинар 29: бутстрэп и квантильная регрессия
library(tidyverse) # манипуляции с данными
library(skimr) # описательные статистики
library(tidymodels) # удобства для оценивания моделей
library(sjPlot) # визуализация коэффициентов
library(lmtest) # тесты для моделей
library(rio) # import / export всех форматов
library(boot) # бутстрэп
library(lmboot) # бутстрэп коэффициентов
library(car) # ещё немного бутстрэпа для регрессии


#################################################################################
# Решение задачи 2 из семинара
# данные
X = c(1,3,5,7)
Y = c(2,4,6,8)
Z = cbind(Y, X)

# уровень значимости
alpha = 0.05

# объем выборки
n = length(X)

# число параметров в модели
k = 1

# число репликаций
R = 10000

# пункт a)
b_BOOT = matrix(0, nrow = k, ncol = R)

set.seed(777) # На удачу!!!
for(r in 1:R){
  Z_boot = Z[sample(n, replace = TRUE), ]
  Y_boot = Z_boot[, 1]
  X_boot = Z_boot[, -1]
  model = lm(Y_boot ~ 0 + X_boot)
  b_BOOT[, r] = coef(model)
}
mean_b_boot = mean(b_BOOT)
mean_b_boot

# пункт b)
CI_b_boot = matrix(0, nrow = k, ncol = 2)
for(k in 1:k){
  CI_b_boot[k, 1] = quantile(b_BOOT[k, ], alpha/2)
  CI_b_boot[k, 2] = quantile(b_BOOT[k, ], 1 - alpha/2)
}
CI_b_boot


#################################################################################
# Задача 1. Использование bootstrap для МНК-оценок регрессии
# Оценим модель с помощью МНК
d = cars
mod_ols = lm(cars, formula = 'dist ~ speed')
summary(mod_ols)
confint(mod_ols)

y_hat = fitted(mod_ols) # сохраним модельные значения = x*beta_ols
res_ols = residuals(mod_ols) # сохраним остатки модели
hist(res_ols) # построим гистограмму остатков модели

library(tseries)
shapiro.test(res_ols) # протестируем остатки на нормальность

sigma = sqrt(deviance(mod_ols)/df.residual(mod_ols)) # сохраним стандартную ошибку sigma_epsilon
sigma


# Реализуем парный бутстрэп
set.seed(777)
pairs_boot = paired.boot(data = d, dist ~ speed, B = 10000)
pairs_boot$bootEstParam
beta_speed = pairs_boot$bootEstParam[,2]
beta_speed

quantile(beta_speed, probs = c(0.025,0.975))

# ДИ для отношения коэффициентов (или любой функции от параметров)
# Так как выборка маленькая, то дельта-метод неприменим.
# На помощь приходит бутстрэп!

# функция, задающая функцию от параметров, для которой хотим построить ДИ
get_ratio = function(model){
  result = coef(model)[2]/coef(model)[1]
  return(result)
}

get_ratio(mod_ols) # проверяем работоспособность функции

# Чтобы строить ДИ для функций от параметров, используем команду Boot из пакета car
paired_boot = Boot(mod_ols, f = get_ratio, R = 10000)
summary(paired_boot)
# построим наивный бутстрэповский ДИ
confint(paired_boot, type = 'basic')

# Реализуем параметрический бутстрэп
# функция для оценивания регрессии
get_boot_coef = function(df, indices, y_hat, sigma) {
  df['y_star'] = y_hat + rnorm(nrow(df), mean = 0, sd = sigma)

  boot_ols = lm(data = df, formula = 'y_star ~ speed') # оценивание моделей с помощью МНК
  boot_coef = coef(boot_ols) # сохраняем коэффициенты
  boot_se = sqrt(diag(vcov(boot_ols))) # сохраняем стандартные ошибки оценок коэффициентов
  boot_t = (coef(boot_ols)-coef(mod_ols))/sqrt(diag(vcov(boot_ols))) # считаем t-статистику

  return(c(boot_coef, boot_se, boot_t))
}


# Зададим число репликаций равным 1000 и оценим модель
boot_coefs = boot(cars, get_boot_coef, R = 10000,
                  y_hat = y_hat,
                  sigma = sigma)

# гистограмма оценок коэффициента наклона
coef_slop = boot_coefs$t[,2]
coef_slop
hist(coef_slop) # гистограмма
abline(v = c(quantile(coef_slop, probs = 0.025),
             quantile(coef_slop, probs = 0.975)), col = 'red', lty = 'dashed') # добавим границы 95% доверительного интервала

quantile(coef_slop, probs = c(0.025, 0.975)) # выведем границы 95% ДИ для коэффициента наклона в модели регрессии

# bias = difference bootstrap mean vs original
boot.ci(boot_coefs, conf = 0.95, type = 'basic', index = 2)


#################################################################################
# Задача 2. Квантильная регрессия
# медианная и квантильная регрессия

# импортируем данные из .txt файла
f = import("/Users/polinapogorelova/Downloads/flats_moscow.txt")
glimpse(f)  # бросим взгляд чтобы убедиться, что загрузка прошла успешно

# оценим линейную модель с помощью МНК
model_ols = lm(data = f, price ~ totsp + dist + metrdist + walk + brick)
summary(model_ols)
confint(model_ols)

# базовый график — диаграмма рассеяния
base = qplot(data = f, totsp, price)
base

# добавляем к базовому графику две линии квантильной регрессии
base_q = base + stat_smooth(method = "rq", method.args = list(tau = 0.1), se = FALSE) +
  stat_smooth(method = "rq", method.args = list(tau = 0.9), se = FALSE)
base_q

# добавляем к графику разделение в зависимости от того, кирпичный дом или нет
base_q + aes(colour = factor(brick))

# квантильная регрессия для квантилей 0.1, 0.5 (медианная), 0.9
model_q = rq(data = f, price ~ totsp + dist + metrdist + walk + brick, tau = c(0.1, 0.5, 0.9))
summary(model_q, se = "boot") # отчет по модели
plot(model_q)


model = formula(price ~ totsp)
coefsf = function(data,ind){
  rq(model, data = f[ind,])$coef
}

boot.mod = boot(f,coefsf,R = 10000)
myboot.ci = list()
for (i in 1:ncol(boot.mod$t)){
  myboot.ci[[i]] = boot.ci(boot.mod, level = .95, type =
                             c("norm","basic"), index = i)
}
myboot.ci[[2]]
