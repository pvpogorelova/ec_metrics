library(rio) # импорт и экспорт данных в разных форматах
library(dplyr) # манипуляции с данными
library(lmtest) # тест Бройша-Пагана
library(sandwich) # оценка дисперсии при гетероскедастичности
library(UStatBookABSC) # WLS
library(estimatr) # получение робастных оценок
library(ggpubr) # для графиков
library(skimr) # для описательных статистик
library(MASS)
library(car)

# сгенерируем данные
set.seed(1)
n = 100
d = data.frame(x1 = rnorm(n, 100, 5),
               x2 = rnorm(n, 20, 3),
               eps = rnorm(n, 0, 0.5))

d = mutate(d, y = 2 + 0.7*x1 - 0.3*x2 + eps)


# 1. Метод Зарембки: выбор между линейной и полулогарифмической моделями
# H0: качество подгонки линейной и полулогарифмической моделей одинаковое
# H1: модель с меньшей RSS лучше
d$lny = log(d$y)
gy = exp(mean(d$lny))
gy # геометрическое среднее

d$y_new = d$y/gy
d$kn_y_new = log(d$y_new)

model1 = lm(y_new ~ x1 + x2, data = d)
model2 = lm(log(y_new) ~ x1 + x2, data = d)

xi_2 = n/2*abs(log(deviance(model1)/deviance(model2)))
xi_2
xi_crit = qchisq(.95, df=1)
xi_crit # так как наблюдаемое значение статистики меньше критического, то на 5% уровне мы не можем отвергнуть гипотезу
# H0, то есть модели имеют одинаковое качество подгонки
# Если бы гипотеза H0 отверглась, то выбор модели производился бы на основе RSS
deviance(model1) # RSS для линейной модели
deviance(model2) # RSS для лог-линейной модели


# 2. PE тест (тест для сравнения линейной и линейной в логарифмах моделей)
# можно сравнивать невложенные модели (в моделях есть набор общих регрессоров + в каждой дополнительно свои регрессоры)
# можно тестировать линейную модель против полулогарифмической, а не только против линейной в логарифмах
reg1 = lm(y ~ x1 + x2, data = d)
d$ypred = predict(reg1)

reg2 = lm(lny ~ x1 + x2, data = d)
d$ln_ypred = predict(reg2)

d$add1 = d$ln_ypred - log(d$ypred)
d$add2 = d$ypred - exp(d$ln_ypred)

reg1_add = lm(y ~ x1 + x2 + add1, data = d)
summary(reg1_add)

reg2_add = lm(lny ~ x1 + x2 + add2, data = d)
summary(reg2_add)
# Вывод: так как обе доп. переменные оказались незначимы, то не можем сделать выбор между моделями

# либо автоматическая команда:
petest(reg1, reg2) # сравнение линейной и лог-линейной моделей

# Тестирование нормальности остатков модели с помощью теста Харке-Бера
# H0: остатки имеют нормальное распределение
# H1: распределение остатков отлично от нормального

install.packages('tseries') # устанавливаем необходимый пакет
library(tseries) # подключаем библиотеку

jarque.bera.test(resid(reg1)) # так как p-value > 0.05, то гипотеза H0 не отвергается на любом разумном уровне значимости,
# то есть остатки линейной модели можно считать нормальными
#fit linear regression model


jarque.bera.test(resid(reg2)) # так как p-value > 0.05, то гипотеза H0 не отвергается на любом разумном уровне значимости,
# то есть остатки линейной модели можно считать нормальными
#fit linear regression model



################################
# Преобразование Бокса-Кокса
model = lm(y ~ x1 + x2, data = d)
help(boxcox)
#find optimal lambda for Box-Cox transformation
bc = boxcox(model)

summary(powerTransform(model))  # оптимальное значение lambda из пакета car и LR-тест

# можно оценить регрессию на преобразованных данных, используя оптимальное значение lambda
new_model = lm(((y^0.7962-1)/0.7962) ~ x1 + x2, data = d)
summary(new_model)
