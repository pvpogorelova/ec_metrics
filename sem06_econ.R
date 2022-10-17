install.packages("tidyverse")
install.packages("sjPlot")
install.packages("car")
install.packages("rio")
install.packages("lmtest")

library(tidyverse) # манипуляции с данными и построение графиков
library(sjPlot) # красивые графики для линейных моделей
library(rio) # импорт/экспорт данных
library(car) # тестирование гипотез
library(lmtest) # тестирование гипотез

# Задание 5.

# Импортируем данные.
data = import("/Users/polinapogorelova/Desktop/Метрика_ИП/Данные/dataflats.xlsx")

# Исследуем датасет и ценах на квартиры в Москве.
head(data)

# Переименуем столбцы с «неговорящими» названиями
data = rename(data, n = A, price = B)

# Сначала посмотрим на описательные статистики переменных.
summary(data) # для всех переменных сразу
summary(data$totsp) # для отдельной переменной
mean(data$totsp)
sd(data$totsp)

# И уберем строчки, в которых хотя бы один элемент пустой.
data = na.omit(data)

# Оценим параметры модели для стоимости 1 кв м квартиры. Сначала создадим такую переменную.
data = mutate(data, price_sq = price/totsp)

# Оценим первую модель, включающую две объясняющие переменные, и проинтерпретируем коэффициенты
ols_1 = lm(price_sq ~ livesp + dist, data = data)
summary(ols_1)

# Оценка ковариационной матрицы оценок коэффициентов модели регрессии ols_1
vcov(ols_1)

# Теперь оценим модель, в которую добавим еще одну переменню в правую часть - metrdist
ols_2 = lm(price_sq ~ livesp + dist + metrdist, data = data)
summary(ols_2)


# Оценка ковариационной матрицы оценок коэффициентов модели регрессии ols_2
vcov(ols_2)

# Получим прогнозные значения для модели ценообразования 1 кв м квартиры в Москве
data = mutate(data, yhat = fitted(ols_1))

coeftest(ols_1)
coeftest(ols_2)

# Теперь построим 90%-й доверительный интервал для параметров моделей
confint(ols_1, level = 0.9)
confint(ols_2, level = 0.9)

# Проверка гипотезы о незначимости моделей в целом
linearHypothesis(ols_1, c("livesp=0", "dist=0"))
linearHypothesis(ols_2, c("livesp=0", "metrdist=0", "dist=0"))

# Проверка линейных ограничений на коэффициенты
# Проверим гипотезу о равенстве коэффициента при регрессоре dist -0.1.
linearHypothesis(ols_1, c("dist = -0.1"))
linearHypothesis(ols_2, c("dist = -0.1"))

# А теперь проверим гипотезу, о том, что жилая площадь и расстояние до метро меняют стоимость 1 кв м на одно и то же число ден.ед., но с противоположным знаком.
linearHypothesis(ols_2, "1*livesp + 1*metrdist = 0")

# Регрессия для стандартизированных показателей
data_scale <- data %>%
              mutate_all(~(scale(.)))

ols2_scale <- lm(price_sq ~ livesp + dist + metrdist, data = data_scale)
summary(ols2_scale)

# Тестирование гипотез с односторонней альтернативой
# Оценка ковариационной матрицы оценок коэффициентов модели регрессии
# H0: beta_dist = -0.1
# H1: beta_dist > -0.1
t = (-0.0448135+0.1)/ 0.0022775
t
# Рассчитаем наблюдаемое значение t-статистики
# t = (beta_j - beta_j^)/se(beta_j) ~ t(n-k)
# Рассчитаем критическое значение
# Квантиль уровня alpha распределения Стьюдента с n-k степенями свободы
# t_cr = qt(alpha,n-k)
n = nrow(data)
t_cr = qt(0.95,n-4)
t_cr

if (t > t_cr) {
  print("Гипотеза H0 отвергается в пользу альтернативной на заданном уровне значимости")
} else {
  print("Гипотеза H0 не отвергается на заданном уровне значимости")
}

# Теперь выведем прогноз модели по 10 новым наблюдениям (которые сами же и придумаем) и построим для него 90%-й доверительный интервал
# Будем считать, что новые наблюдения распределены нормально с заданными параметрами

set.seed(7)
new_data <- tibble(livesp = rnorm(10, mean = 30, sd = 4),
                  dist = rnorm(10, mean = 8, sd = 2),
                  metrdist = rnorm(10, mean = 5, sd = 2))

help(predict.lm)
yhat = predict(ols_2, newdata = new_data, se = TRUE, interval= "predict", level = .90)
yhat$fit

par(mfrow = c(2,2))
plot(ols_2)






