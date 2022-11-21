# Семинар 10: Способы борьбы с мультиколлинеарностью: Ridge, Lasso, PCA.

library(tidyverse) # манипуляции с данными и построение графиков
library(sjPlot) # красивые графики для линейных моделей
library(rio) # импорт/экспорт данных
library(car) # тестирование гипотез
library(lmtest) # тестирование гипотез
library(ggfortify)
library(Hmisc) # данные cars
library(tidymodels) # плюшки для моделей
library(corrplot) # картинки для корреляционной матрицы
library(skimr) # описательные статистики
library(glmnet)  # LASSO + ridge
library(car)  # vif
library(factoextra) # симпатичные графики для метода главных компонент

setwd('/Users/polinapogorelova/Desktop/Метрика_ИП') # установка рабочей директории

# Ridge и LASSO
library("glmnet") # Ridge, Lasso

data = cars
plot(cars$speed, cars$dist) # связь нелинейная

# Так как визуально связь между dist и speed нелинейная, добавим квадрат и куб скорости
data['speed2'] = data['speed'] ^ 2
data['speed3'] = data['speed'] ^ 3

y_unscaled = as.matrix(data['dist']) # сохраним исходную зависимую переменную в матрицу y_unscaled

data = scale(data) # стандартизируем все данные (среднее = 0, стандартное отклонение = 1)

# Определим стандартизированную зависимую переменную dist в вектор y, а стандартизированные объясняющие переменные - в матрицу X
X = as.matrix(data[, c(1, 3, 4)])
y = as.matrix(data[, 2])
data
X
y

# Оценим LASSO при lambda = 0.1
# Параметр alpha = 1, если оценвиается LASSO и 0, если оценивается Ridge
model_lasso = glmnet(X, y_unscaled, alpha = 1, lambda =0.1,
               standardize = FALSE,
               standardize.response = FALSE, intercept = TRUE)
coef(model_lasso) # оценки коэффициентов модели

# Теперь посмотрим как изменятся оценки коэффициентов, перебирая разные lambda
lambdas = c(0.0001, 0.001, 0.01, 0.1, 1) # зададим набор разных lambda
lambdas
model_lasso = glmnet(X, y_unscaled, alpha=1, lambda=lambdas,
                     standardize=FALSE,
                     standardize.response=FALSE, intercept=TRUE)
model_lasso$lambda
model_lasso$beta # оценки коэффициентов при разных lambda

# В угловых решениях некоторые оценки коэффициентов в точности оказываются равными нулю, т.е. LASSO без всякой проверки гипотез в каком-то смысле
# делит коэффициенты на “значимые” и “незначимые”. Оценка LASSO всегда есть, даже в случае жесткой мультиколлинеарности или если регрессоров больше чем, наблюдений.

plot(model_lasso, xvar = "lambda", label = TRUE) # график зависимсоти оценок коэффициентов от ln(lambda)

# Теперь попробуем подобрать оптимальное lambda, исползуя алгоритм кросс-валидации
cv_lambda = cv.glmnet(X,y_unscaled,alpha = 1,
                      standardize = FALSE,
                      standardize.response = FALSE, intercept = TRUE,
                      nfolds = 10)
cv_lambda # оптимальное lambda

# Теперь получим оценки коэффициентов lasso, используя оптимальную lambda
model_lasso_opt = glmnet(X,y_unscaled,alpha = 1, lambda=cv_lambda$lambda.min,
                          standardize=FALSE,
                          standardize.response=FALSE, intercept=TRUE)
coef(model_lasso_opt)


# Ridge-regression
# Оценим ридж-регрессию при lambda=0.1
model_rr = glmnet(X, y_unscaled, alpha = 0, lambda = 0.1,
                     standardize = FALSE,
                     standardize.response = FALSE, intercept = TRUE)
coef(model_rr) # оценки коэффициентов модели

# Теперь посмотрим как изменятся оценки коэффициентов, перебирая разные lambda
lambdas = c(0.0001, 0.001, 0.01, 0.1, 1) # зададим набор разных lambda
lambdas
model_rr = glmnet(X, y_unscaled, alpha = 0, lambda = lambdas,
                     standardize = FALSE,
                     standardize.response = FALSE, intercept = TRUE)
coef(model_rr) # оценки коэффициентов при разных lambda
plot(model_rr, xvar = "lambda", label = TRUE) # график зависимсоти оценок коэффициентов от ln(lambda)


# Подберем оптимальную lambda для ridge регрессии
cv_lambda_rr = cv.glmnet(X,y_unscaled,alpha = 0,
                      standardize = FALSE,
                      standardize.response = FALSE, intercept = TRUE, nfolds = 10)
cv_lambda_rr

# Оценим модель для оптимального lambda
model_rr_opt = glmnet(X,y_unscaled,alpha = 0, lambda = cv_lambda$lambda.min,
                         standardize = FALSE,
                         standardize.response = FALSE, intercept = TRUE)
coef(model_rr_opt)


# Попробуем реализовать МГК на данных dataflats
data = import("/Users/polinapogorelova/Desktop/Метрика_ИП/Данные/dataflats.xlsx") # импорт данных
head(data)
data = mutate(data, price_sq = price/totsp) # добавим переменную price_sq
y = data$price_sq
d = data[2:6]
d

cor(d) # корреляционная матрица количественных регрессоров

# Метод главных компонент с предварительной стандартизацией переменных
d.pca = prcomp(d, scale = TRUE)

# извлечем первую главную компоненту:
pca1 = d.pca$x[, 1]
head(pca1)

# извлечем веса, с которыми переменные входят в первую главную компоненту:
v1 = d.pca$rotation[, 1]
v1

# выборочная дисперсия каждой компоненты:
summary(d.pca) # например, первые три компоненты имеют суммарную выборочную дисперсию равную 90% от суммарной выборочный дисперсии
# стоимости квадратного метра

# а вот первая главная компонента, отвечающая за планировку квартиры, слабо дифференцирует квартиры по стоимости квадратного метра
cor(y, pca1)

# выборочная дисперсия каждой компоненты на графике:
fviz_eig(d.pca) # более симпатичная версия вместо plot(d.pca)

# исходный набор данных в новых осях по горизонтали — pc1 по вертикали — pc2
fviz_pca_biplot(d.pca, repel = TRUE) # более симпатичная версия вместо biplot(h.pca, xlim = c(-1, 1))

# оценим регрессию цены квадратного метра на главные компоненты
model = lm(y ~ d.pca$x[, 1] + d.pca$x[, 2] + d.pca$x[, 3] + d.pca$x[, 4] + d.pca$x[, 5] + data$walk)
summary(model)

# посмотрим, с какими весами входят исходные признаки в незначимую главную компоненту
v4 = d.pca$rotation[, 4]
v4

