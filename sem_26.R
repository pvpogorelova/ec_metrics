# Семинар 26. Модели бинарного выбора.

# подключаем пакеты
library(tidyverse) # графики и манипуляции с данными, подключаются пакеты dplyr, ggplot2, etc
library(rio) # импорт файлов разных форматов
library(mfx) # расчет предельных эффектов
library(vcd) # графики для качественных данных
library(reshape2) # манипуляции с данными
library(skimr) # описательные статистики
library(AUC) # для ROC кривой

# при загрузке файлов R автоматиечски переделывает все строковые переменные в факторные
# команда ниже просит R так не делать :)
options(stringsAsFactors = FALSE)

# читаем данные по женщинам
t = import("/Users/polinapogorelova/Downloads/mroz.dta")

# смотрим на набор данных
glimpse(t)

# объясняем R, какие переменные считать факторными
t = mutate_at(t, vars(city,inlf), factor)
# смотрим на набор данных
glimpse(t)

# мозаичный график - для качественных переменных
mosaic(data = t, ~inlf + city, shade = TRUE)

# 'ящик с усами'
qplot(data = t, x = inlf, y = age, geom = "boxplot")

# два варианта сглаженной функции плотности
ggplot(data = t, aes(x = age, y = ..count.., fill = inlf)) + geom_density(position = "stack")
ggplot(data = t, aes(x = age, y = ..count.., fill = inlf)) + geom_density(position = "fill")

# Оценивание логит и пробит моделей
m_logit = glm(data = t, inlf ~ educ + exper + expersq + age + kidslt6 + kidsge6 + nwifeinc, family = binomial(link = "logit"),
               x = TRUE)
m_probit = glm(data = t, inlf ~ educ + exper + expersq + age + kidslt6 + kidsge6 + nwifeinc, family = binomial(link = "probit"),
                x = TRUE)
# отчеты об оценке моделей
summary(m_logit)
summary(m_probit)

# оценка ковариационной матрицы оценок коэффициентов
vcov(m_logit)

# создаём новый массив данных для прогнозирования
newdata = data.frame(age = seq(from = 30, to = 40, length = 100),
                    educ = 10, exper = 4, expersq = 16, kidslt6 = 1, kidsge6 = 0, nwifeinc = 16)
# посмотрим на начало этой таблички
head(newdata)

# прогнозируем по логит модели
pr_logit = predict(m_logit, newdata, se = TRUE)
# соединим прогнозы и новый массив данных в единую табличку:
newdata_pr = cbind(newdata, pr_logit)
head(newdata_pr)  # глянем на начало таблички

# применив логистическую функцию распределения получим границы доверительного интервала
newdata_pr = mutate(newdata_pr, prob = plogis(fit), left_ci = plogis(fit - 1.96 *
                                                                        se.fit), right_ci = plogis(fit + 1.96 * se.fit))
head(newdata_pr)  # глянем на результат

# посмотрим на графике как меняется доверительный интервал для вероятности
qplot(data = newdata_pr, x = age, y = prob, geom = "line") + geom_ribbon(aes(ymin = left_ci,
                                                                             ymax = right_ci), alpha = 0.2)

# проведем LR тест R при построении разных моделей автоматом использует
# максимальное количество полных наблюдений поэтому часто выходит, что
# ограниченная и неограниченная модель оцениваются на разном наборе данных но в
# таком случае их нельзя сравнивать с помощью LR теста поэтому мы сначала
# создадим набор данных t2 без пропущенных значений и на нем оценим короткую и
# длинную модели H0: beta(kidslt6)=0, beta(kidsge6)=0
t2 = dplyr::select(t, inlf, educ, exper, expersq, age, kidslt6, kidsge6, nwifeinc) %>% na.omit()
# если команда select не работает, возможно подключен пакет, переопределяющий команду select
# чтобы решить проблему, вместо select() явно пишем dplyr::select()

# оцениваем ограниченную модель
m_logit2 = glm(data = t2, inlf ~ educ + exper + expersq + age +  nwifeinc,
                family = binomial(link = "logit"), x = TRUE)
# проводим LR тест
lrtest(m_logit, m_logit2)


# предельные эффекты для среднестатистического пассажира
logitmfx(data = t, inlf ~ educ + exper + expersq + age + kidslt6 + kidsge6 + nwifeinc)

# усредненные предельные эффекты по всем пассажирам
logitmfx(data = t, inlf ~ educ + exper + expersq + age + kidslt6 + kidsge6 + nwifeinc, atmean = FALSE)

# обычный МНК
m_ols = lm(data = t, as.numeric(inlf) ~ educ + exper + expersq + age + kidslt6 + kidsge6 + nwifeinc)
summary(m_ols)

# прогнозы по обычному МНК
pr_ols = predict(m_ols, newdata)
head(pr_ols)

# ROC кривая спрогнозируем скрытую переменную для исходного набора данных
pr_t = predict(m_logit, t, se = TRUE)
# соединим прогнозы с исходным набором данных
t = cbind(t, pr_t)
# применим логистическую функцию распределения, чтобы получить вероятности
t = mutate(t, prob = plogis(fit))
# глянем выборочно на результат:
dplyr::select(t, inlf, prob)

# получим все данные для ROC кривой:
roc.data = roc(t$prob, t$inlf)
str(roc.data)

# три графика для выбора порога отсечения по горизонтали — пороги, по вертикали — чувствительность  = число выживших
# верно предсказанных выжившими / общее количество выживших
qplot(x = roc.data$cutoffs, y = roc.data$tpr, geom = "line")

# по горизонтали — пороги, по вертикали — процент ложноположительных
qplot(x = roc.data$cutoffs, y = roc.data$fpr, geom = "line")

# ROC по горизонтали — процент ложноположительных прогнозов по вертикали ---
# чувствительность
qplot(x = roc.data$fpr, y = roc.data$tpr, geom = "line")


# Оптимальный порог отсечения
rocdf = data.frame(cutoff = roc.data$cutoffs, tpr = roc.data$tpr, spec = 1-roc.data$fpr)

# изобразим на одном графике зависимость чувствительности и специфичности от порогового значения
ggplot(rocdf, aes(cutoff)) +
  geom_line(aes(y = tpr, colour = "TPR")) +
  geom_line(aes(y = spec, colour = "1-FPR"))

# можно искать порог, при котором чувствительность равна специфичности
# найдем значение порога, соответствующее точке пересечения
cut_off = which.min(abs(rocdf$tpr - rocdf$spec))
opt_cut = roc.data$cutoffs[cut_off]

# спрогнозируем переменную inlf
t = mutate(t, pred = case_when(prob > opt_cut ~ 1, prob < opt_cut ~ 0))
# матрица сопряженности
table(t$inlf, t$pred)
