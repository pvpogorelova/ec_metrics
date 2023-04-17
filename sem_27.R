# Семинар 27.
# Устанавливаем пакеты
install.packages("mfx")
install.packages("AUC")
install.packages("Hmisc")
install.packages("MASS")
install.packages('erer')
install.packages("polr")

# Подключаем библиотеки
library("mfx") # подсчет предельных эффектов для бинарных моделей
library("erer") # подсчет предельных эффектов для моделей упорядоченного выбора
library("ggplot2") # графики
library("lmtest") # линейные регрессии
library("foreign") # чтение файлов в некоторых форматах
library("dplyr") # манипуляции с таблицами
library("broom") # описание модели в виде таблички
library("AUC")  # для ROC кривой
library("Hmisc")
library("MASS") # модели упорядоченного выбора
library("car")

# Рассмотрим данные о мнении женщин относительно франы "Место женщины дома, а не на работе"
role = read.dta("/Users/polinapogorelova/Downloads/mesto_jenshini.dta")
glimpse(role) # просмотр данных

# Преобразуем данные в формат tibble и выведем для них описательные статистикм
tidy(role)[c("column", "n", "mean", "median", "min", "max", "sd")]

# Повторение - мать учения!
# Вспомним бинарные модели

# Построим столбчатую диаграмму
ggplot(role) + geom_bar(aes(factor(agree))) + scale_x_discrete(breaks = c("1", "0"),
                                                               labels = c("Да", "Нет")) + labs(x = "Согласны ли с утверждением?", y = "Количество женщин",
                                                                                               title = "Результаты опроса")

# Оценим logit-модель
m_logit = glm(agree ~ age + adjinc + nsibs, data = role, family = binomial(link = "logit"))
summary(m_logit)

# Оценим probit-модель
m_probit = glm(agree ~ age + adjinc + nsibs, data = role, family = binomial(link = "probit"))
summary(m_probit)

# Предельные эффекты, посчитанные по logit- и probit-модели
logitmfx(agree ~ age + adjinc + nsibs, data = role)
probitmfx(agree ~ age + adjinc + nsibs, data = role)

# Добавим два новых наблюдения
new_data = data_frame(age = c(20, 24), adjinc = c(16000, 4000), nsibs = c(2, 5))
new_data

# В логит и пробит-моделях можно прогнозировать значение скрытой переменной (склонности ответить “да”) или вероятность yi=1 (вероятность ответить “да”).
# Прогноз скрытой переменной:
augment(m_logit, newdata = new_data)
augment(m_probit, newdata = new_data)

# Прогнозы вероятности yi=1 можно получить с помощью команды:
augment(m_logit, newdata = new_data, type.predict = "response")
augment(m_probit, newdata = new_data, type.predict = "response")

# Выбор между вложенными моделями
# Likelihood ratio, LR-test, Тест отношения правдоподобия
m2_logit = glm(agree ~ age + age2 + adjinc + nsibs, data = role, family = binomial(link = "logit"))
lrtest(m_logit, m2_logit)

# Поскольку чаще всего нужен доверительный интервал именно для вероятностей, мы построим только его.
# Если нужен доверительный интервал для ожидаемой склонности ответить да, то достаточно просто убрать опцию prediction.type:
new2 = augment(m_logit, newdata = new_data, type.predict = "response")
z_cr = qnorm(0.975)
new2 = mutate(new2,  ci_left = .fitted - z_cr * .se.fit, ci_right = .fitted + z_cr * .se.fit)
new2

# спрогнозируем скрытую переменную для исходного набора данных
pred_role = predict(m_logit, role, se = TRUE)
# соединим прогнозы с исходным набором данных
role = cbind(role, pred_role)

# применим логистическую функцию распределения, чтобы получить вероятности
role = mutate(role, prob = plogis(fit))
# глянем выборочно на результат:
select(role, age, agree, prob)

# Упорядоченная logit-модель
ftable(xtabs(~ agree + tradrole, data = role)) # таблица сопряженности для факторов agree и tradrole

# Преобразуем зависимую порядковую переменную tradrole в факторную
role$tradrole = as.factor(role$tradrole)

# Оценим упорядоченную logit-модель
help(polr)
m_ologit = polr(tradrole ~ age + adjinc + nsibs + urb + fpro, data = role, Hess = TRUE)

# Взглянем на результаты оценивания модели
summary(m_ologit)

# Преобразуем вид таблицы с результатами
ctable = coef(summary(m_ologit))
ctable

## Рассчитаем p-value и сохраним полученные значения
p = pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2

## Добавим столбец p-value в исходную таблицу с оценками коэффициентов модели
ctable = cbind(ctable, "p value" = p)
ctable

# Доверительные интервалы для коэффициентов beta в упорядоченной logit-модели
ci = confint(m_ologit)
ci

# Предельные эффекты для объясняющих переменных
me_ologit = ocME(w = m_ologit)
me_ologit$out

# Посчитаем отношения шансов
exp(coef(m_ologit))

# Пересчитаем границы ДИ для отношений шансов
exp(cbind(OR = coef(m_ologit), ci))

# Тест Бранта для проверки гипотезы о пропорциональности шансов
# H0: шансы пропорциональны beta_k = beta
poTest(m_ologit)

# Добавим четыре новых наблюдения
newdat = data.frame(age = rep(17:20, 1), urb = 1, fpro = 1, adjinc = 10, nsibs = 2)
newdat

# Построим для новых наблюдений прогноз вероятностей принять каждое значение для переменной tradrole
newdat = cbind(newdat, predict(m_ologit, newdat, type = "probs"))
newdat
