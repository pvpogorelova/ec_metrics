clear
set more off
cd /Users/polinapogorelova/Desktop/СМЭФ_Эконометрика/Эконометрика_22_23
log using sem5.log

use CARD.DTA
describe // описание набора данных
summarize // описательные статистики переменных

* 1. Метод наименьших квадратов
reg lwage educ exper expersq black south smsa smsa66 reg662 reg663 reg664 reg665 reg666 reg667 reg668 reg669
est store ols
estat ovtest // RESET-test  (тест Рамсея). Ho: нет пропущенных переменных
predict yhat, xb // сохраняем прогноз по модели
tabstat yhat, statistics(mean) by(south) // среднее значение прогноза по двум категориям бинарной переменной south (tabstat отображает сводную статистику для ряда числовых переменных в одной таблице)
adjust exper = 10, by(south) // команда adjust отображает среднее модельное значение зависимой переменной для опыта = 10 лет в каждой категории переменной south

* 2. Метод инструментальных переменных и 2-МНК
* Проанализируем, какие из имеющихся переменных могут быть инструментами для educ

* nearc4 - инструмент для educ
ivregress 2sls lwage (educ = nearc4) exper expersq black south smsa smsa66 reg662 reg663 reg664 reg665 reg666 reg667 reg668 reg669, first
est store iv

estat firststage // тестирование релевантности (качества) инструментов. H0: инструменты слабые
hausman iv ols // тест Хаусмана на проверку эндогенности регрессоров. H0: OLS-оценки. H1: IV-оценки

* nearc4 и nearc2 - инструменты для educ
ivregress 2sls lwage (educ = nearc4 nearc2) exper expersq black south smsa smsa66 reg662 reg663 reg664 reg665 reg666 reg667 reg668 reg669, first
est store iv2
estat firststage // тестирование релевантности (качества) инструментов. H0: инструменты слабые
estat overid // тестирование валидности (экзогенности) инструментов (l>=k) (тест Саргана). H0: все инструменты экзогенные (не коррелируют с ошибкой). H1: хотя бы один из инструментов эндогенный
hausman iv2 ols // тест Хаусмана на проверку эндогенности регрессоров. H0: OLS-оценки. H1: IV-оценки

* fatheduc, motheduc - инструменты для educ
ivregress 2sls lwage (educ = fatheduc motheduc) educ exper expersq black south smsa smsa66 reg662 reg663 reg664 reg665 reg666 reg667 reg668 reg669, first
est store iv3
* Тестирование инструментов
estat firststage // тестирование релевантности (качества) инструментов. H0: инструменты слабые
estat overid // тестирование валидности (экзогенности) инструментов (l>=k) (тест Саргана). H0: все инструменты экзогенные (не коррелируют с ошибкой). H1: хотя бы один из инструментов эндогенный
hausman iv3 ols // тест Хаусмана на проверку эндогенности регрессоров. H0: OLS-оценки. H1: IV-оценки

* nearc4, fatheduc, motheduc - инструменты для educ
ivregress 2sls lwage (educ = nearc4 fatheduc motheduc) educ exper expersq black south smsa smsa66 reg662 reg663 reg664 reg665 reg666 reg667 reg668 reg669, first
estat iv4
estat firststage
estat overid
hausman iv4 ols // тест Хаусмана на проверку эндогенности регрессоров. H0: OLS-оценки. H1: IV-оценки


* Logit-, probit-модели

clear

* Логит-модель
use http://www.stata.com/data/jwooldridge/eacsap/mroz, clear
describe
summarize

reg inlf educ exper expersq age kidslt6 kidsge6 nwifeinc // оценим линейную модель вероятности
predict prob_ols // прогноз по линейной модели вероятности

logit inlf educ exper expersq age kidslt6 kidsge6 nwifeinc // оценивание logit-модели с помощью ММП (логистические шансы). Коэффициент при переменной показывает, во сколько раз изменится логарифм отношения P("удачи")/P("неудачи")
logit, or //  odds ratio (отношение шансов P("удачи")/P("неудачи")=exp(xi'b)). При увеличении xj на 1 единицу шансы события {yi=1} вырастают в exp(bj) раз.
* или альтернативная команда
logistic inlf educ exper expersq age kidslt6 kidsge6 nwifeinc // odds ratio
predict pl0, pr // прогноз вероятности "успеха"
sum pl0 // описательные статистики прогнозной вероятности "успеха"

logit inlf educ exper expersq age kidslt6 kidsge6 nwifeinc, vce(robust) // оценивание logit-модели с помощью ММП (логистические шансы)
logit, or // odds ratio (отношение шансов P("удачи")/P("неудачи")=exp(xi'b)). При увеличении xj на 1 единицу шансы события {yi=1} вырастают в exp(bj) раз.

predict pl1, pr // прогнозирование вероятности "успеха"
sum pl1 // описательные статистики прогнозной вероятности "успеха"

test kidslt6 = kidsge6 = 0 // тестирование гипотезы о незначимости переменных, характеризующих состав детей в семье

margins, dydx(*) // усредненные предельные эффекты 
margins, dydx(*) atmeans // предельные эффекты (marginal effects) для "среднего" наблюдения. ПЭ для переменной xj показывает, на сколько изменится вероятность "успеха" при увеличении xj на 1 единицу
margins, at(educ = (10(2)20)) // средняя вероятность "успеха" на каждом уровне educ от 10 до 20 лет с шагом 2 года
margins, at(educ = (10(2)20)) vsquish // средняя вероятность "успеха" на каждом уровне educ от 10 до 20 лет с шагом 2 года
margins, at(educ = (10(2)20)) atmeans // вероятность "успеха" на каждом уровне educ от 10 до 20 лет с шагом 2 года при средних значениях остальных переменных

net search fitstat // загрузка fitstat
fitstat // различные показатели качества моделей из разных классов, в том числе и бинарных

* Пробит-модель
probit inlf educ exper expersq age kidslt6 kidsge6 nwifeinc
probit, or
predict probit_pr, pr

margins, dydx(*) atmeans
margins, dydx(*)

fitstat

log close
