clear
set more off
cd /Users/polinapogorelova/Desktop/СМЭФ_Эконометрика/Эконометрика_22_23
log using sem6.log

* Logit-, probit-модели

clear

* Логит-модель
use http://www.stata.com/data/jwooldridge/eacsap/mroz
describe
summarize

reg inlf educ exper expersq age kidslt6 kidsge6 nwifeinc // оценим линейную модель вероятности
predict prob_ols // прогноз по линейной модели вероятности
sum prob_ols // описательные статистики прогнозной вероятности "успеха"

logit inlf educ exper expersq age kidslt6 kidsge6 nwifeinc // оценивание logit-модели с помощью ММП (логистические шансы). 
logit, or //  odds ratio (отношение шансов P("удачи")/P("неудачи")=exp(xi'b)). При увеличении xj на 1 единицу шансы события {yi=1} вырастают в exp(bj) раз. Коэффициент при переменной показывает, во сколько раз изменится логарифм отношения P("удачи")/P("неудачи")
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

* Чувствительность (Sensitivity) — это и есть доля истинно положительных случаев
* Специфичность (Specificity) — доля истинно отрицательных случаев, которые были правильно идентифицированы моделью
lsens, genprob(prob_cutoff) gensens(sens) genspec(spec) title("График зависимости чувствительности и специфичности") // график зависимости Se и Sp от cut-off
gen difference = sens-spec
levelsof sens if abs(difference)<0.01, local(yval)
levelsof prob_cutoff if sens ==`yval', local(xval)

* ROC-кривая отражает зависимость чувствительности от 1-специфичность
lroc // построение ROC-кривой и расчет AUC
estat clas, cutoff(0.55) // таблица классификации (сопряженности y^ и y)
estat gof // H0: модель адекватна (Хи2-критерий Пирсона)

scalar cut_off = 0.55
gen inlf_f = 0
replace inlf_f = 1 if pl1 > cut_off // создание зависимой переменной, полученной в результате оценивания модели и соответствующей оптимальному параметру cut-off

* Пробит-модель
probit inlf educ exper expersq age kidslt6 kidsge6 nwifeinc
probit, or
predict probit_pr, pr

margins, dydx(*) atmeans
margins, dydx(*)

fitstat


* Ordered logit regression
use https://stats.idre.ucla.edu/stat/data/ologit.dta, clear // данные о поступлении в аспирантуру
tab apply
tab apply, nolabel
tab apply public
sum gpa

ologit apply i.pared i.public gpa // оценим порядковый логит. Коэффициенты показывают во сколько раз увеличится логарифм отношения шансов при изменении объясняющей переменной на 1 единицу
ologit apply i.pared i.public gpa, or // получим оценки изменения отношения шансов (odds ratio) перейти с одного уровня на более высокий
listcoef, help

margins, at(pared = (0/1)) predict(outcome(0)) atmeans // прогнозируемая вероятность попасть в низшую категорию в зависимости от значения категориальной переменной "pared"
margins, at(pared = (0/1)) predict(outcome(1)) atmeans // прогнозируемая вероятность попасть в среднюю категорию в зависимости от значения категориальной переменной "pared"
margins, at(pared = (0/1)) predict(outcome(2)) atmeans // прогнозируемая вероятность попасть в высшую категорию в зависимости от значения категориальной переменной "pared"

forvalues i = 0/2 {
  margins, at(gpa = 3.5 pared = 1 public = 1) predict(outcome(`i'))
} // прогноз вероятности попасть в каждую категорию для индивидуума, имеющего GPA равный 3.5, обучавшегося в частной школе и у которого хотя бы один из родителей обучался в аспирантуре

net search omodel
omodel logit apply pared public gpa // альтернативная команда для оценивания упорядоченных моделей логит и пробит, содержащая также реультаты теста на пропорциональность шансов (test for the equivalence of the estimates for cut levels)
predict prob_unlikely prob_somewhatlikely prob_verylikely, pr // прогнозирование вероятностей попасть в каждую категорию
clear

log close
