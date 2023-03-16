clear
set more off
cd /Users/polinapogorelova/Desktop/vega
log using sem9.log


* Модели счетных данных

* Poisson regression
use "/Users/polinapogorelova/Downloads/crime.dta", clear
hist narr86, discrete scheme(sr1mono) title("Гистограмма распределения числа преступлений")

reg narr86 pcnv avgsen tottime ptime86 qemp86 inc86 durat i.black i.hispan, vce(robust) // оценим линейную модель с помощью МНК
predict narr86_f_ols

poisson narr86 pcnv avgsen tottime ptime86 qemp86 inc86 durat i.black i.hispan, vce(robust) // оценим пуассоновскую регрессию. Для получения робастных стандартных ошибок для оценок параметров используем vce(robust). Коэффициент при xj показывает, насколько увеличится log(narr86) при увеличении xj на 1 единицу
poisson, irr //  incident rate ratios - показывает, во сколько разd в среднем увеличится число преступлений при изменении xj на 1 единиц

predict narr86_f_p, n // прогноз числа преступлений
estat gof // goodness-of-fit H0: модель адекватна
fitstat

margins hispan // вычислим прогнозируемое количество преступлений на каждом уровне переменной hispan

separate narr86_f_p, by(hispan)

twoway scatter narr86_f_p0 narr86_f_p1 inc86, connect(l l) sort ///
       ytitle("Predicted Count of Crimes")  
	   

* Negative binomial regression
nbreg narr86 pcnv avgsen tottime ptime86 qemp86 inc86 durat black i.hispan, dispersion(constant)  // NB-I регрессия Var(y) = mu*(1+delta)
nbreg, irr // incidence-rate ratios = exp(b_k) показывает, во сколько раз изменится зависимая переменная (частота события) при увеличении объясняющей переменной на 1 единицу

predict narr86_f_nb1, n
predict narr_12, pr(1,2) // прогноз вероятности P(a <= y_j <= b), где a=1, b=2 для данного случая
sum narr86 narr86_f_nb1 narr_12


nbreg narr86 pcnv avgsen tottime ptime86 qemp86 inc86 durat i.black i.hispan, dispersion(mean)  // NB-II регрессия Var(y) = mu*(1+alpha*mu)
predict narr86_f_nb2, n
test 1.black = 1.hispan

margins i.hispan, atmeans // прогнозируемое количество преступлений в зависимости от значения переменной hispan при среднем уровне всех остальных переменных

margins, at(avgsen = (6(3)24)) vsquish // среднее количество преступлений, рассчитанное для средней продолжительности отбытых приговоров (в месяцах) avgsen={5,6,7,8,9,10}
margins, at(avgsen = (6(3)24)) atmeans vsquish // количество преступлений, рассчитанное для avgsen={5,6,7,8,9,10} при использовании средних значений для остальных переменных
fitstat


separate narr86_f_nb2, by(hispan)
twoway scatter narr86_f_nb20 narr86_f_nb21 inc86, connect(l l) sort ///
       ytitle("Predicted Count of Crimes")


* Панельные данные
import delimited /Users/polinapogorelova/Downloads/cornwell&rupert.csv, clear // экспорт .csv

sum lwage exp smsa fem union ind if year == 1
sum lwage exp smsa fem union ind if year == 7

egen mlwage = mean(lwage), by(year)
egen mexp = mean(exp), by(year)

graph twoway connected mlwage year, sort
graph twoway connected mexp year, sort

* OLS estimation: Pooled Regression
reg lwage exp expsq smsa south wks union ms fem ed blk ind occ

* LSDV - проверим наличие индивидуального эффекта для каждого объекта выборки
reg lwage exp expsq smsa south wks occ union ms fem ed blk ind i.id
* Тот же самый результат можно получить следующим образом
areg lwage exp expsq smsa south wks occ union ms fem ed blk ind, absorb(id)
estimates store a_lsdv

* Далее перейдем к рассмотрению моделей с фиксированными и случайными эффектами
xtset id year // зададим пространственную и временную компоненту

* FE Модель с фиксированными эффектами
* Оценивание FE-модели+тест на спецификацию: H0: pooled-модели H1: FE-модель
xtreg lwage exp expsq smsa south wks occ union ms fem ed blk ind, fe

* Проверим гипотезу о гетероскедастичности данных
* Тест Вальда для проверки групповой гетероскедастичности. H0: гетероскедастичность отсутствует
ssc install xttest3
xttest3

* Оценим FE-модель в предположении о наличии гетероскедастичности в данных
xtreg lwage exp expsq smsa south wks occ union ms fem ed blk ind, fe vce(robust)

* Cохраним результаты оценивания FE-модели
est store fe
// Оценки within, fe и LSDV эквивалентны

* Оценки со случайным эффектом (b_RE=b_GLS)
xtreg lwage exp expsq smsa south wks occ union ms fem ed blk ind, re
est store re

* Between-оценки модели со случайными эффектами
xtreg lwage exp expsq smsa south wks occ union ms fem ed blk ind, be

* RE-оценка представляет собой средневзвешенное внутри- и межугрупповой оценок

* Тест Вальда на спецификацию. H0: pooled-модель H1: RE-модель
xttest0

* Тест Хаусмана на спецификацию. H0: RE-модель. H1: FE-модель
hausman fe re

* Выведем оценки, полученные по трем моделям в виде единой таблицы
estimates table ols a_ols fe re, star stats(N r2 r2_a)


* 2. Динамические модели
use dahlberg.dta, clear

* Оценим обычную модель регрессии без учета панельной структуры
reg expend l.revenue l.grants l.expend
xtset id year

* Anderson-Hsiao: First-differenced estimator (FD2SLS)
xtivreg expend l.revenue l.grants (l.expend = l2.expend), fd
gen lagrevenue = l.revenue
gen laggrants = l.grants
gen lagexpend = l.expend


* Arellano & Bond (GMM) one-step estimator
xtabond expend l.revenue l.grants, lags(1) noconstant

* Тестирование автокорреляции
estat abond // тест на автокорреляцию остатков 1-го и 2-го порядков
* Тест на валидность инструментов (тест Саргана)
estat sargan // отклонение H0 свидетельствует о том, что надо пересмотреть модель (несовместим с опцией vce(robust))

xtabond expend l.revenue l.grants, lags(1) noconstant vce(robust) // Arellano–Bond robust VCE estimator
xtabond expend l.revenue l.grants, lags(1) vce(robust)

* Arellano & Bond (GMM) two-step estimator
xtabond expend l.revenue l.grants, lags(1) noconstant twostep vce(robust) // twostep
// рекомендуется использовать vce(robust), которая дает состоятельные и асимптотически эффективные оценки при гетероскедастичности - Windmeijer bias-corrected (WC) robust VCE

* Тестирование автокорреляции
estat abond // тест на автокорреляцию остатков 1-го и 2-го порядков
* Тест на валидность инструментов (тест Саргана)
estat sargan // отклонение H0 свидетельствует о том, что надо пересмотреть модель (несовместим с опцией vce(robust))

clear
