clear
set more off
log using sem_30.log

* Панельные данные
import excel /Users/polinapogorelova/Desktop/Panel_2.xls, firstrow clear // импорт данных

* OLS estimation: Pooled Regression
reg lnOpEx lnTC

* LSDV - проверим наличие индивидуального эффекта для каждого объекта выборки
reg lnOpEx lnTC D2 D3 D4

* Далее перейдем к рассмотрению моделей с фиксированными и случайными эффектами
xtset i t // зададим пространственную и временную компоненту


* FE Модель с фиксированными эффектами
* Оценивание FE-модели+тест на спецификацию: H0: pooled-модели H1: FE-модель
xtreg lnOpEx lnTC, fe

predict fe, u
* Cохраним результаты оценивания FE-модели
est store fe

* RE Модель со случайными эффектами
xtreg lnOpEx lnTC, re
est store re

* Тест Вальда на спецификацию. H0: pooled-модель H1: RE-модель
xttest0

* Тест Хаусмана на спецификацию. H0: RE-модель. H1: FE-модель
hausman fe re

* Выведем оценки, полученные по трем моделям в виде единой таблицы
estimates table ols a_ols fe re, star stats(N r2 r2_a)

clear
