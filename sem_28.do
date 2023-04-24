clear
set more off
cd /Users/polinapogorelova/Desktop/СМЭФ_Эконометрика/Эконометрика_22_23
log using sem28.log

* Урезанные (усеченные данные) – для части наблюдений знаечние зависимой переменной не наблюдается совсем
* Загрузим данные
use https://stats.idre.ucla.edu/stat/stata/dae/truncreg, clear
* Описательные статистики
summarize achiv langscore

* Построим гистограмму для зависимой переменной
histogram achiv, bin(6) start(40) freq normal

* Оценим с помощью МНК модель регрессии
reg achiv langscore i.prog
* Оценим модель в помощью ММП, принимая во внимание, что данные урезаны
truncreg achiv langscore i.prog, ll(40)

* Рассчитаем предельные эффекты
* ПЭ для E(y)
margins, dydx(*)
* ПЭ для E(y|y>=40)
margins, dydx(*) predict(e(40,.)) 

* Tobit I. (данные о дополнительных выплатах сотрудникам)
use http://fmwww.bc.edu/ec-p/data/wooldridge/fringe.dta, clear

sum hrbens exper age educ tenure married male white

twoway scatter hrbens exper
twoway scatter hrbens tenure

reg hrbens exper expersq age educ tenure tenuresq married male white

help tobit
tobit hrbens exper expersq age educ tenure tenuresq married male white, ll(0)
test educ == age

margins, dydx(*) predict(e(0,.)) // предельный эффект для наблюдаемой зависимой переменной в усеченной совокупности (hrbens>0)
margins, dydx(*) predict(pr(1,2)) // вычисляет Pr(a < yj < b) - вероятность того, что yj|xj будет наблюдаемо в интервале (a,b)

predict yhat
correlate hrbens yhat

log close
