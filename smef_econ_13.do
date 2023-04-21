clear
set more off
cd /Users/polinapogorelova/Desktop/vega
log using sem13.log

* SFA (Stochastic frontier analysis)
use https://www.stata-press.com/data/r16/xtfrontier1, clear
xtset id t

* q_it = f(x_it) - U_it, где q_it - выпуск, x_it - ресурсы, функция f описывает границу производственных возможностей, U_it -  разница между выпуском при наиболее эффективном использовании заданного набора ресурсов и фактическим выпуском 

* Базовая модель: q_i = f(x_i,b)*k_i*exp(v_i) или ln(q_i) = b0 + b1*ln(x_i1) + ... + bm*ln(x_im) - u_i + v_i,  где u_i = -ln(k_i),
* где  k_i - степень эффективности фирмы; x_i - факторы, влияющие на выпуск; u_i ~ N+(0,sigma2_u) - технологическая неэффективность, v_i ~ N(0, sigma2_v) - ошибки модели.

xtreg lnwidgets lnmachines lnworkers, fe
est store res_fe
predict resfe, r
hist resfe // на гистограмме заметна левосторонняя асимметрия

xtreg lnwidgets lnmachines lnworkers, re
est store res_re
predict resre, ue
hist resre // на гистограмме заметна левосторонняя асимметрия


* 1. Модель без гетерогенности и тренда (Time-invariant model)
* ln(q_it) = b0 + b1*ln(x_1it) + ... + bm*ln(x_mit) - u_it + v_it, где u_it = u_i ~ N+(mu, sigma2_u)

xtfrontier lnwidgets lnmachines lnworkers, ti
est store ti_sfa
predict uhat_ti, u // оценка -ln(TE) с помощью E(u_it|v_it)
hist uhat_ti

predict te_ti, te // оценка BC технической эффективности  = E{exp(−u_i|ε_it)
hist te_ti

* 2. Модели без гетерогенности и с трендом (TVD-модель)
* ln(q_it) = b0 + b1*ln(x_1it) + ... + bm*ln(x_mit) - u_it + v_it,

* Battese, Coelli, 1992 (Time-varying decay model)
* u_it = exp(-η*(t-T_i))*u_i, где T_i - последний период для i-й панели, η - параметр затухания, u_i ~ N+(mu,sigma2_u)
* При η > 0 степень неэффективности уменьшается с течением времени, а при η < 0 степень неэффективности увеличивается с течением времени. Поскольку t = Ti
* в последнем периоде, последний период для фирмы i содержит базовый уровень неэффективности для этой фирмы. Если η > 0, то уровень неэффективности снижается до базового уровня. Если η < 0, то уровень неэффективности возрастает до базового уровня. При η = 0 получаем TI-модель.

xtfrontier lnwidgets lnmachines lnworkers, tvd
est store tvd_sfa
predict uhat_tvd, u // оценка -ln(TE)
hist uhat_tvd

predict te_tvd, te // техническая эффективность = E{exp(−u_it|ε_it)
hist te_tvd

twoway scatter te_ti te_tvd // диаграмма рассчеяния ТЭ по двум моделям

* Cornwell et al., 1990
* ln(q_it) = b_0t + b1*ln(x_1it) + ... + bm*ln(x_mit) - u_it + v_it = b_it + b1*ln(x_1it) + ... + bm*ln(x_mit) + v_it , 
* b_it = c_t1 + c_t2 * t + c_t3*t^2 (квадратичный тренд)
sfpanel lnwidgets lnmachines lnworkers, model(fecss)
est store fecss_sf
predict uhat_fecss, u // техническая неэффективность
hist uhat_fecss
predict te_fecss1, jlms // техническая эффективность JLMS-оценка
hist te_fecss1
predict te_fecss2, bc // недоступно для FECSS-моделей

twoway scatter te_ti te_fecss1

* 3. Модели «True» fixed-effect (Greene, 2005)
* ln(q_it) = b_i + b1*ln(x_1it) + ... + bm*ln(x_mit) - u_it + v_it, b_i - firm-specific коэффициент

sfpanel lnwidgets lnmachines lnworkers, model(tfe) distribution(exponential)
sfpanel lnwidgets lnmachines lnworkers, model(tfe) distribution(exponential) usigma(lnworkers) // учет гетерогенности
est store tfe_sf

predict uhat_tfe, u // -ln(TE) = E(u|ε)
hist uhat_tfe
predict te_tfe1, jlms //  техническая эффективность = exp{−E(u|ε)} (Jondrow et al., 1982)
hist te_tfe1
predict te_tfe2, bc // техническая эффективность = E{exp(−u|ε)} (Battese, Coelli, 1988)
hist te_tfe2
twoway scatter te_tfe1 te_tfe2 // сравним с помощью диаграммы рассеяния JLMS- и BC-оценки технической эффективности

* Модели «True» random-effect (Greene, 2005)
* Шаг 1. ln(q_it) = b0 + b1*ln(x_1it) + ... + bm*ln(x_mit) + n_i + v_it

sfpanel lnwidgets lnmachines lnworkers, model(tre) distribution(hnormal) // здесь thetta - стандартное отклонение случайного эффекта
est store tre_sf
predict uhat_tre, u // техническая неэффективность
hist uhat_tre
predict te_tre1, jlms // техническая эффективность JLMS-оценка
hist te_tre1
predict te_tre2, bc // техническая эффективность BC-оценка
hist te_tre2
twoway scatter te_tre1 te_tre2 // сравним с помощью диаграммы рассеяния JLMS- и BC-оценки технической эффективности


* Попробуем оценить модель TRE, используя экспоненциальное распределение
sfpanel lnwidgets lnmachines lnworkers, model(tre) distribution(exponential) // здесь thetta - стандартное отклонение случайного эффекта
predict te_tre3, jlms // техническая эффективность JLMS-оценка
predict te_tre4, bc // техническая эффективность JLMS-оценка
twoway scatter te_tre3 te_tre4

sum te_ti1 te_tvd1 te_fecss1 te_tfe1 te_tre1 te_tre2
spearman te_ti te_tvd te_fecss1 te_tfe1 te_tre1 te_tre2, star(0.05)

twoway (kdensity te_ti1) (kdensity te_tvd1) (kdensity te_fecss1) (kdensity te_tfe1) (kdensity te_tre1) (kdensity te_tre3), legend(lab(1 "TI") lab (2 "TVD") lab(3 "FECSS") lab(5 "TFE") lab(6 "TRE1") lab(7 "TRE2"))
