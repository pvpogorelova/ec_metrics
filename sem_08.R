install.packages("ggfortify")

library(tidyverse) # манипуляции с данными и построение графиков
library(sjPlot) # красивые графики для линейных моделей
library(rio) # импорт/экспорт данных
library(car) # тестирование гипотез
library(lmtest) # тестирование гипотез
library(ggfortify)

setwd('/Users/polinapogorelova/Desktop/Метрика_ИП') # установка рабочей директории
# Импортируем данные
data = import('Данные/Chow_2.xls')
head(data)

# Задание 1.
# Оценим модель регрессии с факторной переменной Quarter (номер квартала)
data$Quarter = as.factor(data$Quarter)

model_1 = lm(C ~ Y + Quarter, data)
summary(model_1)

# Протестируем совместную значимость сезонных дамми переменных
linearHypothesis(reg, c("D2 = 0", "D3 = 0", "D4 = 0")) # все сезонные дамми совместно незначимы, так как p-value > 0.05 и гипотеза H0 не отвергается

reg_wc = lm(C ~ 0 + Y + D1 + D2 + D3 + D4, data)
summary(reg_wc) # получили идентичные результаты, как и для случая с константой

# Изобразим зависимость потребления от дохода в зависимости от квартала
data %>%
  ggplot(aes(x = Y,
             y = C,
             color = Quarter))+
  geom_point()+
  geom_smooth(method = "lm")

# По графику есть основания предположить, что зависимость по кварталам может отличаться. Включим в модель квартальные дамми и перекрестные переменные

model_2 = lm(C ~ Y*Quarter, data)
summary(model_2)


# Задание 2: обнаружение влиятельных наблюдений и выбросов.
reg = lm(C ~ Y + D2 + D3 + D4, data)
summary(reg)


# Таблица с метриками для выявления влиятельных наблюдений и выбросов
influence.measures(reg)

# Показатели воздействия наблюдений: леверидж, воздействие, DFFITS
# 1. Анализ влиятельности наблюдений: точки левериджа
p = 5
n = 32
print(augment(reg)$.hat,4)
plot(augment(reg)$.hat, type = 'h')
as.vector(which(abs(augment(reg)$.hat) > 2*p/n)) # точки левериджа не обнаружены

# 2. Анализ влиятельности наблюдений: воздействие
reg_info = augment(reg) %>% mutate(infl = (.resid * .hat)/(1 - .hat))
reg_info
plot(reg_info$infl, type = 'h') + title("Воздействие")

# 3. Анализ влиятельности наблюдений: DFFITS
dffits = as.data.frame(dffits(reg))
dffits

head(dffits)
thresh_dffits = 2*sqrt(p/n)
thresh_dffits
as.vector(which(abs(dffits) > thresh_dffits))

# 4. Анализ влиятельности наблюдений: DFBETAS
dfbetas = as.data.frame(dfbetas(reg))
head(dfbetas)
thresh = 2 / sqrt(32)
thresh
as.vector(which(abs(dfbetas$Y) > thresh))
as.vector(which(abs(dfbetas$D2) > thresh))
as.vector(which(abs(dfbetas$D3) > thresh))
as.vector(which(abs(dfbetas$D4) > thresh))

# Визуализируем DFBETAS для всех коэффициентов при переменных
par(mfrow = c(2,2)) # задаем раметку для четырех графиков

# график DFBETAS для Y
plot(dfbetas$Y, type = 'h')
abline(h = thresh, lty = 2)
abline(h = -thresh, lty = 2)

# график DFBETAS для D2
plot(dfbetas$D2, type = 'h')
abline(h = thresh, lty = 2)
abline(h = -thresh, lty = 2)

# график DFBETAS для D3
plot(dfbetas$D3, type = 'h')
abline(h = thresh, lty = 2)
abline(h = -thresh, lty = 2)

# график DFBETAS для D4
plot(dfbetas$D4, type = 'h')
abline(h = thresh, lty = 2)
abline(h = -thresh, lty = 2)

# 5. Анализ выбросов: стьюдентизированные остатки
results = augment(reg) %>% mutate(stud_resid = .resid / .sigma / sqrt(1 - .hat))
results$stud_resid
crit = qt(.95, n-p)
as.vector(which(abs(results$stud_resid) > crit))

# Переоценим модель model_1, исключив из набора данных наблюдения с номерами 14, 15, 16, 19
data_new = data[-c(14,15,16,19),]
data_new
nrow(data_new)
reg_new = lm(C ~ Y + D2 + D3 + D4, data = data_new)
summary(reg_new)
