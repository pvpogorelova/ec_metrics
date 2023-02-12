# Загрузка неоходимых пакетов
install.packages("systemfit")
install.packages("foreign")
install.packages("car")
install.packages("rio")
install.packages("wooldridge")

# Подключение библиотек
library(foreign)
library(systemfit) # системы регрессионных уравнений
library(car)
library(rio) # импорт данных
library(wooldridge) # наборы данных из Вулдриджа


# SUR
# Пример из Вулдриджа
d = import("/Users/polinapogorelova/Downloads/dataset-48043.csv")

r1 = hrearn ~ educ + exper + expersq + tenure + tenuresq + union + south + nrtheast + nrthcen + married + white + male
r2 = hrbens ~ educ + exper + expersq + tenure + tenuresq + union + south + nrtheast + nrthcen + married + white + male

fitsur = systemfit(list(hrearnreg = r1, hrbensreg = r2), data = d)
summary(fitsur)

# Системы одновременных уравнений
d = mroz

# Зададим спецификацию уравнений системы
H = hours ~ lwage + educ + age + kidslt6 + kidsge6 + nwifeinc
LW = lwage ~ hours + educ + exper + expersq
sys = list(H,LW)
sys

# Зададим инструментальные переменные (экзогенные)
instr = ~ educ + age + kidslt6 + kidsge6 + nwifeinc + exper + expersq

# Оценим систему одновременных уравнений
mroz.sys = systemfit(sys, inst = instr,
                       method = "2SLS", data = d)
summary(mroz.sys)
