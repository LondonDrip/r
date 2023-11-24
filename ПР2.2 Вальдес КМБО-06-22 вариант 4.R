#Вальдес Перес Адам Денисович вариант 4 КМБО-06-22  №2.2

library("lmtest")
library("GGally")
library("car")

data = na.omit(swiss)

#модель из задания 2.
model = lm(Education ~ Fertility + Agriculture + Infant.Mortality, data)
summary(model)

# 1.

t_critical = qt(0.975, df = 43) #


Std_Error_Intercept = 6.81384
Std_Error_Fertility = 0.08564
Std_Error_Agriculture = 0.04290
Std_Error_Infant.Mortality = 0.34428


# Доверительные интервалы:
print(paste("Доверительный интервал свободного коэффициента: [", model$coefficients[1] - t_critical * Std_Error_Intercept, 
            ",", model$coefficients[1] + t_critical * Std_Error_Intercept, "]"))

print(paste("Доверительный интервал Fertility: [", model$coefficients[2] - t_critical * Std_Error_Fertility, 
            ",", model$coefficients[2] + t_critical * Std_Error_Fertility, "]"))

print(paste("Доверительный интервал Agriculture: [", model$coefficients[3] - t_critical * Std_Error_Agriculture, 
            ",", model$coefficients[3] + t_critical * Std_Error_Agriculture, "]"))

print(paste("Доверительный интервал Inafnt.Mortality: [", model$coefficients[4] - t_critical * Std_Error_Infant.Mortality, 
            ",", model$coefficients[4] + t_critical * Std_Error_Infant.Mortality, "]"))


# 2.

#"Доверительный интервал свободного коэффициента: [ 29.6066425604863 , 57.0894785100593 ]"
# Доверительный интервал не содержит нуля следовательно мы отвергаем статистическую гипотезу о том, что коэффицент равен нулю.

#"Доверительный интервал Fertility: [ -0.59798399569508 , -0.252564955811339 ]"
#Доверительный интервал не содержит нуля ,следовательно мы отвергаем статистическую гипотезу о том, что коэффицент равен нулю.

#"Доверительный интервал Agriculture: [ -0.272002437151662 , -0.0989702464579143 ]"
#Доверительный интервал не содержит нуля следовательно мы отвергаем статистическую гипотезу о том, что коэффицент равен нулю.

#"Доверительный интервал Infant.Mortality: [ -0.350461171130071 , 1.03815240957024 ]"
#Доверительный интервал содержит ноль следовательно мы никак не можем исключеть статистическую гипотезу о том, что коэффицент равен нулю.


# 3. (p = 95%, Fertility = 55, Agriculture = 15, Infant.Mortality = 18)

new.data = data.frame(Fertility = 55, Agriculture = 15, Infant.Mortality = 18)
predict(model, new.data, interval = "confidence")

# Доверительный интервал: [23.36489, 27.00966]