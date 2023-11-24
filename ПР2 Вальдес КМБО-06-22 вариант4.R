#Вальдес Перес Адам Денисович вариант 4 КМБО-06-22  №2

library("lmtest")
library("car")

data = swiss
data

# №1 Проверим отсутствие зависимости между регрессорами
pairwise_comparison_1 = lm(Fertility ~ Agriculture, data)
summary(pairwise_comparison_1) # R^2 ~12% - зависимости нет.

pairwise_comparison_1 = lm(Fertility ~ Infant.Mortality, data)
summary(pairwise_comparison_1) # R^2 ~17% - зависимости нет.

pairwise_comparison_1 = lm(Agriculture ~ Infant.Mortality, data)
summary(pairwise_comparison_1) # R^2 ~0,4 - зависимости нет


# №2 
model = lm(Education ~ Fertility + Infant.Mortality + Agriculture, data)
summary(model)
#R^2 ~ 63%, p-значение у Infant.Mortality высокие (нет звёздочек) - модель плохая 

model = lm(Education ~ Fertility + Agriculture, data)
summary(model)
#R^2 ~ 62%, модель почти не изменилась


# Дальше будем работать с моделью: model = lm(Education ~ Fertility + Agriculture, data)


# №3


model = lm(I(log(Education)) ~ I(log(Fertility)) + I(log(Agriculture)), data)     
vif(model) # линейной зависимости нет.
summary(model)# R^2 ~ 0.47, p-статистика плохая для I(log(Fertility)) и хорошая для I(log(Agriculture)

model = lm(Education ~ I(log(Fertility)) + I(log(Agriculture)), data)     
vif(model) # линейной зависимости нет.
summary(model)# R^2 ~ 0.73, p-статистика хорошая для I(log(Fertility)) и I(log(Agriculture))

model = lm(Education ~ Fertility + I(log(Agriculture)), data)     
vif(model) # линейной зависимости нет.
summary(model)# R^2 ~ 0.69, p-статистика хорошая для I(log(Fertility)) и I(log(Agriculture))

model = lm(Education ~ Agriculture + I(log(Fertility)), data)     
vif(model) # линейной зависимости нет.
summary(model)# R^2 ~ 0.69, p-статистика хорошая для I(log(Fertility)) и I(log(Agriculture))

# Наилучшей из них будет следующая модель:  model = lm(Education ~ Fertility + I(log(Agriculture)), data)   


# №4

model = lm(Education ~ Fertility + Agriculture + I(Fertility^2) + I(Agriculture^2) + I(Fertility*Agriculture), data) 
vif(model) # есть линейная зависимость, уберём регрессоры с максимальным VIF

model = lm(Education ~ Fertility + Agriculture + I(Agriculture^2) + I(Fertility*Agriculture), data) 
vif(model) # есть линейная зависимость, уберём регрессоры с максимальным VIF

model = lm(Education ~ Fertility + Agriculture + I(Agriculture^2), data) 
vif(model) # есть линейная зависимость, уберём регрессоры с максимальным VIF

model = lm(Education ~ Fertility + I(Agriculture^2), data) 
vif(model) # линейной зависимости нет.
summary(model) # R^2 ~ 0.59, p-статистика хорошая  и для Fertility и  для Agriculture^2.


#наилучшая модель:  model = lm(Education ~ Fertility + I(Agriculture^2), data)
