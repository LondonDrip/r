#Вальдес Перес Адам КМБО-06-22 Вариант 4

#П.Р.3

#Номер выборки 20
library("lmtest")
library("rstatix")
library("GGally") 
library("foreign")
library("dplyr")
library(car)
library("rlms") 
library("dplyr") 
library(sandwich)
library("haven")
library("devtools")


data <- read.csv("r20i_os26c.csv",sep=",", dec = ".", header=TRUE)
glimpse(data)
data = select(data, ph5, p_age, p_marst, p_diplom, status, pj13.2, pj6.2)
data = na.omit(data)
glimpse(data)

data2 = select(data) 
glimpse(data)

#Пол
data2["sex"] = 0
data2$sex[which(data$ph5 == 1)] <- 1

#Возраст
age = data$p_age
data2["age"] = (age - mean(age)) / sqrt(var(age))

#Семейное положение:
#Состоит ли в зарегестрированном браке?
data2$wed1 = 0
data2$wed1[which(data$p_marst==2)] <- 1
data2$wed1[which(data$p_marst==6)] <- 1

#Разведён или вдовец?
data2$wed2 = 0
data2$wed2[which(data$p_marst==4)] <- 1
data2$wed2[which(data$p_marst==5)] <- 1

#Никогда не состоял в браке?
data2$wed3 = 0
data2$wed3[which(data$p_marst==1)] <- 1

# Проверим, что отсутствует линейная зависимость между симейными положениями
vif(lm(data$pj13.2 ~ data2$wed1 + data2$wed2 + data2$wed3))

#Наличие высшего образования
data2$higher_educ = 0
data2$higher_educ[which(data$p_diplom==6)] <- 1

#Живёт в городе?
data2$city_status = 0
data2$city_status[which(data$status==1)] <- 1
data2$city_status[which(data$status==2)] <- 1

#Нормализованное среднее число рабочих часов в неделю
working_hours = data$pj6.2
data2$working_hours = (working_hours - mean(working_hours)) / sqrt(var(working_hours))

#Нормализованная средняя зарплата
salary = data$pj13.2
data2$salary = (salary - mean(salary)) / sqrt(var(salary))


# 1. Постройте линейную регрессию зарплаты на все параметры, которые Вы выделили из данных мониторинга. Не забудьте оценить коэффициент вздутия дисперсии VIF.

model1 = lm(data = data2, salary ~ sex + age + wed1 + wed2 + wed3 + higher_educ + city_status + working_hours)
vif(model1)
summary(model1)
#высокий vif, исключаю wed1 

model1 = lm(data = data2, salary ~ sex + age + wed2 + wed3 + higher_educ + city_status + working_hours)
vif(model1)
summary(model1)


# 2. Поэкспериментируйте с функциями вещественных параметров: используйте логарифм и степени (хотя бы от 0.1 до 2 с шагом 0.1).

# с логарифмами:
model1 = lm(data = data2, salary ~ sex + age + wed2 + wed3 + higher_educ + city_status + working_hours + I(log1p(working_hours)) + I(log(ifelse(age == 0, 0.1, age+5))))
vif(model1)
summary(model1) 
#R^2 ~ 0.01922 
# плохой vif у working_hours, исключаю


model1 = lm(data = data2, salary ~ sex + age + wed2 + wed3 + higher_educ + city_status + I(log1p(working_hours)) + I(log(ifelse(age == 0, 0.1, age+5))))
vif(model1)
summary(model1) 
# R^2 ~ 0.01855 
# плохой vif у age, исключаю


model1 = lm(data = data2, salary ~ sex + wed2 + wed3 + higher_educ + city_status + I(log1p(working_hours)) + I(log(ifelse(age == 0, 0.1, age+5))))
vif(model1)
summary(model1) 
# R^2 ~ 0.01825, улучшения значений путем исключения переменных добиться не получилось, переходим к ступеням


#Со степенями:
current_pow = 0.1
model1 = lm(data = data2, salary ~ sex + wed2 + wed3 + higher_educ + city_status + I(Mod(working_hours)^current_pow) + I(Mod(age)^current_pow))
vif(model1)
summary(model1) 
# R^2 ~  0.01258 
# Плохое значение p-статистики у wed3, исключаю


current_pow = 0.2
model1 = lm(data = data2, salary ~ sex + wed2 + higher_educ + city_status + I(Mod(working_hours)^current_pow) + I(Mod(age)^current_pow))
vif(model1)
summary(model1) 
# R^2 ~  0.01264 
# Хорошие значения vif


current_pow = 0.3
model1 = lm(data = data2, salary ~ sex + wed2 + higher_educ + city_status + I(Mod(working_hours)^current_pow) + I(Mod(age)^current_pow))
vif(model1)
summary(model1) 
# R^2 ~  0.01262 
# Плохая р-статистика у sex, исключу



current_pow = 0.4
model1 = lm(data = data2, salary ~ wed2 + higher_educ + city_status + I(Mod(working_hours)^current_pow) + I(Mod(age)^current_pow))
vif(model1)
summary(model1)  
# R^2 ~  0.01261
# Везде неплохая р-статистика

current_pow = 0.5
model1 = lm(data = data2, salary ~ wed2 + higher_educ + city_status + I(Mod(working_hours)^current_pow) + I(Mod(age)^current_pow))
vif(model1)
summary(model1)
# R^2 ~  0.01239 
# Везде неплохая р-статистика


#Заметим, что с увеличением current_pow немного уменьшается R^2, перейдём сразу к степени 0.9

current_pow = 0.9
model1 = lm(data = data2, salary ~ wed2 + higher_educ + city_status + I(Mod(working_hours)^current_pow) + I(Mod(age)^current_pow))
vif(model1)
summary(model1)
# R^2 ~  0.01235
# Везде хорошая p-статистика

# Для степени 1 результат мы уже имеем

current_pow = 1.1
model1 = lm(data = data2, salary ~ wed2 + higher_educ + city_status + I(Mod(working_hours)^current_pow) + I(Mod(age)^current_pow))
vif(model1)
summary(model1)
# R^2 ~  0.01234
# Везде хорошая p-статистика

current_pow = 1.2
model1 = lm(data = data2, salary ~ wed2 + higher_educ + city_status + I(Mod(working_hours)^current_pow) + I(Mod(age)^current_pow))
vif(model1)
summary(model1)
# R^2 ~  0.01233
# Везде хорошая p-статистика

current_pow = 1.3
model1 = lm(data = data2, salary ~ wed2 + higher_educ + city_status + I(Mod(working_hours)^current_pow) + I(Mod(age)^current_pow))
vif(model1)
summary(model1)
# R^2 ~  0.01233
# Везде хорошая p-статистика

current_pow = 1.4
model1 = lm(data = data2, salary ~ wed2 + higher_educ + city_status + I(Mod(working_hours)^current_pow) + I(Mod(age)^current_pow))
vif(model1)
summary(model1)
# R^2 ~  0.01232
# Везде хорошая p-статистика

current_pow = 1.5
model1 = lm(data = data2, salary ~ wed2 + higher_educ + city_status + I(Mod(working_hours)^current_pow) + I(Mod(age)^current_pow))
vif(model1)
summary(model1)
# R^2 ~  0.01232
# Везде хорошая p-статистика

# и далее R^2 уменьшается

current_pow = 1.8
model1 = lm(data = data2, salary ~ wed2 + higher_educ + city_status + I(Mod(working_hours)^current_pow) + I(Mod(age)^current_pow))
vif(model1)
summary(model1)
# R^2 ~  0.01231
# Везде хорошая p-статистика

current_pow = 1.9
model1 = lm(data = data2, salary ~ wed2 + higher_educ + city_status + I(Mod(working_hours)^current_pow) + I(Mod(age)^current_pow))
vif(model1)
summary(model1)
# R^2 ~  0.0123
# Везде хорошая p-статистика

current_pow = 2
model1 = lm(data = data2, salary ~ wed2 + higher_educ + city_status + I(Mod(working_hours)^current_pow) + I(Mod(age)^current_pow))
vif(model1)
summary(model1)
# R^2 ~  0.0123
# Плохая p-статистика у I(age^current_pow), у остальных компонентов р-статистика хорошая


# 3.Выделите наилучшие модели .

# Сравниваем лучшие из полученных моделей 

current_pow = 0.2
model1 = lm(data = data2, salary ~ sex + wed2 + higher_educ + city_status + I(Mod(working_hours)^current_pow) + I(Mod(age)^current_pow))
vif(model1)
summary(model1) 
# (R^2 = 0.01264) наилучшая модель по параметру R^2, так же имеются хорошие значения vif

current_pow = 0.9
model1 = lm(data = data2, salary ~ wed2 + higher_educ + city_status + I(Mod(working_hours)^current_pow) + I(Mod(age)^current_pow))
vif(model1)
summary(model1)
# Тоже является хорошей моделью т.к. в ней R^2 не сильно меньше, а значение р-стаистика большенства параметров ***.


# Итого, среди моделей без линейной зависимости параметров с хорошими показателями p-статистики у регрессоров лучшей по R^2 оказалась модель для степени 0.5:  
current_pow = 0.2
model1 = lm(data = data2, salary ~ sex + wed2 + higher_educ + city_status + I(Mod(working_hours)^current_pow) + I(Mod(age)^current_pow))
vif(model1)
summary(model1)
# с R^2 = 0.01264


# 4. Сделайте вывод о том, какие индивиды получают наибольшую зарплату.

#Согласно этой модели больше всего зарабатывают люди с высшим образованием, проживающие в городах, работающие большое число часов в неделю.
#Пол неважен


# 5. Оцените регрессии для подмножества индивидов: а) Городские жители, не состоявшие в браке; б )разведенные женщины, без высшего образования
current_pow = 0.2

#Городские жители, не состоявшие в браке
data3 = subset(data2, sex  == 1)
data3 = subset(data2, wed1 == 0)
data3 = subset(data3, higher_educ == 1) 

model1 = lm(data = data2, salary ~ sex + wed2 + higher_educ + city_status + I(Mod(working_hours)^current_pow) + I(Mod(age)^current_pow))
summary(model1)
# Все параметры, кроме sex значимые. R^2 = 0.01253
# Наибольшая зарплата у не женатых мужчин  с высшим образованием, работающих много.

#разведенные женщины, без высшего образования
data3 = subset(data2, city_status == 1)
data3 = subset(data3, wed1 == 1) 
model1 = lm(data = data2, salary ~ sex + wed2 + higher_educ + city_status + I(Mod(working_hours)^current_pow) + I(Mod(age)^current_pow))
summary(model1)
# Все параметры, кроме sex значимые. R^2 = 0.01253
# Наибольшая зарплата у проживающих в городе, работающих много, а так же состоящих в браке.