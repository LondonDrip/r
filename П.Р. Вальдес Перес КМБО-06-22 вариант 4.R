#Вальдес Перес Адам Денисович вариант 4 КМБО-06-22

library("lmtest")
data = swiss
data

mean(data$Agriculture)   #cp значение Agriculture=50.65957
mean(data$Education)     #cp значение Education=10.97872
mean(data$Examination)  #cp значение Examination=16.48936

var(data$Agriculture)   #значение дисперсии Agriculture=515.7994
var(data$Education)     #значение дисперсии Education=92.45606
var(data$Examination)   #значение дисперсии Examination=63.64662

sd(data$Agriculture)    #ср-квадратное отклонение Agriculture=22.71122
sd(data$Education)      #ср-квадратное отклонение Education=9.615407
sd(data$Examination)    #ср-квадратное отклонение Examination=7.977883

model1= lm(Agriculture~Education,data)
summary(model1)
summary(model1)$r.square
#Зависимость по коэффициенту R^2 ~41%, модель хорошая.
#Связи отрицательные т.к. Education с отрицательным значением, взаимосвязь высокая ***.
model2 = lm(Agriculture~Examination,data)
summary(model2)
summary(model2)$r.square
#Зависимость по коэффициенту R^2 ~47%, модель хорошая.
#Зависимость отрицательная т.к. Examination с отрицательным значением, взаимосвязь высокая значение ***