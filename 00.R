library(AER)
library(negligible)
library(tidyverse)
data(CASchools)
CASchools <- as.data.frame(CASchools)
glimpse(CASchools)
# Корреляция
neg.cor(v1 = CASchools$math, v2 = CASchools$read, eiU = .2, eiL = -.2)
# Тесты 2 на 2
v1<-as.vector(CASchools$math>median(CASchools$math))
v2<-as.vector(CASchools$grades)
tab<-table(v1,v2)
tab
neg.cat(tab=tab,alpha=.05,eiU=0.5,nbootpd=50)
# Опосредованная корреляция
neg.esm(X = math,Y = read,M = computer,eil = -.15,eiu = .15,
        nboot =  50,data = CASchools)
# Дисперсия независимых выборок по группам
neg.indvars(dv = CASchools$math, iv = CASchools$grades, eps = 0.25)
# Наличие связи между двумя числовыми переменными
neg.intcont(outcome = read,pred1 = math,pred2 = computer,eiL = -.15,eiU = .15, nboot =  50,data = CASchools)
# Равенство средних в зависимых выборках
data("PSID7682")
date <- rbind(PSID7682[PSID7682$year==1976,c(13,12,14)],PSID7682[PSID7682$year==1982,c(13,12,14)])
neg.paired(outcome = date$wage, group = date$year, ID = date$id,eil=-10,eiu=10)
# Эквивалентность коэффициента в регрессионной модели
neg.reg(formula=math~teachers+computer+income,data=CASchools,predictor=teachers,eil=-.1,eiu=.1,nboot=50)
# Эквивалентность двух коэффициентов корреляции
yx1 <- CASchools %>% filter(grades=="KK-06")
yx2 <- CASchools %>% filter(grades!="KK-06")
neg.twocors(r1=cor(yx1$math,yx1$read),n1=length(yx1$math),r2=cor(yx2$math,yx2$read),n2=length(yx1$math),eiu=.15,eil=-0.15,  dep=FALSE)
# Равенство средних в независимых выборках
neg.twoindmeans(dv=math,iv=grades,eiL=-1,eiU=1,data=CASchools)
