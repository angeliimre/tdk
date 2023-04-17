setwd("C:/Users/Gazda/Desktop/corvinyo/mia")
getwd()
library(readxl)
adatok <- read_excel("adatok.xlsx")
View(adatok)
adatok=as.data.frame(adatok)
adatok

adatok$h_gyakorisag=adatok$h_gyakorisag*100
adatok$rel_gyak=adatok$rel_gyak*100

hist(adatok$h_gyakorisag)
hist(adatok$rel_gyak)
adatok["h_gyakorisag"]

summary(adatok$rel_gyak)
summary(adatok$h_gyakorisag)

library(psych)
describe(adatok$h_gyakorisag)
describe(adatok$rel_gyak)

adatok[adatok["h_gyakoriság"]>0,"Diagnózis"]


r_gyakoriak=adatok[order(-adatok["rel_gyak"]),]
r_gyakoriak=r_gyakoriak[1:5,]

h_gyakoriak=adatok[order(-adatok["h_gyakorisag"]),]
h_gyakoriak=h_gyakoriak[1:3,]

gy=function(x){
  if(x%in%r_gyakoriak$Diagnózis|x%in%h_gyakoriak$Diagnózis){
    return(x)
  }
  return("")
}
gy("refluxbetegség")

adatok$labels=lapply(adatok$Diagnózis,gy)

list(gyakoriak["Diagnózis"])

library(ggplot2)
ggplot(data=adatok,aes(x=h_gyakorisag,y=rel_gyak,label=Diagnózis))+geom_point(size = 2,alpha = 0.6)+
  theme_bw()+
  geom_text(aes(label=labels),hjust=0, vjust=0)+
  geom_smooth(method="lm",se=FALSE)

model=lm(data=adatok,formula=rel_gyak~h_gyakorisag)

summary(model)
model$coefficients[1]+model$coefficients[2]*adatok[adatok$Diagnózis=="refluxbetegség","h_gyakorisag"]

adatok[adatok$Diagnózis=="refluxbetegség","rel_gyak"]

r_gyakori=adatok[order(-adatok["rel_gyak"]),]
r_gyakori=r_gyakori[1:20,]

stat <- read_excel("stat.xlsx")
stat=as.data.frame(stat)
stat<-stat[,c("nem","bet","kor")]
stat2=stat[stat$bet%in%c(r_gyakori["Diagnózis"])$Diagnózis,]
nrow(stat)
stat[1:10,]

f=nrow(stat[stat$nem=="Férfi",])
n=nrow(stat[stat$nem=="Nő",])
f/(f+n)
n/(f+n)



ggplot(data=stat2,aes(x=bet,fill=nem))+geom_bar(position = "fill")+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
prop.table(table(stat2$bet,stat2$nem),1)
library(questionr)
cramer.v(table(stat$bet,stat$nem))

r_gyakori=r_gyakori[1:3,]
stat3=stat[stat$bet%in%c(r_gyakori["Diagnózis"])$Diagnózis,]

ggplot(data=stat2,aes(y=kor,fill=bet))+geom_boxplot()
by(stat3$kor,stat3$bet,summary)
vh=summary(aov(kor~bet,data=stat))

varhanyados=vh[[1]]$`Sum Sq`[1]/(vh[[1]]$`Sum Sq`[1]+vh[[1]]$`Sum Sq`[2])
varhanyados^(1/2)

stat$nem=as.factor(stat$nem)
stat$bet=as.factor(stat$bet)

model2=lm(kor~.,data=stat)
summa<-summary(model2)
str(summa)
model_tabla<-as.data.frame(summa$coefficients)
significants<-model_tabla[model_tabla$`Pr(>|t|)`<0.1,]

str(significants)
str(list(rownames(significants)))
rownames(significants)

removing<-function(x){
  return(gsub('bet','',x))
}

rownames(significants)<-lapply(rownames(significants),removing)

stat$bet=as.character(stat$bet)

stat[!stat$bet%in%rownames(significants),"bet"]="referencia"
stat$bet<-as.factor(stat$bet)
levels(stat$bet)
stat$bet<-relevel(stat$bet,ref="referencia")

stat$nem<-as.factor(stat$nem)

model3=lm(kor~.,data=stat)
summary(model3)

model4<-lm(kor~.+nem*bet,data=stat)
summary(model4)
stat[stat$bet=="crestszindróm",]

egyuttallasok<-read_excel("stat2.xlsx")
egyuttallasok<-as.data.frame(egyuttallasok)
egyuttallasok

gyakok<- read_excel("gyakok.xlsx")
gyakok<-as.data.frame(gyakok)
gyakok<-gyakok[order(-gyakok["Mennyiségek"]),]
gyakok<-gyakok[1:20,]

egyuttallasok2<-egyuttallasok[egyuttallasok$javitott%in%c(gyakok["Betegségek"])$Betegségek,]
egyuttallasok2


ggplot(data=egyuttallasok2,aes(x=javitott_r,fill=nem))+geom_bar(position = "fill")+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
prop.table(table(egyuttallasok2$javitott_r,egyuttallasok2$nem),1)
library(questionr)
cramer.v(table(egyuttallasok$javitott_r,egyuttallasok$nem))

ggplot(data=egyuttallasok2,aes(y=kor,fill=javitott_r))+geom_boxplot()
by(egyuttallasok2$kor,egyuttallasok2$javitott,summary)
vh=summary(aov(kor~javitott_r,data=egyuttallasok))
varhanyados=vh[[1]]$`Sum Sq`[1]/(vh[[1]]$`Sum Sq`[1]+vh[[1]]$`Sum Sq`[2])
varhanyados^(1/2)

model5=lm(kor~.,data=egyuttallasok)
summa2<-summary(model5)
model_tabla2=as.data.frame(summa2$coefficients)
significants2<-model_tabla2[model_tabla2$`Pr(>|t|)`<0.01,]

str(significants2)
str(list(rownames(significants2)))
rownames(significants2)

removing2<-function(x){
  return(gsub('javitott','',x))
}

rownames(significants2)<-lapply(rownames(significants2),removing2)

egyuttallasok[!egyuttallasok$javitott%in%rownames(significants2),"javitott"]="referencia"
egyuttallasok$javitott<-as.factor(egyuttallasok$javitott)
levels(egyuttallasok$javitott)
egyuttallasok$javitott<-relevel(egyuttallasok$javitott,ref="referencia")
levels(egyuttallasok$javitott)

egyuttallasok$nem<-as.factor(egyuttallasok$nem)

model5<-lm(kor~.+nem*javitott,data=egyuttallasok)
summary(model5)

