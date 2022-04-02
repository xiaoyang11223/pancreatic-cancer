rm(list = ls()) 
table<-read.csv("F:/R语言/胰腺癌总.csv")
table<-as.data.frame(table)
ind<-sample(2,nrow(table),replace = TRUE,prob = c(0.70,0.30))
set.seed(123)
trainset<-table[ind==1,]
testset<-table[ind==2,]
write.csv(trainset,"F:/R语言/train.csv")
write.csv(testset,"F:/R语言/test.csv")
library(tableone)

rm(list = ls()) 

aa<- read.csv('F:/R语言/yixian1.csv')

library(tableone)
head(aa)
str(aa)

for(i in names(aa)[c(1:4)]){aa[,i]<-as.factor(aa[,i])}##批量转为因子factor
for(i in names(aa)[c(6:11)]){aa[,i]<-as.factor(aa[,i])}##批量转为因子factor
for(i in names(aa)[c(13:18)]){aa[,i]<-as.factor(aa[,i])}##批量转为因子factor
for(i in names(aa)[c(20,21)]){aa[,i]<-as.factor(aa[,i])}##批量转为因子factor
names(aa)
shapiro.test(aa$Age)
shapiro.test(aa$Tumor.size)
shapiro.test(aa$Time)

myVars<-c("Age74","Sex","Race","Marital.state","Insurance","Grade","T","N","AJCC","Group","Surgery"
          ,"Radiation","Chemotherapy","Tumor.size")
catVars<-c("Age74","Sex","Race","Marital.state","Insurance","Grade","T","N","AJCC","Group","Surgery"
           ,"Radiation","Chemotherapy")
nonvar <- c("Tumor.size")




table<- CreateTableOne(vars = myVars,       
                       factorVars = catVars, 
                       strata = "Group",
                       data = aa, 
                       addOverall = TRUE)  

table1<- print(table, 
               nonnormal = nonvar,
               catDigits = 2,contDigits = 3,pDigits = 4, 
               
               showAllLevels=TRUE, 
               quote = FALSE, 
               noSpaces = TRUE, 
               printToggle = TRUE) 

write.csv(table1, file = "F:/R语言/yixian33.csv")



library(survival)
library(plyr)

rm(list = ls()) 

aa<- read.csv('F:/R语言/yixian1.csv')

head(aa)

str(aa)

aa$status<-factor(aa$status)
summary(aa$Group)
y<- Surv(time = aa$Time,event = aa$State==1)


cox<- coxph(y~Radiation, data=aa);summary(cox)

Uni_cox1<- function(x){ FML<- as.formula(paste0 ("y~",x))#x就是输入的变量
cox<- coxph(FML,data=aa)
cox1<-summary(cox)
HR <- round(cox1$coefficients[,2],2)    
PValue <- round(cox1$coefficients[,5],3) 
CI5<-round(cox1$conf.int[,3],2)
CI95<-round(cox1$conf.int[,4],2)
Uni_cox<- data.frame(
  names<-rownames(cox1$conf.int),
  'HR'=HR,
  'CI5'=CI5,
  'CI95'=CI95,
  'P'=PValue
)
return(Uni_cox)} 

names(aa)


variable.names<- colnames(aa)[c(1,2,4,6,8,9,10,11,12,13,15,16,17)] ##选择需要进行单因素Cov分析的变量


cox3 <- lapply(variable.names, Uni_cox1)
cox3 <- ldply(cox3,data.frame)
cox3$HR.CI95<-paste0(cox3$HR,"(",cox3$CI5,'-',cox3$CI95,")");cox3
cox2<-cox3[,-2:-4]
colnames(cox2)[1]<-'Characteristics'
write.csv(cox2, file = "F:/R语言/单因素胰腺癌.csv")

for(i in names(aa)[c(1,2,4)]){aa[,i]<-as.factor(aa[,i])}
for(i in names(aa)[c(6:17)]){aa[,i]<-as.factor(aa[,i])}
for(i in names(aa)[c(12:18)]){aa[,i]<-as.factor(aa[,i])}
for(i in names(aa)[c(20,21)]){aa[,i]<-as.factor(aa[,i])}

summary(aa$AJCC)
mul_cox<-coxph(Surv(Time,State==1)~
                 Sex+Race+Age74+Grade+Surgery+
                 Radiation+Chemotherapy+T+N+Insurance+Marital.state+AJCC,
               data=aa
);summary(mul_cox)


cox<-summary(mul_cox) 
cox$coefficients    
cox$conf.int  
mul_HR<- round(cox$coefficients[,2],2) 
mul_PValue<- round(cox$coefficients[,5],4) 
mul_CI1<-round(cox$conf.int[,3],2)
mul_CI2<-round(cox$conf.int[,4],2)
mul_CI95<-paste(mul_CI1,'-',mul_CI2)

mul_cox1 <- data.frame("HR" =mul_HR,
                       "CI95" =mul_CI95,
                       "P"=mul_PValue);mul_cox1

rm(list = ls()) 
library(rms)
install.packages("rms")
aa<- read.csv('F:/R语言/yixian1.csv')
aa<- read.csv('F:/R语言/yixianspss.csv')
summary(aa$Surgery)
aa$Surgery<-factor(aa$Surgery,
                   levels = c("Extended pancreatoduodenectomy","Local excision of tumor","Local or pancreatectomy","No","Total pancreatectomy"),
                   labels = c("RP","LET","LP","No","TP"))


aa$Age<-factor(aa$Age,
               levels = c(1,2),
               labels = c("65-74",">74"))
aa$Grade<-factor(aa$Grade,
                 levels = c("1","2","3","4"),
                 labels = c("I","II","III","IV"))
aa$Insurance<-factor(aa$Insurance,
                     levels = c("0","1"),
                     labels = c("No","insured"))
aa$Surgery<-factor(aa$Surgery,
                   levels = c("0","1","2","3","4"),
                   labels  = c("No","EP","LET","LP","TP"))
aa$T<-factor(aa$T,
             levels = c("1","2","3"),
             labels = c("T1","T2","T3"))
aa$N<-factor(aa$N,
             levels = c("0","1"),
             labels = c("N0","N1"))
aa$Radiation<-factor(aa$Radiation,
                     levels = c("0","1"),
                     labels = c("No","Yes"))
aa$Chemotherapy<-factor(aa$Chemotherapy,
                        levels = c("0","1"),
                        labels = c("No","Yes"))
aa$AJCC<-factor(aa$AJCC,
                levels = c(1,2,3,4),
                labels=c("IA","IB","IIA","IIB"))
aa$Grade<-factor(aa$Grade)
aa$AJCC<-factor(aa$AJCC)
summary(aa$Age)
for(i in names(aa)[c(1:10)]){aa[,i]<-as.factor(aa[,i])}##批量转为因子factor
for(i in names(aa)[c(1,2)]){aa[,i]<-as.factor(aa[,i])}##批量转为因子factor
for(i in names(aa)[c(10:11)]){aa[,i]<-as.factor(aa[,i])}##批量转为因子factor
for(i in names(aa)[c(13:18)]){aa[,i]<-as.factor(aa[,i])}##批量转为因子factor
for(i in names(aa)[c(20,21)]){aa[,i]<-as.factor(aa[,i])}##批量转为因子factor
summary(aa$ajcc)
str(aa)

nomo<-datadist(aa)
options(datadist='nomo')
units(aa$time) <- "Month" 
nomo2 <- cph(Surv(time,state==1)~Age+Grade+Surgery+Radiation+Chemotherapy+T+N+Insurance+AJCC,
             x=T,y=T,
             data=aa,
             surv=T,
             time.inc = 12*5)

nomo1 <- cph(Surv(Time,State==1)~Age+Grade+Surgery+Radiation+Chemotherapy+T+N+Insurance+AJCC,
             x=T,y=T,
             data=aa,
             surv=T,
             time.inc = 12*5)

library(survival)
library(survminer)
nomo22 <- coxph(Surv(Time,State==1)~Age74+Race+Grade+Surgery+Radiation+Chemotherapy+T+N+Insurance,data=aa)
ccox<-cox.zph(nomo22)
print(ccox)
#options(repr.plot.width=10,repr.plot.heigh=40)
ggcoxzph(ccox)
#或者直接计算C指数,C-index =1-c；
Cindex <- rcorrcens(Surv(as.numeric(aa$time),aa$state==1)~predict(nomo2))
Cindex 


surv <- Survival(nomo2)
surv1 <- function(x)surv(12*1,lp=x)
surv2 <- function(x)surv(12*3,lp=x)
surv3 <- function(x)surv(12*5,lp=x)

nomo2<-nomogram(nomo2,
              
                fun=list(surv1,surv2,surv3),
             
                funlabel=c('1-year Survival',
                           '3-year Survival',
                           '5-year Survival'),
                
                lp =F, 
                
                maxscale=100,
                
                fun.at=c("0.95",'0.9','0.8',
                         '0.7','0.6','0.5','0.4',
                         '0.3','0.2','0.1')
);plot(nomo2)





library(rms)


rm(list = ls())
aa<- read.csv('F:/R语言/training.csv')
names(aa)
for(i in names(aa)[c(1:11)]){aa[,i]<-as.factor(aa[,i])}##批量转为因子factor
for(i in names(aa)[c(14,15)]){aa[,i]<-as.factor(aa[,i])}##批量转为因子factor
for(i in names(aa)[c(13:18)]){aa[,i]<-as.factor(aa[,i])}##批量转为因子factor
for(i in names(aa)[c(20,21)]){aa[,i]<-as.factor(aa[,i])}##批量转为因子factor

nomo<-datadist(aa)
options(datadist='nomo')


nomo1 <- cph(Surv(time,state==1)~age+race+grade+surgery+radiation+chemotherapy+t+n+insurance+ajcc,
             x=T,y=T,
             data=aa,
             surv=T,
             time.inc=12*1#示例数据time=月所以12*5就是评估5年的校准曲线
)
Cindex <- rcorrcens(Surv(as.numeric(aa$time),aa$state==1)~predict(nomo1))
Cindex$SD

p1<- calibrate(nomo1,
               cmethod='KM',
               method='boot',
               u=12*1,
               m=1100, 
               B=1000)

plot(p1,
     add=F,
     conf.int=T,
     subtitles = F,
     cex.subtitles=0.8, 
     lwd=2,
     lty=1,,
     errbar.col="blue",
     xlim=c(0,1),
     ylim=c(0,1),
     xlab="Nomogram Predicted CSS",
     ylab="Actual Survival",
     col="red")


nomo3 <- cph(Surv(time,state==1)~age+race+grade+surgery+radiation+chemotherapy+t+n+insurance+ajcc,
             x=T,y=T,
             data=aa,
             surv=T,
             time.inc=12*3
)

p<- calibrate(nomo3,
              cmethod='KM',
              method='boot',
              u=12*3,
              m=1100, 
              B=1000)


plot(p,
     add=F,
     conf.int=T,）
     subtitles = F,
     cex.subtitles=0.8, 
     lwd=2,
     lty=1,,
     errbar.col="blue",
     xlim=c(0.0,1),
     ylim=c(0.0,1),
     xlab="Nomogram Predicted CSS",
     ylab="Actual Survival",
     col="red")


nomo5 <- cph(Surv(time,state==1)~age+race+grade+surgery+radiation+chemotherapy+t+n+insurance+ajcc,
             x=T,y=T,
             data=aa,
             surv=T,
             time.inc=12*5
)

p5<- calibrate(nomo5,
               cmethod='KM',
               method='boot',
               u=12*5,
               m=1100, 
               B=1000)

plot(p,
     add=F,
     conf.int=T,
     subtitles = F,
     cex.subtitles=0.8, 
     lwd=2,
     lty=1,,
     errbar.col="blue",
     xlim=c(0.0,1),
     ylim=c(0.0,1),
     xlab="Nomogram predicted of CSS",
     ylab="Actual survival",
     col="red")


plot(p1,
     
     subtitles = F,
     lwd=2,lty=1,
     errbar.col="blue",xlim=c(0,1),ylim=c(0,1),
     xlab="Nomogram  Predicted Survival",ylab="Actual Survival",
     col="red")


plot(p,
     add=T,
     subtitles = F,
     lwd=2,lty=1,
     errbar.col="orange",xlim=c(0,1),ylim=c(0,1),
     xlab="Nomogram  Predicted Survival",ylab="Actual Survival",
     col="#407600")

plot(p5,
     add=T,
     subtitles = F,
     lwd=2,lty=1,
     errbar.col="#00ff00",xlim=c(0,1),ylim=c(0,1),
     xlab="Nomogram  Predicted Survival",ylab="Actual Survival",
     col="#9fc5e8")
legend("bottomright", legend=c("1 year", "3 year","5 year"), 
       col=c("red", "407600","#9fc5e8"), 
       lwd=3)





rm(list = ls()) #清理环境
aa<- read.csv('F:/R语言/test1.csv')
names(aa)


nomo<-datadist(aa)
options(datadist='nomo')
units(aa$time) <- "Month" 
coxm <- cph(Surv(time,state==1)~age+race+grade+surgery+rad+chem+t+n+insurance+ajcc,
            x=T,y=T,data=aa,surv=T)

Cindex <- rcorrcens(Surv(as.numeric(aa$time),aa$state==1)~predict(coxm))

coxm4 <- cph(Surv(time,state==1)~predict(coxm, newdata=aa),
             time.inc =12,x=T,y=T,data=aa,surv=T)
cal4 <- calibrate(coxm4, cmethod='KM', method='boot', u=12, m=490, B=1000) 
plot(cal4,
     subtitles = F,
     lwd=2,lty=1,
     errbar.col=c(rgb(0,118,192,maxColorValue=255)),xlim=c(0,1),ylim=c(0,1),
     xlab="Nomogram  Predicted  1-year  Survival",ylab="Actual Survival",
     col=c(rgb(192,98,83,maxColorValue=255)))
coxm5 <- cph(Surv(time,state==1)~predict(coxm, newdata=aa),
             time.inc =36,x=T,y=T,data=aa,surv=T)
cal5 <- calibrate(coxm5, cmethod='KM', method='boot', u=36, m=490, B=1000) 
plot(cal5,
     subtitles = F,
     lwd=2,lty=1,
     errbar.col=c(rgb(0,118,192,maxColorValue=255)),xlim=c(0,1),ylim=c(0,1),
     xlab="Nomogram  Predicted  3-year  Survival",ylab="Actual Survival",
     col=c(rgb(192,98,83,maxColorValue=255)))
coxm6 <- cph(Surv(time,state==1)~predict(coxm, newdata=aa),
             time.inc =60,x=T,y=T,data=aa,surv=T)
cal6 <- calibrate(coxm6, cmethod='KM', method='boot', u=60, m=490, B=1000) 
plot(cal6,
     subtitles = F,
     lwd=2,lty=1,
     errbar.col=c(rgb(0,118,192,maxColorValue=255)),xlim=c(0,1),ylim=c(0,1),
     xlab="Nomogram  Predicted  5-year  Survival",ylab="Actual Survival",
     col=c(rgb(192,98,83,maxColorValue=255)))


plot(cal4,
     
     subtitles = F,
     lwd=2,lty=1,
     errbar.col="blue",xlim=c(0,1),ylim=c(0,1),
     xlab="Nomogram  Predicted Survival",ylab="Actual Survival",
     col="red")


plot(cal5,
     add=T,
     subtitles = F,
     lwd=2,lty=1,
     errbar.col="orange",xlim=c(0,1),ylim=c(0,1),
     xlab="Nomogram  Predicted Survival",ylab="Actual Survival",
     col="#407600")

plot(cal6,
     add=T,
     subtitles = F,
     lwd=2,lty=1,
     errbar.col="#00ff00",xlim=c(0,1),ylim=c(0,1),
     xlab="Nomogram  Predicted Survival",ylab="Actual Survival",
     col="#9fc5e8")
legend("bottomright", legend=c("1 year", "3 year","5 year"), 
       col=c("red", "#407600","#9fc5e8"), 
       lwd=3)

library(rms)
library(ggDCA)
library(survival)  
library(ggprism)

rm(list = ls()) 


aa<- read.csv('F:/R语言/yixianspss.csv')
aa<- read.csv('F:/R语言/training.csv')
for(i in names(aa)[c(1:10)]){aa[,i]<-as.factor(aa[,i])}

bb<-datadist(aa)
options(datadist='bb')
model3 <- coxph(Surv(time,state==1)~age+grade+surgery+radiation+chemotherapy+t+n+insurance+ajcc,,data=aa)
model4 <- coxph(Surv(time,state==1)~t+n,data=aa)
d_train <- dca(model3,model4,times=c(12,36,60),
               model.names =c('Nomogram','TNM stage'))
ggplot(dca2,
       lwd = 1.2)

dca2<- dca(model3,model4,times=c(12,36,60),model.names =c('Nomogram','TNM stage'))
ggplot(dca2,
       lwd = 1.2)




rm(list = ls()) 
library(nomogramFormula)

library(survcomp) 



library(survival)
library(rms)


data1<-read.csv('F:/R语言/risk.csv')
data1$Surgery<-factor(data1$Surgery,
                      levels = c("Extended pancreatoduodenectomy","Local excision of tumor","Local or pancreatectomy","No","Total pancreatectomy"),
                      labels = c("RP","LET","LP","No","TP"))



for(i in names(data1)[c(1:10)]){data1[,i]<-as.factor(data1[,i])}
data1$status<-factor(data1$status)
dd<-datadist(data1)
options(datadist='dd')
units(data1$time) <- "Month" 
coxmpoint <- cph(Surv(time,status==1)~age+race+grade+sur+rad+che+t+n+ins, 
                 x=T,y=T,data=data1,surv=T)

surv <- Survival(coxmpoint)

surv1 <- function(x)surv(1*12,lp=x)  
surv2 <- function(x)surv(1*36,lp=x)
surv3 <- function(x)surv(1*60,lp=x)
nompoint<-nomogram(coxmpoint,fun=list(surv1,surv2,surv3),lp = F,
                   funlabel=c('1-year survival ',
                              '3-year survival ',
                              '5-year survival '),
                   maxscale=100,
                   fun.at=c('0.95','0.9','0.8','0.70','0.6','0.5','0.4','0.3','0.2','0.1'))
plot(nompoint,xfrac = .3)

results<-formula_rd(nomogram=nompoint)
data1<-na.omit(data1)
data1$points<-points_cal(formula = results$formula,rd=data1)

write.csv(data1,"F:/R语言/总体风险值.csv")

data3<-read.csv("F:/R语言/总体风险值1.csv")
data3<-read.csv("F:/R语言/总体风险值.csv")
head(data3)


library(survivalROC)
library(survminer)
library(survival)
for(i in names(data3)[c(2:10)]){data3[,i]<-as.factor(data3[,i])}
for(i in names(data3)[c(11,16)]){data3[,i]<-as.factor(data3[,i])}
cutoff<-surv_cutpoint(data3, 
                      time="time",
                      event="status",
                      variables="points"
);summary(cutoff) 


fit <- survfit(Surv(time,status) ~ risk,  
               data = data3) 
summary(fit)
ggsurvplot(fit, 
           data = data3, 
           conf.int = TRUE, 
           con.int.style="step",
           conf.int.alpha=0.3,
           xlim=c(0,83),
           pval = TRUE, 
           pval.size=5,
           pval.coord=c(0.4,0.4),
           
           risk.table.pos ="out",
           #risk.table="abs_pct",
           surv.median.line = "hv",  
           risk.table = TRUE,
           xlab = "Follow up time(Months)", 
           legend = c(0.15,0.2), 
           legend.title = "", 
           legend.labs = c("Risk-high", "Risk-low"),
           font.legend=15,  
           break.x.by = 20, 
           palette="lancet") 
####
fit1 <- survfit(Surv(time, status)~risk, data=data3)
ggsurvplot(fit, 
           data = data3,    
           pval=TRUE,        
           pval.method=TRUE,  
           palette = "lancet",
           risk.table = TRUE, 
           conf.int = TRUE)   



rm(list = ls()) 
library(nomogramFormula)

library(survcomp) 

library(survival)
library(rms)

data1<-read.csv('F:/R语言/training1.csv')
data1$Surgery<-factor(data1$Surgery,
                      levels = c("Extended pancreatoduodenectomy","Local excision of tumor","Local or pancreatectomy","No","Total pancreatectomy"),
                      labels = c("RP","LET","LP","No","TP"))

dd<-datadist(data1)
options(datadist='dd')
units(data1$time) <- "Month" 

library(survival)
library(survminer)
nomo22 <- coxph(Surv(time,state==1)~age+race+grade+surgery+rad+chem+t+n+insurance,data=data1)
ccox<-cox.zph(nomo22)
print(ccox)
options(repr.plot.width=10,repr.plot.heigh=40)
ggcoxzph(ccox)

coxmpoint <- cph(Surv(time,state==1)~age+race+grade+surgery+rad+chem+t+n+insurance, 
                 x=T,y=T,data=data1,surv=T)

surv <- Survival(coxmpoint)

surv1 <- function(x)surv(1*12,lp=x)  
surv2 <- function(x)surv(1*36,lp=x)
surv3 <- function(x)surv(1*60,lp=x)
nompoint<-nomogram(coxmpoint,fun=list(surv1,surv2,surv3),lp = F,
                   funlabel=c('1-year survival ',
                              '3-year survival ',
                              '5-year survival '),
                   maxscale=100,
                   fun.at=c('0.95','0.9','0.8','0.70','0.6','0.5','0.4','0.3','0.2','0.1'))
plot(nompoint,xfrac = .3)

results<-formula_rd(nomogram=nompoint)
data1<-na.omit(data1)
data1$points<-points_cal(formula = results$formula,rd=data1)

write.csv(data1,"F:/R语言/训练集风险值.csv")


library(survivalROC)
library(survminer)
library(survival)
for(i in names(data3)[c(2:12)]){data3[,i]<-as.factor(data3[,i])}
for(i in names(data3)[c(11,15)]){data3[,i]<-as.factor(data3[,i])}
cutoff<-surv_cutpoint(data1, 
                      time="time",
                      event="state",
                      variables="points"
);summary(cutoff) 

data3<-read.csv("F:/R语言/训练集风险值.csv")
head(data3)
fit <- survfit(Surv(time,state) ~ risk,  
               data = data3) 
summary(fit)
ggsurvplot(fit, 
           data = data3,  
           conf.int = TRUE, 
           con.int.style="step",
           conf.int.alpha=0.3,
           xlim=c(0,83),
           pval = TRUE,
           pval.size=5,
           pval.coord=c(0.4,0.4), 
           
           risk.table.pos ="out",
           
           surv.median.line = "hv",  
           risk.table = TRUE,
           xlab = "Follow up time(Months)",
           legend = c(0.15,0.2), 
           legend.title = "", 
           legend.labs = c("Risk-high", "Risk-low"),
           break.x.by = 20, 
           palette="lancet") 


rm(list = ls()) 
library(nomogramFormula)

library(survcomp) 

library(survival)
library(rms)


data1<-read.csv('F:/R语言/test.csv')
data1$Surgery<-factor(data1$Surgery,
                      levels = c("Extended pancreatoduodenectomy","Local excision of tumor","Local or pancreatectomy","No","Total pancreatectomy"),
                      labels = c("RP","LET","LP","No","TP"))

dd<-datadist(data1)
options(datadist='dd')
units(data1$time) <- "Month" 
coxmpoint <- cph(Surv(time,state==1)~age+race+grade+surgery+rad+chem+t+n+insurance, 
                 x=T,y=T,data=data1,surv=T)

surv <- Survival(coxmpoint)

surv1 <- function(x)surv(1*12,lp=x)  
surv2 <- function(x)surv(1*36,lp=x)
surv3 <- function(x)surv(1*60,lp=x)
nompoint<-nomogram(coxmpoint,fun=list(surv1,surv2,surv3),lp = F,
                   funlabel=c('1-year survival ',
                              '3-year survival ',
                              '5-year survival '),
                   maxscale=100,
                   fun.at=c('0.95','0.9','0.8','0.70','0.6','0.5','0.4','0.3','0.2','0.1'))
plot(nompoint,xfrac = .3)

results<-formula_rd(nomogram=nompoint)
data1<-na.omit(data1)
data1$points<-points_cal(formula = results$formula,rd=data1)

write.csv(data1,"F:/R语言/验证集风险值.csv")


library(survivalROC)
library(survminer)
library(survival)
for(i in names(data3)[c(2:12)]){data3[,i]<-as.factor(data3[,i])}
for(i in names(data3)[c(11,15)]){data3[,i]<-as.factor(data3[,i])}
cutoff<-surv_cutpoint(data1, 
                      time="time",
                      event="state",
                      variables="points"
);summary(cutoff) 

data3<-read.csv("F:/R语言/验证集风险值.csv")
head(data3)
fit <- survfit(Surv(time,state) ~ risk, 
               data = data3) 
summary(fit)
ggsurvplot(fit, 
           data = data3,  
           conf.int = TRUE, 
           con.int.style="step",
           conf.int.alpha=0.3,
           xlim=c(0,83),
           pval = TRUE, 
           pval.size=5,
           pval.coord=c(0.4,0.4), 
           
           risk.table.pos ="out",
           
           surv.median.line = "hv", 
           risk.table = TRUE,
           xlab = "Follow up time(Months)",
           legend = c(0.15,0.2), 
           legend.title = "", 
           legend.labs = c("Risk-high", "Risk-low"), 
           font.legend=15,  
           break.x.by = 20, 
           palette="lancet") 






library(rms)
library(DynNom)

rm(list = ls()) 
aa<- read.csv('F:/R语言/yixianspss.csv')
str(aa)

nomo<-datadist(aa)
options(datadist='nomo')
units(aa$time) <- "Month" 


nomo1 <- cph(Surv(time,state==1)~Age+Grade+Surgery+Radiation+Chemotherapy+T+N+Insurance+AJCC,
             x=T,y=T,
             data=aa,
             surv=T,
             time.inc=12*1
)

surv <- Survival(nomo1)
surv1 <- function(x)surv(12*1,lp=x)
surv2 <- function(x)surv(12*3,lp=x)
surv3 <- function(x)surv(12*5,lp=x)
nomo2<-nomogram(nomo1,
            
                fun=list(surv1,surv2,surv3),
         
                funlabel=c('1-year Survival ',
                           '3-year Survival',
                           '5-year Survival'),
         
                lp =F, 
          
                maxscale=100,
             
                fun.at=c("0.99","0.95",'0.9','0.8',
                         '0.7','0.6','0.5','0.4',
                         '0.3','0.2','0.1')
);plot(nomo2)

DynNom(nomo1) 

DNbuilder(nomo1) 

install.packages('rsconnect')

rsconnect::setAccountInfo(name='yixianainomogram1',
                          token='0E20A3D359CB865BCE1FEF0B563F2E79',
                          secret='mhBLzfoZH9xxjZ27dfUCQi6rfDn7vEYistJ3OmrV')




