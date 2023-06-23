library("easypackages")
library(dplyr)
library(corrplot)
library(vcd)
library(GGally)

lib_req<-c("MASS","visdat","car","caret","pROC","class")# Listado de librerias requeridas por el script
easypackages::packages(lib_req)

credit_customers <- read.csv("C:/Users/aleja/Downloads/taller final R/credit_customers.csv", stringsAsFactors=TRUE)

str(credit_customers)                           
windows(height=10,width=15)
visdat::vis_miss(credit_customers)

attach(credit_customers)
#Preparación de los datos categóricos a numéricos ordinales
#1
unique(checking_status)
checking_status <- factor(checking_status, ordered = TRUE, levels = c("no checking", "<0", "0<=X<200",">=200"))
credit_customers$checking_status <- as.numeric(checking_status)

#2
unique(credit_history)
credit_history <- factor(credit_history, ordered = TRUE, levels = c("critical/other existing credit", 
                                                                    "delayed previously", "existing paid",
                                                                   "no credits/all paid","all paid"))
credit_customers$credit_history <- as.numeric(credit_history)

#3
unique(savings_status)
savings_status <- factor(savings_status, ordered = TRUE, levels = c("no known savings", "<100", "100<=X<500",
                                                                    "500<=X<1000",">=1000"))
credit_customers$savings_status <- as.numeric(savings_status)

#4
unique(employment)
employment <- factor(employment, ordered = TRUE, levels = c("unemployed", "<1", "1<=X<4",
                                                                    "4<=X<7",">=7"))
credit_customers$employment <- as.numeric(employment)

#5
unique(job)
job <- factor(job, ordered = TRUE, levels = c("unemp/unskilled non res", "unskilled resident", "skilled",
                                                            "high qualif/self emp/mgmt"))
credit_customers$job <- as.numeric(job)

###### FIN PREPARACIÓN DATOS  #########

#Matriz de Correlación
credit_customers$class <- as.numeric(class)-1
columnas <- c("checking_status", "duration", "credit_history", "credit_amount", "savings_status", "employment", 
              "installment_commitment","age", "existing_credits", "job", "num_dependents","class")
matriz_cor <- cor(credit_customers[, columnas])
p.cor=corrplot::cor.mtest(credit_customers[, columnas])$p


windows(height=10,width=15)
corrplot(matriz_cor, method = "color")



color=ifelse(class=="good","Black","Red")
windows(width=10, height=7)
par(mfrow=c(1,2))
plot(as.numeric(savings_status),as.numeric(class)-1,col=color,pch=20,ylab="Type",ylim=c(-0.5,1.5),
     cex=2, main="Modelo de Regresión Lineal")
mod_lin=lm((as.numeric(class)-1)~as.numeric(savings_status))
abline(h=c(0,1), lty=2,col="gray")
abline(mod_lin,lty=2,lwd=2,col="Blue")



plot(as.numeric(credit_history),as.numeric(class)-1,col=color,pch=20,ylab="Type",ylim=c(-0.5,1.5),
     cex=2, main="Modelo de Regresión Lineal")
mod_lin=lm((as.numeric(class)-1)~credit_history)
abline(h=c(0,1), lty=2,col="gray")
abline(mod_lin,lty=2,lwd=2,col="Blue")


windows(width=10, height=7)
par(mfrow=c(1,2))
plot(age,as.numeric(class)-1,col=color,pch=20,ylab="Type",ylim=c(-0.5,1.5),
     cex=2, main="Modelo de Regresión Lineal")
mod_lin=lm((as.numeric(class)-1)~age)
abline(h=c(0,1), lty=2,col="gray")
abline(mod_lin,lty=2,lwd=2,col="Blue")

plot(age,as.numeric(class)-1,col=color,pch=20,cex=2, main="Modelo de Regresión Logística")
mod_log=glm(class~age, family = "binomial")
points(age,predict(mod_log,type = "response"),pch=20,col="Blue")
abline(h=c(0,1), lty=2,col="gray")
abline(h=0.5, col="gray")

Modelo1 <- glm(formula= class~savings_status, data=credit_customers,family = "binomial")  # Ajusta un Modelo de regresión logistica simple
summary(Modelo1) 

resultado <- chisq.test(class, savings_status)

Modelo2 <- glm(formula= class~checking_status, data=credit_customers,family = "binomial")  # Ajusta un Modelo de regresión logistica simple
summary(Modelo2) 

Modelo3 <- glm(formula= class~duration, data=credit_customers,family = "binomial")  # Ajusta un Modelo de regresión logistica simple
summary(Modelo3) 

resultado2 <- chisq.test(class, credit_history)


Modelo_Sat = glm(formula= class~., data=credit_customers,family = "binomial")  # Ajusta un Modelo saturad --> todas las variables
summary(Modelo_Sat)                                               # Verifica la significancia individual


## usando la función step de R
Modelo_Back=step(Modelo_Sat,direction = "backward")
summary(Modelo_Back)


anova(Modelo_Sat,Modelo_Back,test="Chisq")
anova(Modelo2,Modelo_Back,test="Chisq") 
resultado <- chisq.test(credit_customers$class, credit_customers$savings_status)
resultado

Prob_RL1  = predict(Modelo_Back,type = "response")
# Curva ROC: Explora un mejor punto de corte para el Modelo de regresión Logistica
roc <- pROC::roc(class,Prob_RL1, auc = TRUE, ci = TRUE)
print(roc)
windows(height=10,width=15)
pROC::plot.roc(roc, legacy.axes = TRUE, print.thres = "best", print.auc = TRUE,
               auc.polygon = FALSE, max.auc.polygon = FALSE, auc.polygon.col = "gainsboro",
               col = 2, grid = TRUE) 

# Calidad de la Clasificacion en el modelo múltiple
pc=0.696
Predict_RLM1 = as.factor(ifelse(Prob_RL1>pc,"good","bad")) 

# Indicadores de correcta clasificación.
caret::confusionMatrix(Predict_RLM1,class,positive = "good")
ICC_RLM1=caret::confusionMatrix(Predict_RLM1,class,positive = "good")
Ind_class=list()
Ind_class$RLM1=ICC_RLM1$byClass
