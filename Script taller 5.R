#install.packages("easypackages")        # Libreria especial para hacer carga automática de librerias
library("easypackages")

lib_req<-c("car","lmtest","visdat","corrplot","MASS","olsrr")# Listado de librerias requeridas por el script# Listado de librerias requeridas por el script
easypackages::packages(lib_req) 

if(!require('readxl')) {
  install.packages("readxl")
  library("readxl")
}

wd="C:/Users/Andres/Documents/Metodos/Taller 5"       # Establecer el directorio de trabajo
setwd(wd)

dicosSet <- read_xlsx("Discos.xlsx")
summary(dicosSet)
dim(dicosSet)
#### Punto 1.1####
table(Conf)             
DiscoConfCero=dicosSet[Conf!=1,]              
DiscoConfUno=dicosSet[Conf!=0,]


color=c("red", "Blue") 
attach(DiscoConfCero) 
windows(height=10,width=15)
plot(Tiempo~Carga,pch=20,ylab="Tiempo",cex.lab=1.4) # grafica medv en funcion de estrato bajo
r = cor(Tiempo,Carga,method = c("pearson"),use="pairwise.complete.obs")# calculo  coeficiente de pearson
text(9,3, paste("r = ", round(r,2) ), cex=1.4) #pone en el grafico el valor de r
cor.test(Tiempo,Carga,alternative = c("less"), method = c("spearman"), conf.level = 0.95) # averiguo el valor de p, me dio muy bajo, rechazo la hipotesis nula, significa que no es cuestion de asar la corelacion que existe, si vuelvo a tomar una muestra, me va a dar la misma correlacion

with(dicosSet[dicosSet$Conf=="0",],{
  points(Carga,Tiempo,col=color[2],pch=20,ylab="Tiempo",cex=0.5)
  lines(smooth.spline(Carga,Tiempo,df=3),col=color[2])
})
detach(DiscoConfCero)

attach(DiscoConfUno) 
windows(height=10,width=15)
plot(Tiempo~Carga,pch=20,ylab="Tiempo",cex.lab=1.4) # grafica medv en funcion de estrato bajo
r = cor(Tiempo,Carga,method = c("pearson"),use="pairwise.complete.obs")# calculo  coeficiente de pearson
text(9,3, paste("r = ", round(r,2) ), cex=1.4) #pone en el grafico el valor de r
cor.test(Tiempo,Carga,alternative = c("less"), method = c("spearman"), conf.level = 0.95) # averiguo el valor de p, me dio muy bajo, rechazo la hipotesis nula, significa que no es cuestion de asar la corelacion que existe, si vuelvo a tomar una muestra, me va a dar la misma correlacion

with(dicosSet[dicosSet$Conf=="1",],{
  plot(Carga,Tiempo,col=color[1],pch=20,ylab="Tiempo",cex=0.5)
  lines(smooth.spline(Carga,Tiempo,df=3),col=color[1])
})
detach(DiscoConfUno)

legend("topleft",c("Conf 1" , "Conf 0"), col= color,lty=1,bty="n")
detach(dicosSet)
####fin punto 1.1####
#________________________________________Punto 1.2________________

####Punto 1.2####

attach(dicosSet) 
windows(height=10,width=15)
plot(Tiempo~Carga,pch=20,ylab="Tiempo",cex.lab=1.4) # grafica medv en funcion de estrato bajo
r = cor(Tiempo,Carga,method = c("pearson"),use="pairwise.complete.obs")# calculo  coeficiente de pearson
text(10,1, paste("r = ", round(r,2) ), cex=1.4) #pone en el grafico el valro de r
cor.test(Tiempo,Carga,alternative = c("less"), method = c("spearman"), conf.level = 0.95) # averiguo el valor de p, me dio muy bajo, rechazo la hipotesis nula, significa que no es cuestion de asar la corelacion que existe, si vuelvo a tomar una muestra, me va a dar la misma correlacion
detach(dicosSet)

Model_s1 = lm(Tiempo~Carga, data=dicosSet) #lm=lineal, madv= variable respuesta, lstat= variable predictiva
Model_s1
anova(Model_s1)
summary(Model_s1)

attach(dicosSet)
windows(height=10,width=15)
plot(Tiempo~Carga,pch=20,ylab="Tiempo",cex.lab=1.4)
abline(Model_s1, col="red", lty=2, lwd=2)
r = cor(Tiempo,Carga,method = c("pearson"),use="pairwise.complete.obs")# calculo  coeficiente de pearson
text(10,1, paste("r = ", round(r,2) ), cex=1.4)
detach(dicosSet)


windows()
par(mfrow=c(1,2))
residualPlot(Model_s1,variable="fitted",type = "rstudent",pch=20) # fitted, estimaciones de ella misma
residualPlot(Model_s1,variable="Carga",type = "rstudent",pch=20)

#---------------------------------------------------##
####punto 1.3####
Model_s2 = lm(Tiempo~Carga+Conf*Carga, data=dicosSet) 

Model_s2
anova(Model_s2)
summary(Model_s2)
####punto 1.4####
anova(Model_s1,Model_s2)
####punto 1.5
color=c("red", "Blue")
attach(dicosSet)
windows(height=10,width=15)
plot(Carga,Tiempo,pch=20,ylab="Tiempo",cex=0.5,col=ifelse(Conf=="1",color[2],color[1]))
points(Carga,fitted(Model_s2),lty=2,lwd=2,pch=20,col=ifelse(Conf=="0",color[2],color[1]))
detach(dicosSet)
legend("topright",c("Conf = 1" , "Conf = 0"), col= color,lty=1,bty="n")

#grafica del modelo 1 y modelo 2
attach(dicosSet)
windows(height=10,width=15)
plot(Tiempo~Carga,pch=20,ylab="Tiempo",cex.lab=1.4)
abline(Model_s2, col="red", lty=2, lwd=2)
abline(Model_s1, col="blue", lty=2, lwd=2)
r = cor(Tiempo,Carga,method = c("pearson"),use="pairwise.complete.obs")# calculo  coeficiente de pearson
text(10,1, paste("r = ", round(r,2) ), cex=1.4)
legend("bottomright",c("Modelo 2" , "Modelo 1"), col= color,lty=1,bty="n")
detach(dicosSet)

attach(dicosSet)
windows(height=10,width=15)
plot(Tiempo~Carga,pch=20,ylab="Tiempo",cex.lab=1.4)
points(Carga,fitted(Model_s2),col="red",lty=2,lwd=2,pch=20)
detach(dicosSet)

#Reciduos

residuos=rstudent(Model_s2)
windows()
par(mfrow=c(2,2)) 
plot(Model_s2)
windows()
car::influenceIndexPlot(Model_s2, vars="Cook")
t.test(residuos, alternative="two.sided")
bptest(Model_s2)
dwtest(Model_s2)
shapiro.test(residuos)

#____________________________________________________________________
####punto 2####

wd="C:/Users/Andres/Documents/Metodos/Taller 5"       # Establecer el directorio de trabajo
setwd(wd)

diabetesSet <-read.table(file = "diabetes.txt", header = TRUE)

color=c("Gray", "Blue")    # Define los Colores para barrios que no atraviesa y que atraviesa el rio charles 

attach(diabetesSet)
windows(height=10,width=15)
par(mfrow=c(3,5)) 
boxplot(Y~SEX,col=color)
lapply(colnames(diabetesSet[,-c(4,14)]),function(y){
  plot(diabetesSet[SEX=="1",y],diabetesSet[SEX=="1","Y"],col=color[1],pch=20,ylab="Y",xlab=y)
  points(diabetesSet[SEX=="2",y],diabetesSet[SEX=="2","Y"],col=color[2],pch=20)
  lines(smooth.spline(diabetesSet[,y],diabetesSet[,"Y"],df=3),col="Black")
})   
detach(diabetesSet)

### Matriz de correlación - todas las variables cuantitativas
AQ.cor = cor(diabetesSet[,-2],method="pearson")
p.cor=cor.mtest(diabetesSet[,-2],method="pearson")[[1]]
print(AQ.cor)
windows(height=10,width=15)
corrplot::corrplot(AQ.cor, method = "ellipse",addCoef.col = "black",type="upper",p.mat=p.cor,order="hclust")

formula_sat = as.formula(Y~.);
Model_sat = lm(formula_sat,data  = diabetesSet)         # Modelo inicial, con toda las variables
summary(Model_sat)

windows()
vif=car::vif(Model_sat)
barplot(sort(vif),cex.names = 0.6,col=ifelse(sort(vif)>4,"red","gray"),main="Factor Inflación de Varianza",ylab="VIF")


### Selección de variables, backward, forward, both
formula_sat  = as.formula(Y~.);  #~. Para que incluya todas las variables predictivas 
formula_null = as.formula(Y~1)# ~1, para definir el modelo nulo, sin ninguna variable
Model_null = lm(formula_null,data  = diabetesSet)   # Modelo Nulo sin variable explicativas

summary(Model_null)

Model_bw = step(Model_sat, direction='backward', scope=formula_null) 
Model_fw = step(Model_null, direction='forward', scope=formula_sat) 
Model_bfw = step(Model_null, direction='both', scope=formula_sat)

summary(Model_bw)
summary(Model_fw)
summary(Model_bfw)

#Evaluando los supuestos del error

residuos=rstudent(Model_bw)
windows()
par(mfrow=c(2,2)) 
plot(Model_bw)
windows()
car::influenceIndexPlot(Model_bw, vars="Cook")
t.test(residuos, alternative="two.sided")
bptest(Model_bw)
dwtest(Model_bw)
shapiro.test(residuos)

