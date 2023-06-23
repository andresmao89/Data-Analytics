install.packages("easypackages")        # Libreria especial para hacer carga automática de librerias
library("easypackages")

lib_req<-c("MASS","visdat","car","HSAUR2","corrplot","plotrix")# Listado de librerias requeridas por el script
easypackages::packages(lib_req) 

if(!require('readxl')) {
  install.packages("readxl")
  library("readxl")
}
wd="C:/Users/Andres/Documents/Metodos/Taller 4"       # Establecer el directorio de trabajo
setwd(wd)

pibSet <- read_xlsx("PIBpc.xlsx")
summary(pibSet)
dim(pibSet)

#Se cambia los nombres de las columnas para comodidad visual
nombres = c("Departamento","Abreviatura","Agro","Minería","Manofactura","Servicios Publicos","Construcción",
            "Comercio","TIC","Financiera","Inmobiliario", "Profesionales", "Seguridad Social","Recreación",
            "Impuestos")
colnames(pibSet) <-nombres

nombre_filas= c(pibSet$Abreviatura)
rownames(pibSet) <- nombre_filas


windows()
visdat::vis_miss(pibSet) # No hay datos faltantes

Resumen= rbind(apply(pibSet[,3:15],2,"mean"),  
               apply(pibSet[,3:15],2,"sd"))  
rownames(Resumen)=c("Promedio", "Desviación")
print(Resumen,2)

windows(height=10,width=15)
par(mfrow=c(3,5))
lapply(colnames(pibSet[,3:15]),function(y){
  boxplot(pibSet[,y],ylab=y,cex=1.5,pch=20,col="blue")
})

windows()
pairs(pibSet[,3:15],pch=20,cex=1.5,lower.panel = NULL) # graficas de correlacion mostrando solo el panel superior.

#Matriz de Correlación
M.cor = cor(pibSet[,3:15],method="pearson") 
print(M.cor,2)

#Test de Significancia
p.cor=corrplot::cor.mtest(pibSet[,3:15])$p
print(p.cor,4)

#Elipses de Correlación
windows(height=10,width=15)
corrplot::corrplot(M.cor, method = "ellipse",addCoef.col = "black",type="upper", #upper, solo la parte superior
                   col=c("blue","red"),diag=FALSE,#order="hclust", #diagonal False para que no muestre las correaciones entre ella misma, "hclust" hace cluster entre las variables
                   p.mat = p.cor, sig.level = 0.01, insig = "blank" ) # sig.level=0.01, es el valor minimo para que me muestre correlacion, siendo bastante exigente 0.01 

#Teniendo en cuenta el gráfico boxplot, analizamos las columnas con mayor número de datos atípicos
  
valor.atipico=boxplot.stats(pibSet$"Seguridad Social")$out 
atipicos.SeguridadS = sapply(valor.atipico, function(y){which(pibSet$"Seguridad Social"==y)})
which(pibSet$"Seguridad Social"==valor.atipico)

valor.atipico=boxplot.stats(pibSet$Construcción)$out 
atipicos.Construccion = sapply(valor.atipico, function(y){which(pibSet$Construcción==y)})
which(pibSet$Construcción==valor.atipico)

valor.atipico=boxplot.stats(pibSet$Impuestos)$out 
atipicos.Recreación = sapply(valor.atipico, function(y){which(pibSet$Impuestos==y)})
which(pibSet$Impuestos==valor.atipico)

valor.atipico=boxplot.stats(pibSet$Mineria)$out 
atipicos.mineria = sapply(valor.atipico, function(y){which(pibSet$Mineria==y)})

valor.atipico=boxplot.stats(pibSet$Comercio)$out 
atipicos.comercio = sapply(valor.atipico, function(y){which(pibSet$Comercio==y)})
which(pibSet$Comercio==valor.atipico)

valor.atipico=boxplot.stats(pibSet$TIC)$out 
atipicos.TIC = sapply(valor.atipico, function(y){which(pibSet$Comercio==y)})
which(pibSet$TIC==valor.atipico)

valor.atipico=boxplot.stats(pibSet$Financiera)$out 
atipicos.financiera = sapply(valor.atipico, function(y){which(pibSet$Financiera==y)})
which(pibSet$Financiera==valor.atipico)

valor.atipico=boxplot.stats(pibSet$Profesionales)$out 
atipicos.Profesionales = sapply(valor.atipico, function(y){which(pibSet$Profesionales==y)})
which(pibSet$Profesionales==valor.atipico)

#Matriz de Correlación
M.cor = cor(pibSet[-3,3:15],method="pearson") 
print(M.cor,2)

#Test de Significancia
p.cor=corrplot::cor.mtest(pibSet[-3,3:15])$p
print(p.cor,4)

#Elipses de Correlación
windows(height=10,width=15)
corrplot::corrplot(M.cor, method = "ellipse",addCoef.col = "black",type="upper", #upper, solo la parte superior
                   col=c("blue","red"),diag=FALSE,#order="hclust", #diagonal False para que no muestre las correaciones entre ella misma, "hclust" hace cluster entre las variables
                   p.mat = p.cor, sig.level = 0.01, insig = "blank" ) # sig.level=0.01, es el valor minimo para que me muestre correlacion, siendo bastante exigente 0.01 


# Componentes Principales
#X=data[-c(atipicos.mineria), ]   #se eliminan los datos atípicos

X=pibSet [-3,3:15]
PCA=prcomp(X,center=TRUE,scale=TRUE)   # centrar y escalar para para quitarle la diferencia de escala  y que todas las variables aporten en la misma escala a mi sistema de variables , no tener una variable de mas varianza que vaya a dominar el resultado Estandariza la matriz
PCA
summary(PCA)


PCA$sdev     
Var_exp=cumsum(PCA$sdev^2)*100/sum(PCA$sdev^2) #Cada valor propio lo elevo al cuadrado, para tener la varianza, y hago una suma acumulada y divido sobre la suma total. Lo cual es el PORCENTAJE DE VARIANZA EXPLICADA
Var_exp

windows(height=10,width=15)
par(mfrow=c(1,2))
coord=barplot(PCA$sdev^2, xlab="Componente",ylab="Valor Propio")
lines(coord,PCA$sdev^2,col="blue",lwd=2)
abline(h=1,col="red", lty=2)
coord=barplot(Var_exp, xlab="Componente",ylab="Varianza Acumulada")
lines(coord,Var_exp,col="blue",lwd=2)
text(coord,Var_exp,round(Var_exp,2), pos=3,cex=0.6)

# Se seleccionan 3 componentes,  recogen el 79.48% de la Varianza Explicada
print(PCA$rotation[,1:3],3)

windows(height=10,width=15)
par(mfrow=c(3,1))
barplot(PCA$rotation[,1],ylim=c(-0.6,0.6),col=ifelse(PCA$rotation[,1]>0,"green","red"),
        main="Coeficientes estimados PC1")
barplot(PCA$rotation[,2],ylim=c(-0.6,0.6),col=ifelse(PCA$rotation[,2]>0,"green","red"),
        main="Coeficientes estimados PC2")
barplot(PCA$rotation[,3],ylim=c(-0.6,0.6),col=ifelse(PCA$rotation[,2]>0,"green","red"),
        main="Coeficientes estimados PC3")

F_PCA=predict(PCA)[,1:2] #Le pone los puntajes de cada unos de las final en cada PC


#Se eligen solo 2 de los PC
Competitividad = predict(PCA)[,1]
Comp.AgroMinera =predict(PCA)[,2]
name_row=c("Anti","Atla","Boli","Boya","Cald","Caqu","Cauc","Cesa","Cord","Cund","Choc","Huil","La G","Magd","Meta","Nari","Nort","Quin","Risa","Sant","Sucr","Toli","Vall","Arau","Casa","Putu","San","Amaz","Guai","Guav","Vaup","Vich")


windows(height=10,width=15)
par(mfrow=c(1,2))
dotchart(Competitividad,labels=name_row,pch=20,cex.lab=0.5, main= "PC1 : Competitividad",
         cex.lab=0.8)
abline(v=0,col="red",lty=2)
dotchart(Comp.AgroMinera,pch=20,name_row, main= "PC2 : Competitividad Agrícola y minera",
         cex.lab=0.8)
abline(v=0,col="red",lty=2)

windows(height=10,width=15)
plot(Competitividad, Comp.AgroMinera,pch=20,xlab="PC1 : Competitividad",ylab="PC2 : Competitividad Agrícola y minera")
abline(h=0,v=0,lty=2, col="red")
text(Competitividad, Comp.AgroMinera,name_row,cex=0.8,col="gray",pos=3)

# Representación simultánea de individuos y variables
windows(height=10,width=15)
biplot(PCA,col=c("gray","blue"),cex=0.8,xlim=c(-0.4,0.4), ylim=c(-0.4,0.4))
abline(h=0,v=0,lty=2, col="red")

Bog_PF=predict(PCA,newdata=pibSet[3,])[1:2]
Bog_PF

windows(height=10,width=15)
plot(Competitividad, Comp.AgroMinera,pch=20,xlab="PC1 : Competitividad",ylab="PC2 : Competitividad Agrícola y minera",xlim=c(-15,15))
abline(h=0,v=0,lty=2, col="red")
text(Competitividad, Comp.AgroMinera,name_row,cex=0.8,col="gray",pos=3)
points(Bog_PF[1],Bog_PF[2],pch=20,col="red")
text(Bog_PF[1],Bog_PF[2],"Bogotá",col="red")

F_PCA=rbind(F_PCA,Bog_PF) # Vamos a hacer el cluster no por pibSet, sino por F_PCA, agregamos Bogota

###librerias###
library("easypackages")

lib_req<-c("MASS","visdat","car","HSAUR2","corrplot","plotrix","cluster","factoextra", "FactoMineR")# Listado de librerias requeridas por el script
easypackages::packages(lib_req)


K=5                      # Inicialmente definiremos 3 grupos

km_clusters5 <- kmeans(x = F_PCA, centers = K, nstart = 50,iter.max=1000) # ejecución algortimo Kmeans
km_clusters5                      # Resumen resultados de la agrupación 
Grupos5=km_clusters5$cluster       # Etiquetado de grupo para cada individuo de la hoja de datos

s5 = silhouette(Grupos5, dist(F_PCA))     # Silhoutte plot, distancia de todos los componente principales (dist(F_PCA))
windows()
plot(s5)

# Evaluación del Número adecuado de cluster 

Evaluar_k=function(n_clust,data,iter.max,nstart){
  km <- kmeans(x = data, centers = n_clust, nstart = nstart,iter.max=iter.max)# Guarda las sumas del cuadrado del error
  return(km$tot.withinss)
}

k.opt=2:10
Eval_k=sapply(k.opt,Evaluar_k,data=F_PCA,iter.max=1000,nstart=50)

windows(height=10,width=15)
plot(k.opt,Eval_k,type="l",xlab="Número Cluster",ylab="SSE")


pibSet=cbind(pibSet,F_PCA,Grupos5)

# Representación grafica univariante de los cluster
windows(height=10,width=15)
par(mfrow=c(3,5))

lapply(colnames(pibSet[3:13]),function(y){
  boxplot(pibSet[,y]~pibSet$Grupos5,ylab=y,cex=1.5,pch=20,col="blue")
})
