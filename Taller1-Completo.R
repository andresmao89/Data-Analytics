#----------------------------------------------------------------------
#Integrantes: Andrés Mauricio Valdés Mondragón 2300833 Juan Fernando Parra 2300060
#
#
#
#                         El Juego de Meré
#Se reutilizarán los métodos para ambas versiones del juego, por lo que
#adicional a los parámetros establecidos en el taller, se incluirá el
#parámetro de versión juego: vj
#


juego.simple <- function(n,vj){ #función que simula un juego, 
                                    #donde n es el numero de lanzamientos por juego
                                    #y vj es la versión del juego (1 o 2)
  juego <- c()                  
  
  if(vj==1){                       #Evalua la versión del juego 
    juego <- sample(1:6,n)         #Si es la versión 1, lanza el dado 1 vez y almacena una lista de los
                                   #n-resultados
  }else{
    for (i in 1:n){                 #Si es la versión 2 va a lanzar 2 dados a la vez, razón por la que
                                    #se utiliza el for, para repetir el proceso n veces(lanzamientos)
      dado1 <- sample(1:6,1)        #es por eso que el sample se aplica una sola vez por lanzamiento
      dado2 <- sample(1:6,1)        #en vez de aplicarlo n veces, ya que sería como si lanzara el primer
                                    #dado n veces y luego el segundo dado n veces y no como si lanzara dos
                                    #dados al tiempo n veces.
      
      if (dado1 == 6 && dado2==6){  #Para evitar incrementar la complejidad de guardar dos valores por
                                    #posición de la lista, se decide guardar un único valor booleano
        juego[i] = TRUE             #donde True es si saca el resultado esperado(6,6), False es cualquier
                                    #otro resultado
      }else{
        juego[i] = FALSE
      }
    }
  }
  
  return(juego)             #retorno una lista con los resultados obtenidos por cada lanzamiento
}

prob.ganar <- function(l,n,vj){
  
  juegos <- replicate(l,juego.simple(n,vj))       #Replica un juego 'l' veces, el método replicate crea una
                                                  #una matriz, donde las columnas son 'l' y las filas son los lanzamientos.
  p = get.prob(juegos,vj)                         #Obtiene la probabilidad por cada juego
  
  return(list(juegos,p))                          #retorna lista con los resultados de los juegos y su
}                                                 #respectiva probabilidad

get.prob <- function(matriz,vj){    #Esta función se crea para obtener cada una de las probabilidades de una lista
                                    #de juegos
  suma = 0
  
  datoComparativo = 6              #Como dependiendo de la versión del juego, el valor de éxito puede ser
                                  #6 o true, entonces se crea una varible que adopta el valor dependiendo  
  if(vj==2){                      #de la versión que se le asigne
    datoComparativo= TRUE
  }
  
  p <- numeric()

  for (i in 1:ncol(matriz)) {   #se recorre la matriz por columna, ya que cada columna representa un juego
                                
    suma = suma +  sum(matriz[,i]== datoComparativo)   #Se calcula cuantas veces se cumple el caso de éxito por juego (columna)
                                                       #y se acumula a medida que recorre la matriz
    len = nrow(matriz)*i  #Dado que el número de filas de la matriz representa la cantidad de lanzamientos
                          #por juego, a medida que se recorren las columnas ('i'), se va multiplicando para calcular
    p[i] = suma/len       #el total de lanzamientos y así calcular su probabilidad (cantidad de éxitos/total datos)
                          #y se guardan en una lista.
  }
  
  return(p)
  
}

graf.mere <- function(l,seq.n,vj){
  
 p <- prob.ganar(l,seq.n,vj)[[2]]  #se almacena la lista de probabilidades que se encuentran en la posición 2
                                   #de la lista que retorna el método prob.ganar. Recordemos que la primera
                                   #posición tiene la matriz con los datos de lanzamientos.
 
 prob <- sapply(1:l, function (i) 1-(1-p[[i]])^i, simplify = FALSE) #Con el método sapply se le aplica a cada
                                                                    #una de la probabilidades el formato de pascal
                                                                    #y se guarda en un arreglo
                                                                   
 print( prob)
 
 windows()
 
 plot(1:l, unlist(prob), ylab="P: Probabilidad de Ganar", xlab="n: Número de lanzamientos", ylim = c(0,1),
      main = paste("El Juego de Caballero de Meré - Modo ",vj))  #Se grafica las probabilidades por juego ('l')con el formato de pascal
 
 
 if(vj==1){
   curve(curva.pascal1, from = 1, to = l, col="red", add = TRUE)  #Se grafica la curva de pascal según la versión del juego
 }else{                                                           #ya que dependiendo de la versión, la probabilidad
   curve(curva.pascal2, from = 1, to = l, col="red", add = TRUE)  # puede ser de 1/6 para la versión 1 y 1/36 para la versión 2
 }
 
 legend(x = "bottomright", legend = c("Pascal's Curve", "Simulation Point"), #Se inserta legenda al gráfico
        fill = c("red", "grey"))                                             # para especificar cuál es la curva de pascal
                                                                             # y cuáles son los datos de la simulación
 return(prob)
  
}

curva.pascal1 <- function(i){
  
  p<-numeric()
  
  p [i] <- 1-(1-(1/6))^i
  
}  #Función que utiliza el método curve, para obtener los puntos de 
                                   #la curva a graficar para la versión 1
curva.pascal2 <- function(i){
  
  p<-numeric()
  
  p [i] <- 1-(1-(1/36))^i
  
}  #para la versión 2

graf.mere(20,4,1)
graf.mere(100,24,2)


#----------------------------- FIN JUEGO DE MERÉ --------------------------------------------------------

# -------------------------------------------------------------------------------------------------------
#                             ANAGRAMA

if(!require('seqinr')) {       #Para utilizar una función que convierte un string en un arreglo se instala
  install.packages('seqinr')   #y se importa la libreria 'seqinr'. Se valida que no exista esta librería en
  library('seqinr')            #el entorno de desarrollo, sino existe se instala. 
}

anagrama <- function(str1, str2){ 
  
  
  if(nchar(str1)== nchar(str2)){ #Valido la cantidad de caracteres en los dos strings para comparar su tamaño
                                #si son iguales, entra a validar sin son anagramas,sino simplemente no lo son
                                #y retornamos FALSE.
    
    s1 <- s2c(tolower(str1))  #Se convierte el string en cadena y convierte todos los caracteres en minuscula   
                              #para evitar errores en la comparación.
    s2 <- s2c(tolower(str2))
    
    for (i in s1) {          #La forma en que se valida si son anagramas, es con los caracteres del
      x = match(i,s2)        #primer string. Se recorre caracter por caracter y con el método match se valida
      s2= s2[-x]             #si ese caracter existe en el otro string, si existe, lo elimino del string 2, para
                             #evitar una mala comparación, en caso de que los strings tengan letras repetidas.
    }
    
    if(length(s2)==0){      #Una vez finalizado el recorrido de los caracteres del string 1, y teniendo en cuenta
      return(TRUE)         #que ya se sabe que el tamaño de los strings son iguales, si la longitud del string 2
    }                      #es 0, es decir que se le eliminó todos los caracteres que conincidieron con el string 1
                           #esto significa que efectivamente eran anagramas.
  }
  
  return(FALSE)
  
}

if(anagrama("Hola","ohal")){
  print("es un anagrama")
}

anagrama("Hola","Palo")

anagrama("Hola","AlEjAndra")

anagrama("bob","obb")

anagrama("amor","ROMA")


#----------------------------------FIN ANAGRAMA-------------------------------------------------------------

#-----------------------------------------------------------------------------------------------------------
#                                LECTURA DE DATOS


if(!require('readxl')) {     #Para utilizar el método read_excel, importamos e instalamos la librería 'readxl'
  install.packages("readxl") #se valida si existe en el entorno de desarrollo sino se instala.
  library("readxl")
}

bd <- read_excel(file.choose())    #Se cargar el archivo de excel que se elija

graficar.TipoInmueble <- function(my_data){    #Esta función, grafica la tabla de frecuencia por tipo de inmueble
  
  frecuencia <- table(my_data[,3])      #El método table crea la tabla frecuencia tomando como parámetro la columna 3 del archivo de excel
                                        #que corresponde al tipo de inmueble.
  
  pos_tabla = round(prop.table(frecuencia),4) #Para la estética del gráfico, se redondea los valores a 4 decimales
  
  
  windows()
  
  coord = barplot(pos_tabla,                                      #Se crea el gráfico de barras con la tabla de frecuencia 
                  col=c("Red","yellowgreen","lightblue",
                        "magenta","yellow", "sienna", "salmon"), 
                        main = "Inmuebles", xlab = "Tipo Propiedad", ylab = "Porcentaje",
                  ylim = c(0,0.2))
  
  abline(h=pos_tabla, col="turquoise3", lty=2, lwd=2)
  
  porcentaje = paste(pos_tabla*100,"%")   #Se transforman los valores decimales en porcentaje
  
  sapply(1:length(pos_tabla), function(i) text(coord[i], pos_tabla[i]-0.05, porcentaje[i]), simplify = FALSE) #Se le aplica el formato de porcentaje a todos los valores del gráfico
}

graficar.TipoInmueble(bd)

ranking.FiltroBarcelona <- function(my_data){   # Esta función, hace un ranking de los vendedores por fuera de Barcelona según su nivel de ventas
  
  reduccion <- my_data[, c('Provincia', 'Precio Venta','Vendedor')]  # Se reduce la tabla a sólo las 3 columnas que contienen las información que se necesita.
  attach(reduccion)
  filtro <- subset(reduccion, Provincia != "Barcelona") #Se filtra la tabla anterior abstrayendo todos los datos que cumplen la condición de provincia  diferente a 'Barcelona'
                                                        #prácticamente se está eliminando las filas donde provincia es igual que barcelona. 

  
    vendedores <- names(table(filtro[,3]))  #El método 'table' crea una tabla de frecuencia con la columna 3 de la tabla reducida,que son los vendedores, poniendo             
                                            #los nombres de vendedores como los títulos de las columnas. Al generar la tabla de frecuencia
                                            #se está obteniendo todos los vendedores sin repetir, y para almacenarlos en una lista se utiliza
                                            #el método names, el cual toma los nombres de las columnas, que para el caso corresponde a los 
                                            #nombres de los vendedores.
    
  f <- sapply(vendedores, function(i) obtener.ventas(i,filtro), simplify = TRUE)  #El método sapply aplica la función 'obtener.ventas' a cada uno de los vendedores que están en la lista
                                                                                  #y crea una lista con esa información.
      
  f <-sort(f, decreasing=TRUE) #Organiza la lista de forma descendente.
  
  print(f)
  print(filtro)
  
  windows()
  
  coord = barplot(f, 
                  col=c("Red","yellowgreen","magenta","yellow",   #Genera el gráfico de barras con los vendedores y el total de ventas de cada uno
                        "sienna", "salmon"), 
                        main = "Inmuebles", xlab = "Vendedores", 
                  ylab = "Total Ventas x Miles")
  
  
  #porcentaje = paste(m,"M")
  
  # sapply(1:length(m), function(i) text(coord[i], m[i]-0.05, porcentaje[i]), simplify = FALSE)
  
  
}

obtener.ventas <- function(v,t){  # Obtiene la sumatoria de todas las ventas por vendedor
  
  filtro <- subset(t, Vendedor == v) #Se abstrae de la tabla que recibe como parámetro, todos los datos que correspondan al vendedor
                                     #que también recibe como parámetro.
  
  x = sum(filtro[,2])    #Se conoce que el formato de la tabla que recibe es de 3 columnas, donde la posición 2 corresponde a la columna precio de venta
                         #para obtener la sumatoria de todos los elemento de la columna, se utiliza la función sum.
  
  return(x/10000)   #Dado que precio de venta es un número muy grande, para estética del gráfico se reduce el tamaño del número por miles.
}

ranking.FiltroBarcelona(bd)

superficie.precio <- function(my_data){  #Este método relaciona la superficie con el precio de venta de inmuebles según filtro
  
  reduccion <- my_data[, c('Provincia','Tipo','Operación','Superficie','Precio Venta')] #Reduce a las 5 columnas que tienen los datos que se necesitan.
  attach(reduccion) #Permite que se puedan llamar los nombres de las columnas de una forma directa
  
  filtro1 <- subset(reduccion, Provincia == "Lleida") #Se aplican filtros
  filtro2 <- subset(filtro1, Tipo == "Casa") #Se aplican filtros
  filtro3 <- subset(filtro2, Operación == "Alquiler") #Se aplican filtros
  
  reduccion2 <- filtro3[, c('Superficie','Precio Venta')] # Ahora se reduce en las dos columnas que se necesitan graficar
  
  print(filtro3)
  
  windows()
  
  plot(reduccion2[[1]], reduccion2[[2]], main = "Superficie vs Precio", 
       xlab = "Superficie", ylab = "Precios") #Grafico Disperción donde 'X' es la superficie,
                                              #recordando que la variable reducción 2 es una lista de las 2 columnas
                                              #en donde la primera columna le asignamos la superficie, por esta razón
                                              #quedan almacenadas en la primera posición de la lista y la segunda posición
                                              #corresponde a la 2da columna que es Precios, que será el eje 'Y'
                                                                                  
  
}

superficie.precio(bd)





