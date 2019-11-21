#install.packages("knitr")
library(matchingR)

#*********************funciones****************************
#Matriz de preferencias de acuerdo a un flujo y listado
PREFERENCIAS<-function(n,m,CLI,LAB,CapCompra,CapVenta,theta){
  
  #********Obteniendo las PREFERENCIAS DE ACUERDO AL FLUJO DE DINERO*************
  individuos<-seq(1,n+m,1)
  
  #Se obtienen los flujos
  flujo<-c()
  for(cliente in CapCompra){
    for (laboratorio in CapVenta) {
      flujo<-append(flujo,c(cliente*laboratorio))
    }
  }
  flujo<-matrix(flujo,n,m,T)
  #Nombres para la matriz
  rownames(flujo)<-sprintf("Cliente %d",1:n)
  colnames(flujo)<-sprintf("Laboratorio %d",1:m)
  
  
  #Rankeo para los clientes
  rankeo<-vector()
  for (clien in 1:n) {
    rankeo<-append(rankeo,c(rank(-flujo[clien,])))
  }
  rankeoclien<-matrix(rankeo,m,n,F)
  #Le tenemos que sumar n clientes para que coincidan las etiquetas
  rankeoclien<-rankeoclien+n
  
  #Rankeo para los laboratorios
  rankeo<-vector()
  for (lab in 1:m) {
    rankeo<-append(rankeo,c(rank(-flujo[,lab])))
  }
  rankeolab<-matrix(rankeo,n,m,F)
  
  #Ahora tenemos los rankeos de los clientes y los laboratorios
  PrefClie<-rankeoclien
  PrefLab<-rankeolab
  
  
  MFlujo<-matrix(,n+m,n+m)
  
  #Llena la matriz con las preferencias del cliente
  for (i in 1:nrow(PrefClie)) {
    for (j in 1:ncol(PrefClie)) {
      MFlujo[i,j]<-PrefClie[i,j]
    }
  }
  #Llena la matriz con las preferencias del laboratorio
  for (i in 1:nrow(PrefLab)) {
    for (j in 1:ncol(PrefLab)) {
      MFlujo[i,j+n]<-PrefLab[i,j]
    }
  }
  
  
  #Despu?s se prefieren a s? mismos
  #Clientes
  for (k in 1:ncol(PrefClie)) {
    MFlujo[nrow(PrefClie)+1,k]<-k
  }
  #Laboratorios
  for (l in 1:ncol(PrefLab)) {
    MFlujo[nrow(PrefLab)+1,l+n]<-l+n
  }
  
  
  #Los dem?s valores no importan porque ya est?n delimitados por "si mismos"
  resto<-vector()
  for (indi in 1:(n+m)) {
    resto<-append(resto,individuos[-intersect(MFlujo[,indi],individuos)] )
  }
  
  #Terminamos de rellenar la matriz
  NAs<-matrix(is.na(MFlujo),n+m,n+m)
  conta<-1
  
  for (j in 1:(n+m)) {
    for (i in 1:(n+m)) {
      if(NAs[i,j] == TRUE){
        MFlujo[i,j]<-resto[conta]
        conta<-conta+1
      }
    }
  }
  
  
  #**************Preferencias DE ACUERDO AL CLIENTE-LABORATORIO**************
  
  #MFlujo es una matriz donde se guardan las preferencias-cliente laboratorio
  #de acuerdo al FLUJO de efectivo entre estos
  
  #MPref es una matriz donde se guardan las prefrencias-cliente laboratorio
  #de acuerdo a ellos
  
  MPref<-matrix(,n+m,n+m)
  
  #Llena la matriz con las preferencias del cliente
  for (i in 1:nrow(CLI)) {
    for (j in 1:ncol(CLI)) {
      MPref[i,j]<-CLI[i,j]
    }
  }
  #Llena la matriz con las preferencias del laboratorio
  for (i in 1:nrow(LAB)) {
    for (j in 1:ncol(LAB)) {
      MPref[i,j+n]<-LAB[i,j]
    }
  }
  
  #Despu?s se prefieren a s? mismos
  #Clientes
  for (k in 1:ncol(CLI)) {
    MPref[nrow(CLI)+1,k]<-k
  }
  #Laboratorios
  for (l in 1:ncol(LAB)) {
    MPref[nrow(LAB)+1,l+n]<-l+n
  }
  #Rellenamos los valores restantes, asegurandonos que tengan los mismos valores
  #que en MPref
  
  na<-matrix(is.na(MPref),n+m,n+m)
  for (j in 1:(n+m)) {
    for (i in 1:(n+m)) {
      if(na[i,j] == TRUE){
        MPref[i,j]<-MFlujo[i,j]
      }
    }
  }
  
  
  #***************AGREGANDOLES EL PARAMETRO THETA***********************
  thetaComp<-1-theta
  
  aux1<-MPref*theta
  aux2<-MFlujo*thetaComp
  
  M<-aux1+aux2
  
  #Tengo que ordenarlas por columnas
  PREFERENCIAS<-matrix(,n+m,n+m)
  for (w in 1:(n+m)) {
    PREFERENCIAS[,w]<-rank(M[,w])
  }
  
  #Debemos de asegurarnos de que los rangos son unicos
  for (i in 1:(n+m)) {
    if((length(unique(PREFERENCIAS[,i]))) != n+m){
      PREFERENCIAS[,i]<-MFlujo[,i]
    }
  }
  
  
  
  return(PREFERENCIAS)
  
}

#Funcion que le ingresas una pareja (C,L) que ya se encontró 
# a una matriz y reordena las preferencias de dicha matriz
Reordenamiento<-function(C,L){
  lugar1<-match(L,PREFERENCIAS[,C])
  lugar2<-match(C,PREFERENCIAS[,L])
  i1<-PREFERENCIAS[,C]
  i2<-PREFERENCIAS[,L]
  nuevo1<-c(i1[-lugar1], i1[lugar1])
  nuevo2<-c(i2[-lugar2], i2[lugar2])
  PREFERENCIAS[,C]<-nuevo1
  PREFERENCIAS[,L]<-nuevo2
  return(PREFERENCIAS)
}

#**********************************************************
#Parametros
n<-3 #Numero de clientes
m<-2 #Numero de laboratorios
individuos<- c(1:(n+m))
theta<-.5
CapCompra<-c(20,10,5)
CapVenta<-c(100,200)
CLI<-matrix(c(4,5,5,4,5,4),m,n,F) #lista de preferencias de clientes
LAB<-matrix(c(3,2,1,2,3,1),n,m,F) #Lista de preferencias de laboratorios

PREFERENCIAS<-PREFERENCIAS(n,m,CLI,LAB,CapCompra,CapVenta,theta)

PREFERENCIAS
#**********************PRIMERA ETAPA***************************
###************** PRIMER EMPAREJAMIENTO*********************###
PRIMERA<-galeShapley.marriageMarket(proposerPref = PREFERENCIAS, reviewerPref = PREFERENCIAS)
PrimerasParejas<-PRIMERA$proposals
PrimerasParejas

#Ordena las preferencias despues del PRIMER encuentro
for (i in 1:(n+m)) {
  if(PrimerasParejas[i,1]!= i){
    PREFERENCIAS<-Reordenamiento(i,PrimerasParejas[i])
  }
} 
#******************** Fin de la primer etapa*****************


#****************** INICIO DEL MODELO DIN?MICO************
#Ingresa a los individuos disponibles
disponibles<-c(3,1,5)

Parejas<-function(disponible){
  #Creamos una matriz con dimensi?n: disponibles x disoonibles
  ReMatching<-matrix(,length(disponibles), length(disponibles))
  
  #Detectamos a los individuos no disponibles
  Nodisponibles<- individuos[!individuos %in% disponibles]
  
  #Le quitamos a la matriz de preferencias los no disponibles
  NuevasPref<-PREFERENCIAS[,-Nodisponibles]
  
  for (i in 1:(ncol(NuevasPref))){
    aux6<-NuevasPref[, i]
    aux7<-aux6[!aux6 %in% Nodisponibles]
    ReMatching[,i]<-aux7
  }
  
  ReMatching #matriz de disponiblesxdisponibles para remachear
  
  ReMatchRank<-ReMatching #no se si voy a voler a usar ReMatching
  for (colum in 1:(ncol(ReMatchRank))) { #Formato adecuado 
    ReMatchRank[,colum]<-rank(ReMatchRank[,colum])
  }
  ReMatchRank
  MatchCorto<- galeShapley.marriageMarket(proposerPref = ReMatchRank, reviewerPref = ReMatchRank)
  p<-MatchCorto$proposals
  disponibles<-sort(disponibles)
  
  #Equivalencias
  E<-matrix(,nrow(p),2)
  E[,2]<-disponibles
  E[,1]<-rank(disponibles)
  
  W<-matrix(,n+m,1)
  
  #Cambio
  for (j in 1:(nrow(p))) {
    pos<-E[j,2]
    auz<-match(p[j,1],E[,1])
    auz2<-E[auz,2]
    W[pos,1]<-auz2
  }
  
  return(W)
  
} #Solo se corre una vez, después de
#la primer etapa

Parejas(disponibles)

#Ingresa la pareja laboratorio-cliente que ya se encontr?
#OJO: INGRESAR TODAS LAS PAREJAS QUE YA SE ENCONTRAR?N DESPU?S
#DE CADA ETAPA 
Reordenamiento(5,1)
   