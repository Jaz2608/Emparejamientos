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
  
  
  #Despues se prefieren a si mismos
  #Clientes
  for (k in 1:ncol(PrefClie)) {
    MFlujo[nrow(PrefClie)+1,k]<-k
  }
  #Laboratorios
  for (l in 1:ncol(PrefLab)) {
    MFlujo[nrow(PrefLab)+1,l+n]<-l+n
  }
  
  
  #Los demas valores no importan porque ya est?n delimitados por "si mismos"
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
  
  #Debemos de asegurarnos de que los rangos son ?nicos
  for (i in 1:(n+m)) {
    if((length(unique(PREFERENCIAS[,i]))) != n+m){
      PREFERENCIAS[,i]<-MFlujo[,i]
    }
  }
  
  
  
  return(PREFERENCIAS)
  
}

#Funcion que le ingresas una pareja (C,L), una matriz 
#y reordena las preferencias de dicha matriz
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

#############***********************SIMULACION***********************######

#Parametros
n<-40 #Numero de clientes
m<-60 #Numero de laboratorios
individuos<- c(1:(n+m))

CapCompra<-sample(1:100,n,F)
CapCompra
CapVenta<-sample(1:100,m,F)
CapVenta

CLI<-matrix(,m,n,F) #lista de preferencias de clientes
LAB<-matrix(,n,m,F) #Lista de preferencias de laboratorios

#Llenamos aleatoriamente las matrices de preferencias de los clientes
#Y de los laboratorios
x<-(n+1):(n+m) 
y<-1:n

#Para los clientes
for(i in 1:n){
  CLI[,i]<-sample(x,m,F)
}
CLI

#Para los laboratorios
for(i in 1:m){
  LAB[,i]<-sample(y,n,F)
}
LAB

#Las preferencias de acuerdo a los flujos y laboratorios son guardadas
PrefFlujo<-PREFERENCIAS(n,m,CLI,LAB,CapCompra,CapVenta,0)
PrefCL<-PREFERENCIAS(n,m,CLI,LAB,CapCompra,CapVenta,1)

PREFERENCIAS<-PREFERENCIAS(n,m,CLI,LAB,CapCompra,CapVenta,.5)
#**********************PRIMERA ETAPA***************************
###************** PRIMER EMPAREJAMIENTO*********************###
PRIMERA<-galeShapley.marriageMarket(proposerPref = PREFERENCIAS, reviewerPref = PREFERENCIAS)
PrimerasParejas<-PRIMERA$proposals
PrimerasParejas


#Ahora hagamos un simulaci?n de emparejamientos al azar
#Como son l?s clientes que laboratorios lo logico es que ningun cliente
#Se quede sin emparejar

ALAZAR<-matrix(sample(x,n,F),n,1)
ALAZAR

#AHORA VEAMOS CU?L EMPAREJAMIENTO TIENE UN GRADO DE SATISFACCI?N M?S ALTO
#SACL = satisfaccion IMPLEMENTANDO EL ALGORITMO de los clientes-laboratorios
#SAF = satisfacci?n IMPLEMENTANDO EL ALGORITMO de acuerdo al flujo de dinero
#SSaCL = satisfaccion de los clientes-labs sin usar algoritmo
#SSaF = satisfaccion de acuerdo al flujo de efectivo sin usar algoritmo

#Hasta n porque spg n<m, entonces todos los clientes quedan emparejados

PP<-PrimerasParejas[1:n,]
azar<-as.vector(ALAZAR)


# Calculando SAF
SACL<-0
for (k in PP==PrefFlujo[1,1:n]) {
  if(k==TRUE){
    SACL<-SACL+3
  }
}
for (k in PP==PrefFlujo[2,1:n]){
  if(k==TRUE){
    SACL<-SACL+2
  }}
for (k in PP==PrefFlujo[3,1:n]) {
  if(k==TRUE){
    SACL<-SACL+1
  }
}
SACL

# Calculando SACL
SAF<-0
for (k in PP==PrefCL[1,1:n]) {
  if(k==TRUE){
    SAF<-SAF+3
  }
}
for (k in PP==PrefCL[2,1:n]){
  if(k==TRUE){
    SAF<-SAF+2
  }}
for (k in PP==PrefCL[3,1:n]) {
  if(k==TRUE){
    SAF<-SAF+1
  }
}
SAF


#Calculando SSaCL
SSaCL<-0
for (k in azar==PrefFlujo[1,1:n]) {
  if(k==TRUE){
    SSaCL<-SSaCL+3
  }
}
for (k in azar==PrefFlujo[2,1:n]) {
  if(k==TRUE){
    SSaCL<-SSaCL+2
  }
}
for (k in azar==PrefFlujo[3,1:n]) {
  if(k==TRUE){
    SSaCL<-SSaCL+1
  }
}


#Calculando SSaF
SSaF<-0
for (k in azar==PrefFlujo[1,1:n]) {
  if(k==TRUE){
    SSaF<-SSaF+3
  }
}
for (k in azar==PrefFlujo[2,1:n]) {
  if(k==TRUE){
    SSaF<-SSaF+2
  }
}
for (k in azar==PrefFlujo[3,1:n]) {
  if(k==TRUE){
    SSaF<-SSaF+1
  }
}


#Entonces la satisfacci?n global usando el agoritmo es:
S_Algoritmo<-SAF + SACL
S_Algoritmo
#Entonces, la satisfacci?n global sin usar el algoritmo es:
S_SIN<-SSaCL+SSaF
S_SIN



