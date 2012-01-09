# File-Name:       puntos_ideales_2011.R
# Date:            08.01.12
# Author:          Federico Carlés
# Email:           fedecarles@gmail.com                                      
# Data:            129.csv, Honorable Cámara de Diputados de la Nación
# Packages Used:   pscl (Simon Jackman), ggplot2 (Hadley Wickham)

# Los cálculos sobre la matriz de votaciones están hechos en base al método
# desarrollado por Keith Poole en "SPATIAL MODELS OF PARLIAMENTARY VOTING" (2005)

library(pscl)
library(ggplot2)

#setwd() Especificar el working directory.

####### CARGA Y PREPARACIÓN DE DATOS ##############################
per.129<-read.delim("Documentos/VN/129.csv", header=F, sep=",")
# Borramos a aquellos legisladores que no hayan estado en al menos el 70% 
# de la votaciones.
df.129 <-per.129[apply(per.129, 1, function(v) sum(!is.na(v)) >= 38 ), ]
df.129<-df.129[-which(rowSums(df.129[4:44]=="2")>=38),]
#df.129<-replace(df.129, df.129== 2,0)
#df.129<-replace(df.129,df.129==9,0)
#df.129[is.na(df.129)]<- 99

nombres<-df.129[,1]
legData<-data.frame(df.129[,2],length(df.129[,2]),1)
colnames(legData)<-"party"

legis<-df.129$V1
partido<-df.129$V2
df.129<-df.129[,4:58]

# Función de afiliación. De Simon Jackman. Calcula la proporción en que dos 
# elementos de la matriz coinciden entre si.
agreement <- function(x){
n <- dim(x)[1]
k <- dim(x)[2]
a <- matrix(NA,n,n)
for(i in 1:n){
for(j in 1:n){
a[i,j] <- sum(x[i,]==x[j,],na.rm=TRUE)/sum(!is.na(x[i,]) & !is.na(x[j,]))
}
}
a[is.nan(a)] <- 0
a
}

Sa<-agreement(df.129)
Sd <- (1-Sa)^2 # Convierte la matriz de acuerdo a una matriz de distancia.

# Función de centrado de la matriz. A cada valor se le resta la media de
# las filas y la media de las columnas. Luego se le suma la media de la matriz
# y se divide por -2.
doubleCenter <- function(x){
n <- dim(x)[1]
k <- dim(x)[2]
rowMeans <- matrix(apply(x,1,mean,na.rm=TRUE),n,k,byrow=TRUE)
colMeans <- matrix(apply(x,2,mean,na.rm=TRUE),n,k,byrow=FALSE)
matrixMean <- matrix(mean(x,na.rm=TRUE),n,k)
(x - rowMeans - colMeans + matrixMean)/-2
}

Sd <- doubleCenter(Sd)
Se <- eigen(Sd) # Extrae los vectores y valores propios de la matriz centrada.
Slambda <- Se$values
Sx <- Se$vector[,1] * sqrt(Slambda[1])
names(Sx)<-partido


####### Cálculo de los promedios por Bloque ######################
medias<- aggregate (Sx, by=list(names(Sx)),FUN=mean )
med<- as.numeric(medias$x)
names(med)<-medias$Group.1

png("Documentos/promedios2011.png", width=1280, height=900, res=150, unit="px")
qplot( medias$x, y = reorder(medias$Group.1, medias$x), data = medias,
        xlab="<< Oposición - Oficialismo >>", ylab=NULL,
        main="HCDN - Período 129")
dev.off()

###### Gráficos por legisladores ################################

Sx.df<-as.data.frame(Sx)
#Sx.df$cent<-scale(Sx.df$Sx, scale=T, center=T)
Sx.df$legis<-legis
Sx.df$partido<- partido
#Sx.df<-Sx.df[order(-Sx.df$cent),]


###### Oficialismo ##############################################
Sx.ofi<-Sx.df[order(-Sx.df$Sx),]
ofi<-Sx.ofi[1:10,] 
ofi<-ofi[order(ofi$Sx),]
ofi<-ofi[with(ofi,order(Sx)), ]
ofi$legis <- ordered(ofi$legis, levels=levels(ofi$legis)[unclass(ofi$legis)])

png("Documentos/ofi2011.png", width=1280, height=900, res=150, unit="px")
qplot(ofi$Sx,ofi$legis,main="Los 10 legisladores mas oficialistas 2011",
      xlab=" Oficialismo >>>",ylab=NULL)
dev.off()

###### Oposición #################################################
Sx.opo<-Sx.df[order(-Sx.df$Sx),]
opo<-Sx.opo[239:248,]
opo<-opo[order(opo$Sx),]
opo<-opo[with(opo,order(Sx)), ]
opo$legis <- ordered(opo$legis, levels=levels(opo$legis)[unclass(opo$legis)])

png("Documentos/opo2011.png", width=1280, height=900, res=150, unit="px")
qplot(opo$Sx,opo$legis, main="Los 10 legisladores mas opositores 2011",
      xlab="<<< Oposición",ylab=NULL)
dev.off()
