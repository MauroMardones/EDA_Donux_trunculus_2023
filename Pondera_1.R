##CALCULO DE LA DISTANCIA DE LOS TRACKS (SOLO PUNTO INICIAL Y FINAL) PARA DATOS AGAPA##
##PLOTS TALLAS y RESULTADOS

#M. DELGADO, 31/08/17

##DATOS
data<-data_ieo_2020_def
#data<-WPcalculotracks_2018

##Calculos de TRACKS
#Conversión a decimal de los grados/min

lat1<-data$Lat1g+(data$Lat1min/60)
long1<-data$Long1g+(data$Long1min/60)
lat2<-data$Lat2g+(data$Lat2min/60)
long2<-data$Long2g+(data$Long2min/60)

#Los datos tienen que ir en radianes (conversión de grados a radianes)
long1<--6.39333
lat1<-36.80040
long2<--6.39372
lat2<-36.80045

deg2rad <- function(deg) return(deg*pi/180)

long1<-deg2rad(long1)
lat1<-deg2rad(lat1)
long2<-deg2rad(long2)
lat2<-deg2rad(lat2)

R<-6371#Radius of the Earth

#Calculo de Haversine distance

gcd.hf2 <- function(long1,lat1,long2,lat2) {
  dlong<-long2-long1
  dlat<-lat2-lat1
  a<-(sin(dlat/2))^2+cos(lat1)*cos(lat2)*(sin(dlong/2))^2
  c<-2*atan2(sqrt(a),sqrt(1-a))
  d<-R*c#(where R is the radius of the Earth) 
  return(d) # Distance in km
}
dists<-(gcd.hf2(long1,lat1,long2,lat2))*1000
dists#Distancia en metros

data$dists<-dists
data<-as.data.frame(data)
write.table(data, file = "data_new.csv", append = FALSE, quote = TRUE, sep = ";", dec = ".", col.names = TRUE)

##RESULTS: Distribuciones de frecuencia

#SELECCION DATOS

data<-data_ieo_2020_def
data_station<-data[data$Beach=="Donana"&data$rastro=="POBLACIONAL",]
#data_station<-data_station[data_station$zaranda=="R",]
data_station<-data_station[data_station$Sampling.point==1,]

#data_area<-data[data$Beach=="A",]##Selecciono playa 
#data_month<-data_area[data_area$months=="ENERO",]##Selecciono año (2014,2015)
#data_rastro<-data_month[data_month$rastro=="MODIFICADO",]#Selecciono tipo de rastro

#PLOTS TALLAS
opar<-par()
par(oma=c(2,2,2,2),mar=c(1,2,1,2))#Se puede variar para el tema de los márgenes
ppi <- 300##resolución gráfica
multiplot<-function(row,col){par(mfrow=c(row,col),pty="s")}#Función para salida de gráficos múltiples
multiplot(3,2)##Preparo el area de salida de los gráficos 3x
v<-25
#months<-c(7,8,9,10,11,12)#!!CUIDADO CON ESTE VECTOR!!
months<-unique(data_station$months)

#Salida N15 y d15 (indice de reclutamiento)
#months<-unique(data_station$months)
months<-c(1:12)
t<-length(months)
month<-rep(0,t)
area<-rep(0,t)
N15<-rep(0,t) 
d15<-rep(0,t)
resultN15<- data.frame(month,area,N15,d15) ###### CREAR NUEVA TABLA CON DATOS DEL VECTOR
head(resultN15)

#months<-10#Para hacer solo un mes concreto en bucle
months<-c(1,2,6,7,8,9,10,12)#Para 2020

for (i in months){
  y<-seq(3,40,by=1)
  data_month<-data_station[data_station$months==i,]
  if(unique(data_month$CAT==1)){
    z<-cut(data_month$SizeE,breaks=y)
    counts<-table(z)
    total<-sum(counts)
    pct1<-prop.table(counts)
    pct1<-pct1*100#frecuencia en %
    pct1df<-as.data.frame(pct1)
    pct1df$z<-c(3:39)
    pct1df25<-pct1df[pct1df$z>=25,]
    Per25<-round(sum(pct1df25$Freq),digits=2)
    b1<-barplot(pct1,xlim=c(3,40),ylim=c(0,30),ylab="Frequency (%)",xlab="Size class (mm)",names.arg=c(3:39))
    abline(v=v,lwd=1.5)
    text(11,18,labels="month:")
    text(17,18,labels=i)
    text(35,18,labels="%>25:")
    text(35,16,labels=Per25)
    counts_df<-as.data.frame(counts)
    counts_df$z<-c(3:39)
    counts_df15<-counts_df[counts_df$z<15,]
    N15<-round(sum(counts_df15$Freq),digits=2)
    d15<-N15/((unique(data_month$track_activelog))*0.445)
    resultN15[i,1]<-i
    resultN15[i,2]<-unique(data_month$track_activelog)*0.445
    resultN15[i,3]<-N15
    resultN15[i,4]<-d15
  }else{
    data_month_g<-data_month[data_month$months==i&data_month$Categoria=="g",]
    zg<-cut(data_month_g$SizeE,breaks=y)
    counts_g<-table(zg)
    
    total_g_clam<-(unique(data_month$Clam_sample_weigth[data_month$Categoria=='g']))
    meas_g_clam<-(unique(data_month$Measured_clam_sample_weigth[data_month$Categoria=='g']))
    fpg<-total_g_clam/meas_g_clam
    
    counts_g_p<-counts_g*fpg
    
    data_month_p<-data_month[data_month$months==i&data_month$Categoria=="p",]
    zp<-cut(data_month_p$SizeE,breaks=y)
    counts_p<-table(zp)
    
    total_p_clam<-(unique(data_month$Clam_sample_weigth[data_month$Categoria=='p']))
    meas_p_clam<-(unique(data_month$Measured_clam_sample_weigth[data_month$Categoria=='p']))
    fpp<-total_p_clam/meas_p_clam
    
    counts_p_p<-counts_p*fpp
    counts_t<-counts_g_p+counts_p_p
    total<-sum(counts_t)
    pct1<-prop.table(counts_t)
    pct1<-pct1*100#frecuencia en %
    pct1df<-as.data.frame(pct1)
    pct1df$z<-c(3:39)
    pct1df25<-pct1df[pct1df$z>=25,]
    Per25<-round(sum(pct1df25$Freq),digits=2)
    b1<-barplot(pct1,xlim=c(3,40),ylim=c(0,30),ylab="Frequency (%)",xlab="Size class (mm)",names.arg=c(3:39))
    abline(v=v,lwd=1.5)
    text(11,18,labels="month:")
    text(17,18,labels=i)
    text(36,18,labels="%>25:")
    text(36,12,labels=Per25)
    counts_df<-as.data.frame(counts_t)
    counts_df$z<-c(3:39)
    counts_df15<-counts_df[counts_df$z<15,]
    N15<-round(sum(counts_df15$Freq),digits=2)
    d15<-N15/(unique(data_month$track_activelog*0.445))
    resultN15[i,1]<-i
    resultN15[i,2]<-unique(data_month$track_activelog)*0.445
    resultN15[i,3]<-N15
    resultN15[i,4]<-d15
  }
}

write.table(resultN15,file=c("resultsD15_Pto1_Donana_2020.csv"),append=TRUE,quote=TRUE,sep=";",dec=".",col.names=TRUE)
head(resultN15)



#mtext("Donana_poblacional_Pto1_2020",side=3,outer=T,font=2)#Para multiplot

#title(main="Punto 5")#Para un solo plot

#SALIDA DE DATOS (estadística básica de tallas) + DENSIDAD

#Dataframe de salida
months<-unique(data_station$months)
months<-c(1:12)#Para 2020
t<-length(months)
month<-rep(0,t)
N<-rep(0,t) 
Media<-rep(0,t) 
sd<-rep(0,t)
track<-rep(0,t)
area<-rep(0,t)
dens<-rep(0,t)
result<- data.frame(month,N,Media,sd,track,area,dens) ###### CREAR NUEVA TABLA CON DATOS DEL VECTOR
head(result)

#months<-7
months<-c(1,2,6,7,8,9,10,12)#Para 2020

for (i in months){
  y<-seq(3,40,by=1)
  data_month<-data_station[data_station$months==i,]
  if(unique(data_month$CAT==1)){
    z<-cut(data_month$SizeE,breaks=y)
    counts<-table(z)#frecuencia en numero
    total<-sum(counts)
    fp<-unique(data_month$Clam_sample_weigth/data_month$Measured_clam_sample_weigth)
    total2<-total*fp
    size<-data_month$SizeE
    media<-as.numeric(round(sum(size)/total,digits=2))##MEDIA
    sd<-sqrt(((sum((size-media)^2))/(total-1)))
    sdr<-round(sd,digits=2)#DESVIACION
    track<-unique(data_month$track_activelog)
    area<-track*0.445#Ancho boca rastro 0.445m poblacional/0.435 ancho boca rastro comercial
    dens<-total2/area
    result[i,1]<-i
    result[i,2]<-total2
    result[i,3]<-media
    result[i,4]<-sdr
    result[i,5]<-track
    result[i,6]<-area
    result[i,7]<-dens
  }else{
    data_month_g<-data_month[data_month$Categoria=="g",]
    zg<-cut(data_month_g$SizeE,breaks=y)
    counts_g<-table(zg)
    total_g_clam<-(unique(data_month$Clam_sample_weigth[data_month$Categoria=='g']))
    meas_g_clam<-(unique(data_month$Measured_clam_sample_weigth[data_month$Categoria=='g']))
    fpg<-total_g_clam/meas_g_clam
    counts_g_p<-counts_g*fpg
    data_month_p<-data_month[data_month$Categoria=="p",]
    zp<-cut(data_month_p$SizeE,breaks=y)
    counts_p<-table(zp)
    total_p_clam<-(unique(data_month$Clam_sample_weigth[data_month$Categoria=='p']))
    meas_p_clam<-(unique(data_month$Measured_clam_sample_weigth[data_month$Categoria=='p']))
    fpp<-total_p_clam/meas_p_clam
    counts_p_p<-counts_p*fpp
    counts_t<-counts_g_p+counts_p_p
    total<-sum(counts_t)
    total
    counts_tdf<-as.data.frame(counts_t)
    clase<-c(4:40)
    counts_tdf$clase<-clase
    size_t<-counts_tdf$Freq*counts_tdf$clase
    media<-as.numeric(round(sum(size_t)/total,digits=2))##MEDIA
    sd<-sqrt(sum(((counts_tdf$clase-media)^2)*counts_tdf$Freq)/(total))
    sdr<-round(sd,digits=2)#DESVIACION
    track<-unique(data_month$track_activelog)
    area<-track*0.445#Ancho boca rastro 0.445m poblacional/0.435 ancho boca rastro comercial
    dens<-total/area
    result[i,1]<-i
    result[i,2]<-total
    result[i,3]<-media
    result[i,4]<-sdr
    result[i,5]<-track
    result[i,6]<-area
    result[i,7]<-dens
  }
}

##ESTOS RESULTADOS DE DENSIDAD HAY QUE REVISARLOS POR SI HAY QUE HACER ALGUNA PONDERACIÓN POR SUBMUESTREO A POSTERIORI##
write.table(result,file=c("results_Pto1_Donana_2020.csv"),append=TRUE,quote=TRUE,sep=";",dec=".",col.names=TRUE)
head(result)