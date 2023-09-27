## CÁLCULOS de DENSIDAD, D15, TALLA MEDIA+SD (Densidad sin incorporar daños!)

## PLOTS DISTRIBUICIONES TALLAS POR ESTACION DE MUESTREO

#M. DELGADO, 17/05/2021

##DATOS

dataP<-merge(Data_sample_POBL,
             Data_size_POBL,
             by="ID_codificado_punto")#Creados a partir de Data_sample y Data_size, y seleccionando poblacional solo

dataP<-dataP[-c(8,9,10,11,22,36,37,38,39,40,44,45,46,47,48,49,50,51,54)]

names(dataP)<-c("ID",
                "month",
                "Date",
                "Beach",
                "Sampling.point",
                "m_track",
                "tow_time",
                "Lat","Long",
                "rastro",
                "mariscador",
                "SW",
                "SWsub",
                "CSWsub",
                "MCSWsub",
                "fps",
                "CSW",
                "MSCW",
                "DCSWsub",
                "DCSW",
                "TCSW",
                "Btotal",
                "Categoria",
                "CAT",
                "Nmedida",
                "fpn",
                "NtotalCSW", 
                "Ndanossub",
                 "Ndanos",
                "Ntotal",
                "area",
                "bio",
                "dens",
                "size",
                "sizeE")

#Herramienta ayuda para Manejo datos
unique(Data_sample_POBL$ID)
unique(Data_size_POBL$ID)
unique(data$ID)
a<-count(Data_size_POBL,"ID")
b<-count(data,"ID")


##RESULTS: Distribuciones de frecuencia

#SELECCION DATOS POBLACIONAL

data_station<-dataP[dataP$Sampling.point==9,]

#PLOTS TALLAS
opar<-par()
par(oma=c(2,2,2,2),mar=c(1,2,1,2))#Se puede variar para el tema de los márgenes
ppi <- 300##resolución gráfica
multiplot<-function(row,col){par(mfrow=c(row,col),pty="s")}#Función para salida de gráficos múltiples
multiplot(3,2)##Preparo el area de salida de los gráficos 3x
v<-25

#Salida N15 y d15 (indice de reclutamiento)
#months<-unique(data_station$month)
months<-c(1:12)
t<-length(months)
month<-rep(0,t)
area<-rep(0,t)
N15<-rep(0,t) 
d15<-rep(0,t)
resultN15<- data.frame(month,area,N15,d15) ###### CREAR NUEVA TABLA CON DATOS DEL VECTOR
head(resultN15)

#months<-10#Para hacer solo un mes concreto en bucle
#months<-c(1:12)#Para 2021, puntos 2, 4 y 6
months<-c(1,2,3,4,5,6,7,8,10,11,12) #Falta mes septiembre 2022
months<-c(3,7,11)#Espacial en 2022
#months<-c(1,4,7,10,12)#Para 2021, puntos 9, 10 y 11

for (i in months){
  y<-seq(3,40,by=1)
  data_month<-data_station[data_station$month==i,]
  if(unique(data_month$CAT==1)){
    z<-cut(data_month$sizeE,breaks=y)
    counts<-table(z)
    total<-sum(counts)
    fpn<-unique(data_month$fpn)
    fps<-unique(data_month$fps)
    counts_s<-counts*fpn*fps
    
    pct1<-prop.table(counts_s)
    pct1<-pct1*100#frecuencia en %
    pct1df<-as.data.frame(pct1)
    pct1df$z<-c(4:40)
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
    d15<-N15/((unique(data_month$m_track))*0.445)
    resultN15[i,1]<-i
    resultN15[i,2]<-unique(data_month$m_track)*0.445
    resultN15[i,3]<-N15
    resultN15[i,4]<-d15
  }else{
    data_month_g<-data_month[data_month$month==i&data_month$Categoria=="g",]
    zg<-cut(data_month_g$sizeE,breaks=y)
    counts_g<-table(zg)
    fpn_g<-unique(data_month_g$fpn)
    counts_g_p<-counts_g*fpn_g
    
    data_month_p<-data_month[data_month$month==i&data_month$Categoria=="p",]
    zp<-cut(data_month_p$sizeE,breaks=y)
    counts_p<-table(zp)
    fpn_p<-unique(data_month_p$fpn)
    counts_p_p<-counts_p*fpn_p
    
    fps<-unique(data_month_g$fps)
    counts_t<-(counts_g_p+counts_p_p)*fps#Ponderacion para submuestra con cascajo
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
    counts_df$z<-c(4:40)
    counts_df15<-counts_df[counts_df$z<15,]
    N15<-round(sum(counts_df15$Freq),digits=2)#Incluye hasta 15.99mm
    d15<-N15/(unique(data_month$m_track*0.445))
    resultN15[i,1]<-i
    resultN15[i,2]<-unique(data_month$m_track)*0.445
    resultN15[i,3]<-N15
    resultN15[i,4]<-d15
  }
}

write.table(resultN15,file=c("resultsD15_Pto10_2022.csv"),append=TRUE,quote=TRUE,sep=";",dec=".",col.names=TRUE)
head(resultN15)



#mtext("Donana_poblacional_Pto1_2020",side=3,outer=T,font=2)#Para multiplot

#title(main="Punto 5")#Para un solo plot


#SALIDA DE DATOS (estadística básica de tallas) + DENSIDAD

#Dataframe de salida
months<-unique(data_station$months)
months<-c(1:12)#Para 2022
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
#months<-c(1:12)#Para 2021
months<-c(3,7,11)#Para 2022, puntos 9, 10 y 11
months<-c(1,2,3,4,5,6,7,8,10,11,12)

for (i in months){
  y<-seq(3,40,by=1)
  data_month<-data_station[data_station$month==i,]
  if(unique(data_month$CAT==1)){
    z<-cut(data_month$sizeE,breaks=y)
    counts<-table(z)#frecuencia en numero
    fpn<-unique(data_month$fpn)
    fps<-unique(data_month$fps)
    counts_s<-counts*fpn*fps
    total<-sum(counts_s)
    
    size<-data_month$sizeE
    media<-as.numeric(round(sum(size)/total,digits=2))##MEDIA
    sd<-sqrt(((sum((size-media)^2))/(total-1)))
    sdr<-round(sd,digits=2)#DESVIACION
    track<-unique(data_month$m_track)
    area<-track*0.445#Ancho boca rastro 0.445m poblacional/0.435 ancho boca rastro comercial
    dens<-total/area
    result[i,1]<-i
    result[i,2]<-total
    result[i,3]<-media
    result[i,4]<-sdr
    result[i,5]<-track
    result[i,6]<-area
    result[i,7]<-dens
  }else{
    data_month_g<-data_month[data_month$Categoria=="g",]
    zg<-cut(data_month_g$sizeE,breaks=y)
    counts_g<-table(zg)
    fpn_g<-unique(data_month_g$fpn)
    counts_g_p<-counts_g*fpn_g
    
    data_month_p<-data_month[data_month$Categoria=="p",]
    zp<-cut(data_month_p$sizeE,breaks=y)
    counts_p<-table(zp)
    fpn_p<-unique(data_month_p$fpn)
    counts_p_p<-counts_p*fpn_p
    
    fps<-unique(data_month$fps)
    counts_t<-(counts_g_p+counts_p_p)*fps
    total<-sum(counts_t)
    total
    counts_tdf<-as.data.frame(counts_t)
    clase<-c(4:40)
    counts_tdf$clase<-clase
    size_t<-counts_tdf$Freq*counts_tdf$clase
    media<-as.numeric(round(sum(size_t)/total,digits=2))##MEDIA
    sd<-sqrt(sum(((counts_tdf$clase-media)^2)*counts_tdf$Freq)/(total))
    sdr<-round(sd,digits=2)#DESVIACION
    track<-unique(data_month$m_track)
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

write.table(result,file=c("results_Pto9_2022.csv"),append=TRUE,quote=TRUE,sep=";",dec=".",col.names=TRUE)
head(result)