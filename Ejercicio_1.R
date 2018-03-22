
library(readr)
library(dplyr)
library(sampling)
library(samplingVarEst)

elecciones<- read_csv("~/Documents/MaestriaCD/Trabajo_Muestreo/Trabajo_final/007_ResElect_NACIONAL_seccs_PRES2012.csv")

## Lo primero que hay que hacer es sumar los partidos por coalicion

elecciones<-elecciones %>% mutate(EPN=PRI+PVEM+`COALICION PRI PVEM`) %>%
  mutate(AMLO=PRD + PT + `MOVIMIENTO CIUDADANO` + `COALICION PRD PT MC` +  `COALICION PRD PT` +
           `COALICION PRD MC`+`COALICION PT MC`) %>%
  mutate(JVM=PAN) %>%
  mutate(GC= `NUEVA ALIANZA`) %>%
  mutate(NoReg= `NO REGISTRADOS`) %>%
  mutate(Nulos=NULOS)  %>%
  select(-(8:21))
elecciones[is.na(elecciones)] <- 0

#Una vez que hicimos esto procedemos al responder las preguntas

## Numero de secciones por muestra
n1 <- 6500
n2 <- 500
n3 <- 250
n4 <- 100
n5 <- 50

## El N poblacional es

N<-nrow(elecciones)

## Extraer muestra n bajo muestreo aleatorio simple

set.seed(17)

muestra_1<-srswor1(n1,N)
muestra_2<-srswor1(n2,N)
muestra_3<-srswor1(n3,N)
muestra_4<-srswor1(n4,N)
muestra_5<-srswor1(n5,N)

## Entonces aplicamos estas muestas que obtuvimos a nuestros variables de interes
epn_1<-elecciones$EPN[muestra_1==1]
amlo_1<-elecciones$AMLO[muestra_1==1]
jvm_1<-elecciones$JVM[muestra_1==1]
gc_1<-elecciones$GC[muestra_1==1]
noreg_1<-elecciones$NoReg[muestra_1==1]
nulos_1<-elecciones$Nulos[muestra_1==1]

epn_2<-elecciones$EPN[muestra_2==1]
amlo_2<-elecciones$AMLO[muestra_2==1]
jvm_2<-elecciones$JVM[muestra_2==1]
gc_2<-elecciones$GC[muestra_2==1]
noreg_2<-elecciones$NoReg[muestra_2==1]
nulos_2<-elecciones$Nulos[muestra_2==1]

epn_3<-elecciones$EPN[muestra_3==1]
amlo_3<-elecciones$AMLO[muestra_3==1]
jvm_3<-elecciones$JVM[muestra_3==1]
gc_3<-elecciones$GC[muestra_3==1]
noreg_3<-elecciones$NoReg[muestra_3==1]
nulos_3<-elecciones$Nulos[muestra_3==1]

epn_4<-elecciones$EPN[muestra_4==1]
amlo_4<-elecciones$AMLO[muestra_4==1]
jvm_4<-elecciones$JVM[muestra_4==1]
gc_4<-elecciones$GC[muestra_4==1]
noreg_4<-elecciones$NoReg[muestra_4==1]
nulos_4<-elecciones$Nulos[muestra_4==1]

epn_5<-elecciones$EPN[muestra_5==1]
amlo_5<-elecciones$AMLO[muestra_5==1]
jvm_5<-elecciones$JVM[muestra_5==1]
gc_5<-elecciones$GC[muestra_5==1]
noreg_5<-elecciones$NoReg[muestra_5==1]
nulos_5<-elecciones$Nulos[muestra_5==1]

##Estimar el total de votos por cada uno de los candidatos de 2012 (EPN, AMLO, JVM, GQT, NoReg, y Nulos -
#notar que hay coaliciones, tendrá que crear nuevas columnas en los datos – creará un total de 6 columnas, que son las que utilizará -, para más información ver:


## Para usar el estimador de H-T primero tenemos que encontrar los Pk´s, estos son diferentes para cada muestra
pk_1<- rep(n1/N, times=n1)
pk_2<- rep(n2/N, times=n2)
pk_3<- rep(n3/N, times=n3)
pk_4<- rep(n4/N, times=n4)
pk_5<- rep(n5/N, times=n5)

v_epn_1<-Est.Total.NHT(epn_1, pk_1)
v_amlo_1<-Est.Total.NHT(amlo_1, pk_1)
v_jvm_1<-Est.Total.NHT(jvm_1, pk_1)
v_gc_1<-Est.Total.NHT(gc_1, pk_1)
v_noreg_1<-Est.Total.NHT(noreg_1, pk_1)
v_nulos_1<-Est.Total.NHT(nulos_1, pk_1)

v_epn_2<-Est.Total.NHT(epn_2, pk_2)
v_amlo_2<-Est.Total.NHT(amlo_2, pk_2)
v_jvm_2<-Est.Total.NHT(jvm_2, pk_2)
v_gc_2<-Est.Total.NHT(gc_2, pk_2)
v_noreg_2<-Est.Total.NHT(noreg_2, pk_2)
v_nulos_2<-Est.Total.NHT(nulos_2, pk_2)

v_epn_3<-Est.Total.NHT(epn_3, pk_3)
v_amlo_3<-Est.Total.NHT(amlo_3, pk_3)
v_jvm_3<-Est.Total.NHT(jvm_3, pk_3)
v_gc_3<-Est.Total.NHT(gc_3, pk_3)
v_noreg_3<-Est.Total.NHT(noreg_3, pk_3)
v_nulos_3<-Est.Total.NHT(nulos_3, pk_3)

v_epn_4<-Est.Total.NHT(epn_4, pk_4)
v_amlo_4<-Est.Total.NHT(amlo_4, pk_4)
v_jvm_4<-Est.Total.NHT(jvm_4, pk_4)
v_gc_4<-Est.Total.NHT(gc_4, pk_4)
v_noreg_4<-Est.Total.NHT(noreg_4, pk_4)
v_nulos_4<-Est.Total.NHT(nulos_4, pk_4)

v_epn_5<-Est.Total.NHT(epn_5, pk_5)
v_amlo_5<-Est.Total.NHT(amlo_5, pk_5)
v_jvm_5<-Est.Total.NHT(jvm_5, pk_5)
v_gc_5<-Est.Total.NHT(gc_5, pk_5)
v_noreg_5<-Est.Total.NHT(noreg_5, pk_5)
v_nulos_5<-Est.Total.NHT(nulos_5, pk_5)


MatPkl_1           <- Pkl.Hajek.s(pk_1)
var_epn_1<-VE.HT.Total.NHT(epn_1, pk_1, MatPkl_1)
var_amlo_1<-VE.HT.Total.NHT(amlo_1, pk_1, MatPkl_1)


