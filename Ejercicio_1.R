
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
var_jvm_1<-VE.HT.Total.NHT(jvm_1, pk_1, MatPkl_1)
var_gc_1<-VE.HT.Total.NHT(gc_1, pk_1, MatPkl_1)
var_noreg_1<-VE.HT.Total.NHT(noreg_1, pk_1, MatPkl_1)
var_nulos_1<-VE.HT.Total.NHT(nulos_1, pk_1, MatPkl_1)

MatPkl_2           <- Pkl.Hajek.s(pk_2)
var_epn_2<-VE.HT.Total.NHT(epn_2, pk_2, MatPkl_2)
var_amlo_2<-VE.HT.Total.NHT(amlo_2, pk_2, MatPkl_2)
var_jvm_2<-VE.HT.Total.NHT(jvm_2, pk_2, MatPkl_2)
var_gc_2<-VE.HT.Total.NHT(gc_2, pk_2, MatPkl_2)
var_noreg_2<-VE.HT.Total.NHT(noreg_2, pk_2, MatPkl_2)
var_nulos_2<-VE.HT.Total.NHT(nulos_2, pk_2, MatPkl_2)


MatPkl_3           <- Pkl.Hajek.s(pk_3)
var_epn_3<-VE.HT.Total.NHT(epn_3, pk_3, MatPkl_3)
var_amlo_3<-VE.HT.Total.NHT(amlo_3, pk_3, MatPkl_3)
var_jvm_3<-VE.HT.Total.NHT(jvm_3, pk_3, MatPkl_3)
var_gc_3<-VE.HT.Total.NHT(gc_3, pk_3, MatPkl_3)
var_noreg_3<-VE.HT.Total.NHT(noreg_3, pk_3, MatPkl_3)
var_nulos_3<-VE.HT.Total.NHT(nulos_3, pk_3, MatPkl_3)

MatPkl_4           <- Pkl.Hajek.s(pk_4)
var_epn_4<-VE.HT.Total.NHT(epn_4, pk_4, MatPkl_4)
var_amlo_4<-VE.HT.Total.NHT(amlo_4, pk_4, MatPkl_4)
var_jvm_4<-VE.HT.Total.NHT(jvm_4, pk_4, MatPkl_4)
var_gc_4<-VE.HT.Total.NHT(gc_4, pk_4, MatPkl_4)
var_noreg_4<-VE.HT.Total.NHT(noreg_4, pk_4, MatPkl_4)
var_nulos_4<-VE.HT.Total.NHT(nulos_4, pk_4, MatPkl_4)

MatPkl_5           <- Pkl.Hajek.s(pk_5)
var_epn_5<-VE.HT.Total.NHT(epn_5, pk_5, MatPkl_5)
var_amlo_5<-VE.HT.Total.NHT(amlo_5, pk_5, MatPkl_5)
var_jvm_5<-VE.HT.Total.NHT(jvm_5, pk_5, MatPkl_5)
var_gc_5<-VE.HT.Total.NHT(gc_5, pk_5, MatPkl_5)
var_noreg_5<-VE.HT.Total.NHT(noreg_5, pk_5, MatPkl_5)
var_nulos_5<-VE.HT.Total.NHT(nulos_5, pk_5, MatPkl_5)

std_epn_1   <- sqrt(var_epn_1)
std_amlo_1 <- sqrt(var_amlo_1)
std_jvm_1 <- sqrt(var_jvm_1)
std_gc_1 <- sqrt(var_gc_1)
std_noreg_1 <- sqrt(var_noreg_1)
std_nulos_1 <- sqrt(var_nulos_1)


std_epn_2   <- sqrt(var_epn_2)
std_amlo_2 <- sqrt(var_amlo_2)
std_jvm_2 <- sqrt(var_jvm_2)
std_gc_2 <- sqrt(var_gc_2)
std_noreg_2 <- sqrt(var_noreg_2)
std_nulos_2 <- sqrt(var_nulos_2)

std_epn_3   <- sqrt(var_epn_3)
std_amlo_3 <- sqrt(var_amlo_3)
std_jvm_3 <- sqrt(var_jvm_3)
std_gc_3 <- sqrt(var_gc_3)
std_noreg_3 <- sqrt(var_noreg_3)
std_nulos_3 <- sqrt(var_nulos_3)

std_epn_4   <- sqrt(var_epn_4)
std_amlo_4 <- sqrt(var_amlo_4)
std_jvm_4 <- sqrt(var_jvm_4)
std_gc_4 <- sqrt(var_gc_4)
std_noreg_4 <- sqrt(var_noreg_4)
std_nulos_4 <- sqrt(var_nulos_4)

std_epn_5   <- sqrt(var_epn_5)
std_amlo_5 <- sqrt(var_amlo_5)
std_jvm_5 <- sqrt(var_jvm_5)
std_gc_5 <- sqrt(var_gc_5)
std_noreg_5 <- sqrt(var_noreg_5)
std_nulos_5 <- sqrt(var_nulos_5)



# Y ahora calculemos nuestro error absoluto o precision al 95% de confianza
alpha              <- 0.05
abs_epn_1    <- std_epn_1*qnorm(1-alpha/2)
abs_amlo_1    <- std_amlo_1*qnorm(1-alpha/2)
abs_jvm_1    <- std_jvm_1*qnorm(1-alpha/2)
abs_gc_1    <- std_gc_1*qnorm(1-alpha/2)
abs_noreg_1    <- std_noreg_1*qnorm(1-alpha/2)
abs_nulos_1    <- std_nulos_1*qnorm(1-alpha/2)

abs_epn_2    <- std_epn_2*qnorm(1-alpha/2)
abs_amlo_2    <- std_amlo_2*qnorm(1-alpha/2)
abs_jvm_2    <- std_jvm_2*qnorm(1-alpha/2)
abs_gc_2    <- std_gc_2*qnorm(1-alpha/2)
abs_noreg_2    <- std_noreg_2*qnorm(1-alpha/2)
abs_nulos_2    <- std_nulos_2*qnorm(1-alpha/2)


abs_epn_3    <- std_epn_3*qnorm(1-alpha/2)
abs_amlo_3    <- std_amlo_3*qnorm(1-alpha/2)
abs_jvm_3    <- std_jvm_3*qnorm(1-alpha/2)
abs_gc_3    <- std_gc_3*qnorm(1-alpha/2)
abs_noreg_3    <- std_noreg_3*qnorm(1-alpha/2)
abs_nulos_3    <- std_nulos_3*qnorm(1-alpha/2)

abs_epn_4    <- std_epn_4*qnorm(1-alpha/2)
abs_amlo_4    <- std_amlo_4*qnorm(1-alpha/2)
abs_jvm_4    <- std_jvm_4*qnorm(1-alpha/2)
abs_gc_4    <- std_gc_4*qnorm(1-alpha/2)
abs_noreg_4    <- std_noreg_4*qnorm(1-alpha/2)
abs_nulos_4    <- std_nulos_4*qnorm(1-alpha/2)


abs_epn_5    <- std_epn_5*qnorm(1-alpha/2)
abs_amlo_5    <- std_amlo_5*qnorm(1-alpha/2)
abs_jvm_5    <- std_jvm_5*qnorm(1-alpha/2)
abs_gc_5    <- std_gc_5*qnorm(1-alpha/2)
abs_noreg_5    <- std_noreg_5*qnorm(1-alpha/2)
abs_nulos_5    <- std_nulos_5*qnorm(1-alpha/2)

# Entonces nuestros 4 intervalos de confianza a un nivel de 95% son:
liminf_epn_1  <- v_epn_1 - abs_epn_1
liminf_amlo_1  <- v_amlo_1 - abs_amlo_1
liminf_jvm_1  <- v_jvm_1 - abs_jvm_1
liminf_gc_1  <- v_gc_1 - abs_gc_1
liminf_noreg_1  <- v_noreg_1 - abs_noreg_1
liminf_nulos_1  <- v_nulos_1 - abs_nulos_1

limsup_epn_1  <- v_epn_1 + abs_epn_1
limsup_amlo_1  <- v_amlo_1 + abs_amlo_1
limsup_jvm_1  <- v_jvm_1 + abs_jvm_1
limsup_gc_1  <- v_gc_1 + abs_gc_1
limsup_noreg_1  <- v_noreg_1 + abs_noreg_1
limsup_nulos_1  <- v_nulos_1 + abs_nulos_1


liminf_epn_2  <- v_epn_2 - abs_epn_2
liminf_amlo_2  <- v_amlo_2 - abs_amlo_2
liminf_jvm_2  <- v_jvm_2 - abs_jvm_2
liminf_gc_2  <- v_gc_2 - abs_gc_2
liminf_noreg_2  <- v_noreg_2 - abs_noreg_2
liminf_nulos_2  <- v_nulos_2 - abs_nulos_2

limsup_epn_2  <- v_epn_2 + abs_epn_2
limsup_amlo_2  <- v_amlo_2 + abs_amlo_2
limsup_jvm_2  <- v_jvm_2 + abs_jvm_2
limsup_gc_2  <- v_gc_2 + abs_gc_2
limsup_noreg_2  <- v_noreg_2 + abs_noreg_2
limsup_nulos_2  <- v_nulos_2 + abs_nulos_2



liminf_epn_3  <- v_epn_3 - abs_epn_3
liminf_amlo_3  <- v_amlo_3 - abs_amlo_3
liminf_jvm_3  <- v_jvm_3 - abs_jvm_3
liminf_gc_3  <- v_gc_3 - abs_gc_3
liminf_noreg_3  <- v_noreg_3 - abs_noreg_3
liminf_nulos_3  <- v_nulos_3 - abs_nulos_3

limsup_epn_3  <- v_epn_3 + abs_epn_3
limsup_amlo_3  <- v_amlo_3 + abs_amlo_3
limsup_jvm_3  <- v_jvm_3 + abs_jvm_3
limsup_gc_3  <- v_gc_3 + abs_gc_3
limsup_noreg_3  <- v_noreg_3 + abs_noreg_3
limsup_nulos_3  <- v_nulos_3 + abs_nulos_3

liminf_epn_4  <- v_epn_4 - abs_epn_4
liminf_amlo_4  <- v_amlo_4 - abs_amlo_4
liminf_jvm_4  <- v_jvm_4 - abs_jvm_4
liminf_gc_4  <- v_gc_4 - abs_gc_4
liminf_noreg_4  <- v_noreg_4 - abs_noreg_4
liminf_nulos_4  <- v_nulos_4 - abs_nulos_4

limsup_epn_4  <- v_epn_4 + abs_epn_4
limsup_amlo_4  <- v_amlo_4 + abs_amlo_4
limsup_jvm_4  <- v_jvm_4 + abs_jvm_4
limsup_gc_4  <- v_gc_4 + abs_gc_4
limsup_noreg_4  <- v_noreg_4 + abs_noreg_4
limsup_nulos_4  <- v_nulos_4 + abs_nulos_4

liminf_epn_5  <- v_epn_5 - abs_epn_5
liminf_amlo_5  <- v_amlo_5 - abs_amlo_5
liminf_jvm_5  <- v_jvm_5 - abs_jvm_5
liminf_gc_5  <- v_gc_5 - abs_gc_5
liminf_noreg_5  <- v_noreg_5 - abs_noreg_5
liminf_nulos_5  <- v_nulos_5 - abs_nulos_5

limsup_epn_5  <- v_epn_5 + abs_epn_5
limsup_amlo_5  <- v_amlo_5 + abs_amlo_5
limsup_jvm_5  <- v_jvm_5 + abs_jvm_5
limsup_gc_5  <- v_gc_5 + abs_gc_5
limsup_noreg_5  <- v_noreg_5 + abs_noreg_5
limsup_nulos_5  <- v_nulos_5 + abs_nulos_5

# Ahora calculemos el coeficiente de variaci?n estimado de cada una de las 4 estimaciones...
cve_epn_1      <- std_epn_1/v_epn_1
cve_amlo_1      <- std_amlo_1/v_amlo_1
cve_jvm_1      <- std_jvm_1/v_jvm_1
cve_gc_1      <- std_gc_1/v_gc_1
cve_noreg_1      <- std_noreg_1/v_noreg_1
cve_nulos_1      <- std_nulos_1/v_nulos_1

cve_epn_2      <- std_epn_2/v_epn_2
cve_amlo_2      <- std_amlo_2/v_amlo_2
cve_jvm_2      <- std_jvm_2/v_jvm_2
cve_gc_2      <- std_gc_2/v_gc_2
cve_noreg_2      <- std_noreg_2/v_noreg_2
cve_nulos_2      <- std_nulos_2/v_nulos_2

cve_epn_3      <- std_epn_3/v_epn_3
cve_amlo_3      <- std_amlo_3/v_amlo_3
cve_jvm_3      <- std_jvm_3/v_jvm_3
cve_gc_3      <- std_gc_3/v_gc_3
cve_noreg_3      <- std_noreg_3/v_noreg_3
cve_nulos_3      <- std_nulos_3/v_nulos_3

cve_epn_4      <- std_epn_4/v_epn_4
cve_amlo_4      <- std_amlo_4/v_amlo_4
cve_jvm_4      <- std_jvm_4/v_jvm_4
cve_gc_4      <- std_gc_4/v_gc_4
cve_noreg_4      <- std_noreg_4/v_noreg_4
cve_nulos_4      <- std_nulos_4/v_nulos_4

cve_epn_5      <- std_epn_5/v_epn_5
cve_amlo_5      <- std_amlo_5/v_amlo_5
cve_jvm_5      <- std_jvm_5/v_jvm_5
cve_gc_5      <- std_gc_5/v_gc_5
cve_noreg_5      <- std_noreg_5/v_noreg_5
cve_nulos_5      <- std_nulos_5/v_nulos_5
