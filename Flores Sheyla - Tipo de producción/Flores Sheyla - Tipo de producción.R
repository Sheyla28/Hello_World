Datos <- read.csv("Datos_piezas.csv", header = TRUE, sep = ",")
names(Datos)

attach(Datos)

Información <- read.csv("Información.csv")
attach(Información)
names(Información)
data.frame(Información)

library(dplyr)
ls("package:dplyr")

#CORTE
data.frame(Información$"JL", data.frame(Información$Corte))


Régimen_trabajo_Corte <- 8
Régimen_trabajo_Corte

Horas_mtto_Corte <- 135
Horas_mtto_Corte

Cantidad_equipos_Corte <- 3
Cantidad_equipos_Corte

Imprevistos_Corte <- 200
Imprevistos_Corte

Días_año_Corte <- 365
Días_año_Corte

Horas_día_Corte <- 24
Horas_día_Corte

Vacaciones_Corte <- 0*Régimen_trabajo_Corte*Cantidad_equipos_Corte
Vacaciones_Corte

#FONDO PRODUCTIVO TOTAL 
FPT <- Días_año_Corte*Horas_día_Corte*Cantidad_equipos_Corte 
FPT
#FONDO POR REQUERIMIENTOS TECNOLOGICOS  
FRT <- Horas_mtto_Corte*Cantidad_equipos_Corte
FRT
#FONDO POR REGIMEN LABORAL
FRL <- (Régimen_trabajo_Corte*Días_año_Corte*Cantidad_equipos_Corte)+(Vacaciones_Corte)
FRL
#FONDO POR OTRAS CAUSAS
FOC <- Imprevistos_Corte*Cantidad_equipos_Corte
FOC
#FONDO PRODUCTIVO DISPONIBLE
FPD <- FPT-FRT-FRL-FOC 
FPD #h-e/año

#COEFICIENTE DE CARGA 
data.frame(Datos$"Plan_anual", data.frame(Datos$Corte))
Pz_Ntij_Corte <- (Datos$"Plan_anual")* (Datos$"Corte")
Pz_Ntij_Corte
sum(Pz_Ntij_Corte)

FPD_Np_Corte <- FPD*Cantidad_equipos_Corte
FPD_Np_Corte

Ccj_Corte <- sum(Pz_Ntij_Corte)/FPD_Np_Corte
Ccj_Corte

if(Ccj_Corte >= 0.85){
"Masiva"
} else if(Ccj_Corte>=0.2 & Ccj_Corte<0.85){
"Gran Serie"
}else if(Ccj_Corte>=0.08 & Ccj_Corte<0.2){
"Mediana serie"
} else if(Ccj_Corte>=0.04 & Ccj_Corte<0.08){
"Pequeña serie"
} else if(Ccj_Corte < 0.04){
"Unitaria"
} else {
"El número es nulo"
}
print(Ccj_Corte)

#FREGADO
data.frame(Información)
data.frame(Información$"JL", data.frame(Información$Fregado))


Régimen_trabajo_Fregado <- 8
Régimen_trabajo_Fregado

Horas_mtto_Fregado <- 150
Horas_mtto_Fregado

Cantidad_equipos_Fregado <- 2
Cantidad_equipos_Fregado

Imprevistos_Fregado <- 200
Imprevistos_Fregado

Días_año_Fregado <- 365
Días_año_Fregado

Horas_día_Fregado <- 24
Horas_día_Fregado

Vacaciones_Fregado <- 0*Régimen_trabajo_Fregado*Cantidad_equipos_Fregado
Vacaciones_Fregado

#FONDO PRODUCTIVO TOTAL 
FPT <- Días_año_Fregado*Horas_día_Fregado*Cantidad_equipos_Fregado 
FPT
#FONDO POR REQUERIMIENTOS TECNOLOGICOS  
FRT <- Horas_mtto_Fregado*Cantidad_equipos_Fregado
FRT
#FONDO POR REGIMEN LABORAL
FRL <- (Régimen_trabajo_Fregado*Días_año_Fregado*Cantidad_equipos_Fregado)+(Vacaciones_Fregado)
FRL
#FONDO POR OTRAS CAUSAS
FOC <- Imprevistos_Fregado*Cantidad_equipos_Fregado
FOC
#FONDO PRODUCTIVO DISPONIBLE
FPD <- FPT-FRT-FRL-FOC 
FPD #h-e/año

#COEFICIENTE DE CARGA 
data.frame(Datos$"Plan_anual", data.frame(Datos$Fregado))
Pz_Ntij_Fregado <- (Datos$"Plan_anual")* (Datos$"Fregado")
Pz_Ntij_Fregado
sum(Pz_Ntij_Fregado)

FPD_Np_Fregado <- FPD*Cantidad_equipos_Fregado
FPD_Np_Fregado

Ccj_Fregado <- sum(Pz_Ntij_Fregado)/FPD_Np_Fregado
Ccj_Fregado

if(Ccj_Fregado >= 0.85){
  "Masiva"
} else if(Ccj_Fregado>=0.2 & Ccj_Fregado<0.85){
  "Gran Serie"
}else if(Ccj_Fregado>=0.08 & Ccj_Fregado<0.2){
  "Mediana serie"
} else if(Ccj_Fregado>=0.04 & Ccj_Fregado<0.08){
  "Pequeña serie"
} else if(Ccj_Fregado < 0.04){
  "Unitaria"
} else {
  "El número es nulo"
}
print(Ccj_Fregado)

#CEPILLADO
data.frame(Información)
data.frame(Información$"JL", data.frame(Información$Cepillado))


Régimen_trabajo_Cepillado <- 8
Régimen_trabajo_Cepillado

Horas_mtto_Cepillado <- 200
Horas_mtto_Cepillado

Cantidad_equipos_Cepillado <- 4
Cantidad_equipos_Cepillado

Imprevistos_Cepillado <- 200
Imprevistos_Cepillado

Días_año_Cepillado <- 365
Días_año_Cepillado

Horas_día_Cepillado <- 24
Horas_día_Cepillado

Vacaciones_Cepillado <- 0*Régimen_trabajo_Cepillado*Cantidad_equipos_Cepillado
Vacaciones_Cepillado

#FONDO PRODUCTIVO TOTAL 
FPT <- Días_año_Cepillado*Horas_día_Cepillado*Cantidad_equipos_Cepillado 
FPT
#FONDO POR REQUERIMIENTOS TECNOLOGICOS  
FRT <- Horas_mtto_Cepillado*Cantidad_equipos_Cepillado
FRT
#FONDO POR REGIMEN LABORAL
FRL <- (Régimen_trabajo_Cepillado*Días_año_Cepillado*Cantidad_equipos_Cepillado)+(Vacaciones_Cepillado)
FRL
#FONDO POR OTRAS CAUSAS
FOC <- Imprevistos_Cepillado*Cantidad_equipos_Cepillado
FOC
#FONDO PRODUCTIVO DISPONIBLE
FPD <- FPT-FRT-FRL-FOC 
FPD #h-e/año

#COEFICIENTE DE CARGA 
data.frame(Datos$"Plan_anual", data.frame(Datos$Cepillado))
Pz_Ntij_Cepillado <- (Datos$"Plan_anual")* (Datos$"Cepillado")
Pz_Ntij_Cepillado
sum(Pz_Ntij_Cepillado)

FPD_Np_Cepillado <- FPD*Cantidad_equipos_Cepillado
FPD_Np_Cepillado

Ccj_Cepillado <- sum(Pz_Ntij_Cepillado)/FPD_Np_Cepillado
Ccj_Cepillado

if(Ccj_Cepillado >= 0.85){
  "Masiva"
} else if(Ccj_Cepillado>=0.2 & Ccj_Cepillado<0.85){
  "Gran Serie"
}else if(Ccj_Cepillado>=0.08 & Ccj_Cepillado<0.2){
  "Mediana serie"
} else if(Ccj_Cepillado>=0.04 & Ccj_Cepillado<0.08){
  "Pequeña serie"
} else if(Ccj_Cepillado < 0.04){
  "Unitaria"
} else {
  "El número es nulo"
}
print(Ccj_Cepillado)

#TALADRADO
data.frame(Información)
data.frame(Información$"JL", data.frame(Información$Taladrado))


Régimen_trabajo_Taladrado <- 8
Régimen_trabajo_Taladrado

Horas_mtto_Taladrado <- 300
Horas_mtto_Taladrado

Cantidad_equipos_Taladrado <- 3
Cantidad_equipos_Taladrado

Imprevistos_Taladrado <- 200
Imprevistos_Taladrado

Días_año_Taladrado <- 365
Días_año_Taladrado

Horas_día_Taladrado <- 24
Horas_día_Taladrado

Vacaciones_Taladrado <- 0*Régimen_trabajo_Taladrado*Cantidad_equipos_Taladrado
Vacaciones_Taladrado

#FONDO PRODUCTIVO TOTAL 
FPT <- Días_año_Taladrado*Horas_día_Taladrado*Cantidad_equipos_Taladrado 
FPT
#FONDO POR REQUERIMIENTOS TECNOLOGICOS  
FRT <- Horas_mtto_Taladrado*Cantidad_equipos_Taladrado
FRT
#FONDO POR REGIMEN LABORAL
FRL <- (Régimen_trabajo_Taladrado*Días_año_Taladrado*Cantidad_equipos_Taladrado)+(Vacaciones_Taladrado)
FRL
#FONDO POR OTRAS CAUSAS
FOC <- Imprevistos_Taladrado*Cantidad_equipos_Taladrado
FOC
#FONDO PRODUCTIVO DISPONIBLE
FPD <- FPT-FRT-FRL-FOC 
FPD #h-e/año

#COEFICIENTE DE CARGA 
data.frame(Datos$"Plan_anual", data.frame(Datos$Taladrado))
Pz_Ntij_Taladrado <- (Datos$"Plan_anual")* (Datos$"Taladrado")
Pz_Ntij_Taladrado
sum(Pz_Ntij_Taladrado)

FPD_Np_Taladrado <- FPD*Cantidad_equipos_Taladrado
FPD_Np_Taladrado

Ccj_Taladrado <- sum(Pz_Ntij_Taladrado)/FPD_Np_Taladrado
Ccj_Taladrado

if(Ccj_Taladrado >= 0.85){
  "Masiva"
} else if(Ccj_Taladrado>=0.2 & Ccj_Taladrado<0.85){
  "Gran Serie"
}else if(Ccj_Taladrado>=0.08 & Ccj_Taladrado<0.2){
  "Mediana serie"
} else if(Ccj_Taladrado>=0.04 & Ccj_Taladrado<0.08){
  "Pequeña serie"
} else if(Ccj_Taladrado < 0.04){
  "Unitaria"
} else {
  "El número es nulo"
}
print(Ccj_Taladrado)

#COEFICIENTE DE OPERACIONES FIJADAS
#O
Datos_piezas_NA <- read.csv("Datos_piezas_NA.csv")

length(Datos_piezas_NA$Corte) + length(Datos_piezas_NA$Fregado) + length(Datos_piezas_NA$Cepillado) + length(Datos_piezas_NA$Taladrado)
length(as.numeric(Datos_piezas_NA$Corte)) + length(as.numeric(Datos_piezas_NA$Fregado)) + length(as.numeric(Datos_piezas_NA$Cepillado)) + length(as.numeric(Datos_piezas_NA$Taladrado))
!is.na(Datos_piezas_NA$Corte) + !is.na(Datos_piezas_NA$Fregado) + !is.na(Datos_piezas_NA$Cepillado) + !is.na(Datos_piezas_NA$Taladrado)
which(!is.na(Datos_piezas_NA$Corte))
which(!is.na(Datos_piezas_NA$Fregado))
which(!is.na(Datos_piezas_NA$Cepillado))
which(!is.na(Datos_piezas_NA$Taladrado))
length(which(!is.na(Datos_piezas_NA$Corte))) + length(which(!is.na(Datos_piezas_NA$Fregado))) + length(which(!is.na(Datos_piezas_NA$Cepillado))) + length(which(!is.na(Datos_piezas_NA$Taladrado)))

#P
P <- Cantidad_equipos_Taladrado + Cantidad_equipos_Cepillado + Cantidad_equipos_Fregado + Cantidad_equipos_Corte
P

Kof <- (length(which(!is.na(Datos_piezas_NA$Corte))) + length(which(!is.na(Datos_piezas_NA$Fregado))) + length(which(!is.na(Datos_piezas_NA$Cepillado))) + length(which(!is.na(Datos_piezas_NA$Taladrado)))
) / (P)
Kof
if(Kof <= 1){
  "Masiva"
} else if(Kof>1 & Kof<=10){
  "Gran Serie"
}else if(Kof>10 & Kof<=20){
  
  "Mediana serie"
} else if(Kof>20 & Kof<=40){
  "Pequeña serie"
} else if(Kof > 40){
  "Unitaria"
} else {
  "El número es nulo"
}
print(Kof)



