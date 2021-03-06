Datos <- read.csv("Datos_piezas.csv", header = TRUE, sep = ",")
names(Datos)

attach(Datos)

Informaci�n <- read.csv("Informaci�n.csv")
attach(Informaci�n)
names(Informaci�n)
data.frame(Informaci�n)

library(dplyr)
ls("package:dplyr")

#CORTE
data.frame(Informaci�n$"JL", data.frame(Informaci�n$Corte))


R�gimen_trabajo_Corte <- 8
R�gimen_trabajo_Corte

Horas_mtto_Corte <- 135
Horas_mtto_Corte

Cantidad_equipos_Corte <- 3
Cantidad_equipos_Corte

Imprevistos_Corte <- 200
Imprevistos_Corte

D�as_a�o_Corte <- 365
D�as_a�o_Corte

Horas_d�a_Corte <- 24
Horas_d�a_Corte

Vacaciones_Corte <- 0*R�gimen_trabajo_Corte*Cantidad_equipos_Corte
Vacaciones_Corte

#FONDO PRODUCTIVO TOTAL 
FPT <- D�as_a�o_Corte*Horas_d�a_Corte*Cantidad_equipos_Corte 
FPT
#FONDO POR REQUERIMIENTOS TECNOLOGICOS  
FRT <- Horas_mtto_Corte*Cantidad_equipos_Corte
FRT
#FONDO POR REGIMEN LABORAL
FRL <- (R�gimen_trabajo_Corte*D�as_a�o_Corte*Cantidad_equipos_Corte)+(Vacaciones_Corte)
FRL
#FONDO POR OTRAS CAUSAS
FOC <- Imprevistos_Corte*Cantidad_equipos_Corte
FOC
#FONDO PRODUCTIVO DISPONIBLE
FPD <- FPT-FRT-FRL-FOC 
FPD #h-e/a�o

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
"Peque�a serie"
} else if(Ccj_Corte < 0.04){
"Unitaria"
} else {
"El n�mero es nulo"
}
print(Ccj_Corte)

#FREGADO
data.frame(Informaci�n)
data.frame(Informaci�n$"JL", data.frame(Informaci�n$Fregado))


R�gimen_trabajo_Fregado <- 8
R�gimen_trabajo_Fregado

Horas_mtto_Fregado <- 150
Horas_mtto_Fregado

Cantidad_equipos_Fregado <- 2
Cantidad_equipos_Fregado

Imprevistos_Fregado <- 200
Imprevistos_Fregado

D�as_a�o_Fregado <- 365
D�as_a�o_Fregado

Horas_d�a_Fregado <- 24
Horas_d�a_Fregado

Vacaciones_Fregado <- 0*R�gimen_trabajo_Fregado*Cantidad_equipos_Fregado
Vacaciones_Fregado

#FONDO PRODUCTIVO TOTAL 
FPT <- D�as_a�o_Fregado*Horas_d�a_Fregado*Cantidad_equipos_Fregado 
FPT
#FONDO POR REQUERIMIENTOS TECNOLOGICOS  
FRT <- Horas_mtto_Fregado*Cantidad_equipos_Fregado
FRT
#FONDO POR REGIMEN LABORAL
FRL <- (R�gimen_trabajo_Fregado*D�as_a�o_Fregado*Cantidad_equipos_Fregado)+(Vacaciones_Fregado)
FRL
#FONDO POR OTRAS CAUSAS
FOC <- Imprevistos_Fregado*Cantidad_equipos_Fregado
FOC
#FONDO PRODUCTIVO DISPONIBLE
FPD <- FPT-FRT-FRL-FOC 
FPD #h-e/a�o

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
  "Peque�a serie"
} else if(Ccj_Fregado < 0.04){
  "Unitaria"
} else {
  "El n�mero es nulo"
}
print(Ccj_Fregado)

#CEPILLADO
data.frame(Informaci�n)
data.frame(Informaci�n$"JL", data.frame(Informaci�n$Cepillado))


R�gimen_trabajo_Cepillado <- 8
R�gimen_trabajo_Cepillado

Horas_mtto_Cepillado <- 200
Horas_mtto_Cepillado

Cantidad_equipos_Cepillado <- 4
Cantidad_equipos_Cepillado

Imprevistos_Cepillado <- 200
Imprevistos_Cepillado

D�as_a�o_Cepillado <- 365
D�as_a�o_Cepillado

Horas_d�a_Cepillado <- 24
Horas_d�a_Cepillado

Vacaciones_Cepillado <- 0*R�gimen_trabajo_Cepillado*Cantidad_equipos_Cepillado
Vacaciones_Cepillado

#FONDO PRODUCTIVO TOTAL 
FPT <- D�as_a�o_Cepillado*Horas_d�a_Cepillado*Cantidad_equipos_Cepillado 
FPT
#FONDO POR REQUERIMIENTOS TECNOLOGICOS  
FRT <- Horas_mtto_Cepillado*Cantidad_equipos_Cepillado
FRT
#FONDO POR REGIMEN LABORAL
FRL <- (R�gimen_trabajo_Cepillado*D�as_a�o_Cepillado*Cantidad_equipos_Cepillado)+(Vacaciones_Cepillado)
FRL
#FONDO POR OTRAS CAUSAS
FOC <- Imprevistos_Cepillado*Cantidad_equipos_Cepillado
FOC
#FONDO PRODUCTIVO DISPONIBLE
FPD <- FPT-FRT-FRL-FOC 
FPD #h-e/a�o

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
  "Peque�a serie"
} else if(Ccj_Cepillado < 0.04){
  "Unitaria"
} else {
  "El n�mero es nulo"
}
print(Ccj_Cepillado)

#TALADRADO
data.frame(Informaci�n)
data.frame(Informaci�n$"JL", data.frame(Informaci�n$Taladrado))


R�gimen_trabajo_Taladrado <- 8
R�gimen_trabajo_Taladrado

Horas_mtto_Taladrado <- 300
Horas_mtto_Taladrado

Cantidad_equipos_Taladrado <- 3
Cantidad_equipos_Taladrado

Imprevistos_Taladrado <- 200
Imprevistos_Taladrado

D�as_a�o_Taladrado <- 365
D�as_a�o_Taladrado

Horas_d�a_Taladrado <- 24
Horas_d�a_Taladrado

Vacaciones_Taladrado <- 0*R�gimen_trabajo_Taladrado*Cantidad_equipos_Taladrado
Vacaciones_Taladrado

#FONDO PRODUCTIVO TOTAL 
FPT <- D�as_a�o_Taladrado*Horas_d�a_Taladrado*Cantidad_equipos_Taladrado 
FPT
#FONDO POR REQUERIMIENTOS TECNOLOGICOS  
FRT <- Horas_mtto_Taladrado*Cantidad_equipos_Taladrado
FRT
#FONDO POR REGIMEN LABORAL
FRL <- (R�gimen_trabajo_Taladrado*D�as_a�o_Taladrado*Cantidad_equipos_Taladrado)+(Vacaciones_Taladrado)
FRL
#FONDO POR OTRAS CAUSAS
FOC <- Imprevistos_Taladrado*Cantidad_equipos_Taladrado
FOC
#FONDO PRODUCTIVO DISPONIBLE
FPD <- FPT-FRT-FRL-FOC 
FPD #h-e/a�o

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
  "Peque�a serie"
} else if(Ccj_Taladrado < 0.04){
  "Unitaria"
} else {
  "El n�mero es nulo"
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
  "Peque�a serie"
} else if(Kof > 40){
  "Unitaria"
} else {
  "El n�mero es nulo"
}
print(Kof)



