options(encoding = "UTF-8")

#setwd("casos-estad-sticos-1")

library(scales)
library(dplyr)


 # load("data/modelo2.RData")
 # load("data/modelo3.RData")
 # load("data/modelo4.RData")


#* @apiTitle Servicio Perfiles
#* @apiDescription Genera una clasificación basada en el perfil del 
#* cliente
#* @apiVersion 0.0.6

#* Muestra version y ultima fecha de cambio del servicio de perfil
#* @get /
api_info <- function() {
  info <- data.frame(version = "0.0.6",
                     fecha_ultimo_cambio = "Noviembre 3, 2020")
  # Devuelve valor ----
  return(info)
}

#* Calcula el perfil del cliente
#* @post /perfil
perfil <- function(req,
                   Identificacion,
                      Genero,
                      Mon_sal_nominal, 
                      Mon_sal_liquido, 
                      Num_dependientes, 
                      Nivel_academico, 
                      flag_vehiculos, 
                      Cant_propiedades_consolidado,
                      Num_edad_anos) {
  
  if (!("HTTP_API_KEY" %in% names(req))) {
    return("ERROR: Llamado no autorizado")
  } else 
    if (req$HTTP_API_KEY != "ce994743-2715-45de-aabc-a0572b98d80a") {
      return("Llamado no autorizado")
    } else {
      
      tryCatch({
        
        perfil <- data.frame(
          Identificacion,
          Genero,
          Mon_sal_nominal, 
          Mon_sal_liquido, 
          Num_dependientes, 
          Nivel_academico, 
          flag_vehiculos, 
          Cant_propiedades_consolidado,
          Num_edad_anos
        )
# Errores
        `%noin%` <- Negate(`%in%`)
        
        if (nchar((perfil$Identificacion)) >8 ){
          Identificacion= "Valor incorrecto"
        }      
        
        if (perfil$nivel_academico %noin% c("BACHILLER EDUCACION",   "DIPLOMADO",  "OTRO",  "PRIMARIA COMPLETA", "PRIMARIA INCOMPLETA", "SECUNDARIA COMPLETA", "SECUNDARIA INCOMPLETA", "TECNICO", "TECNICO MEDIO", "UNIVERSIDAD COMPLETA", "UNIVERSIDAD INCOMPLETA")) {
          warning("Verificar con el proveedor para incluir el nivel academico")
      }

if (perfil$Genero  %noin% c("Femenino", "Masculino")) {
  warning("Verificar el Genero")
    }

  if (is.na(as.numeric(perfil$num_dependientes))) {
    warning("Verificar Numero de dependientes")
}

if (is.null(as.numeric(perfil$Num_dependientes_cat))) {
  warning("Verificar Numero de dependientes")
}

if (perfil$flag_vehiculos %noin% c("Tiene","No tiene")) {
  warning("Verificar el flag vehículos")
}


if (is.na(as.numeric(perfil$Mon_sal_nominal))) {
  warning("Verificar Monto de salario nominal es NA")
}

if (is.null(as.numeric(perfil$Mon_sal_nominal))) {
  warning("Verificar Monto de salario nominal es NULL")
}

if (is.na(as.numeric(perfil$Mon_sal_liquido))) {
  warning("Verificar Monto de salario liquido es NA")
}

if (is.null(as.numeric(perfil$Mon_sal_liquido))) {
  warning("Verificar Monto de salario liquido es NULL")
}

if (is.na(as.numeric(perfil$Cant_propiedades_consolidado))) {
  warning("Verificar Cantidad de propiedades es NA")
}

if (is.null(as.numeric(perfil$Cant_propiedades_consolidado))) {
  warning("Verificar Cantidad de propiedades es NULL")
}

if (is.na(as.numeric(perfil$Num_edad_anos))) {
  warning("Verificar Cantidad de propiedades es NA")
}

if (is.null(as.numeric(perfil$Num_edad_anos))) {
  warning("Verificar Cantidad de propiedades es NULL")
}
  

# script original
        
        baseAD2<- data.frame(Identificacion=baseAD2$Identificacion[1],
                              Genero= baseAD2$Genero[1],
                              Mon_sal_nominal=  baseAD2$Mon_sal_nominal[1],
                              Mon_sal_liquido=  baseAD2$Mon_sal_liquido[1],
                              Num_dependientes=  baseAD2$Num_dependientes[1],
                              Nivel_academico=  baseAD2$Nivel_academico[1],
                              flag_vehiculos=  baseAD2$flag_vehiculos[1],
                              Cant_propiedades_consolidado=  baseAD2$Cant_propiedades_consolidado[1],
                              Num_edad_anos=  baseAD2$Num_edad_anos[1])
        
##Recode
        
  baseAD2$Mon_sal_nominal=ifelse(baseAD2$Mon_sal_nominal<=350000,'menor350',
                          ifelse(baseAD2$Mon_sal_nominal>350000 &baseAD2$Mon_sal_nominal<=420000,'de350a420',
                          ifelse(baseAD2$Mon_sal_nominal>420000 & baseAD2$Mon_sal_nominal<=510000,'de420a510', 
                          ifelse(baseAD2$Mon_sal_nominal>510000 & baseAD2$Mon_sal_nominal<=610000,'de510a610',
                          ifelse(baseAD2$Mon_sal_nominal>610000 & baseAD2$Mon_sal_nominal<=700000,'de610a700',
                          ifelse(baseAD2$Mon_sal_nominal>700000 &baseAD2$Mon_sal_nominal<=815000,'de700a815',
                          ifelse(baseAD2$Mon_sal_nominal>815000 & baseAD2$Mon_sal_nominal<=965000,'de815a965',
                          ifelse(baseAD2$Mon_sal_nominal>965000 & baseAD2$Mon_sal_nominal<=1150000,'de965a1150'
                          ,'mayor1150'))))))))
        
        
        
  baseAD2$Cant_propiedades_consolidado=ifelse(baseAD2$Cant_propiedades_consolidado==0,'prop0',
                                       ifelse(baseAD2$Cant_propiedades_consolidado==1,'prop1','propmas2'))
  
  colnames(baseAD2)=c('Identificacion','genero','sal_bruto_cat','sal_liquido','num_dependientes_cat',
                            'nivel_academico','flag_vehiculos','Cant_propiedades_consolidado',
                            'edad')
        
        
  baseAD2$num_dependientes_cat=ifelse(is.na(baseAD2$num_dependientes_cat)==TRUE,0,baseAD2$num_dependientes_cat)
  baseAD2$num_dependientes_cat=as.numeric(baseAD2$num_dependientes_cat)
  baseAD2$num_dependientes_cat=ifelse(baseAD2$num_dependientes_cat>=6,'mas_6',baseAD2$num_dependientes_cat)
        
        
  baseAD2$num_dependientes_cat=as.character(baseAD2$num_dependientes_cat)
        
  baseAD2$sal_liquido=ifelse(baseAD2$sal_liquido<=350000,'Menor 350',
                      ifelse(baseAD2$sal_liquido>350000 & baseAD2$sal_liquido<=600000,'De 350 a 600',
                      ifelse(baseAD2$sal_liquido>600000 & baseAD2$sal_liquido<=1000000,'De 601 a 1 millon',
                      ifelse(baseAD2$sal_liquido>1000000 &baseAD2$sal_liquido<=1500000,'De 1 millon a millon y medio', 'Mayor de millon y medio'))))
        
        
        
  baseAD2$nivel_academico= ifelse(baseAD2$nivel_academico=='PRIMARIA COMPLETA'|baseAD2$nivel_academico=='PRIMARIA INCOMPLETA','PRIMARIA',
                           ifelse(baseAD2$nivel_academico=='TECNICO'| baseAD2$nivel_academico=='TECNICO MEDIO'| baseAD2$nivel_academico=='DIPLOMADO','TECNICO', baseAD2$nivel_academico))
        
        
  baseAD2$genero=ifelse(baseAD2$genero=='Sin Definir','Masculino','Femenino')
        
  baseAD2$edad=ifelse(baseAD2$edad<=25,'Menor 25',
                            ifelse(baseAD2$edad>25 &baseAD2$edad<=35,'De 26 a 35',
                            ifelse(baseAD2$edad>35 & baseAD2$edad<=50,'De 36 a 50',
                                          'Mayor de 51') ))
        
  baseAD2$flag_vehiculos=ifelse(is.na(baseAD2$flag_vehiculos)==FALSE,baseAD2$flag_vehiculos,'No tiene')
  baseAD2$Cant_propiedades_consolidado=ifelse(is.na(baseAD2$Cant_propiedades_consolidado)==FALSE, baseAD2$Cant_propiedades_consolidado,'prop0')
  
## Modelos
  
  baseAD2$PUNT_MOD21<-  as.numeric(0.043438 +
  -0.011060* baseAD2 %>% select(flag_vehiculos) %>% mutate(flag_vehiculos=ifelse(flag_vehiculos=="tiene",1,0)) +
  -0.004613* baseAD2 %>% select(sal_bruto_cat) %>% mutate(sal_bruto_cat=ifelse(sal_bruto_cat=="de350a420",1,0)) +
  -0.001645*baseAD2 %>% select(sal_bruto_cat) %>% mutate(sal_bruto_cat=ifelse(sal_bruto_cat=="de420a510",1,0))+
  -0.011581*baseAD2 %>% select(sal_bruto_cat) %>% mutate(sal_bruto_cat=ifelse(sal_bruto_cat=="de510a610",1,0))+
  -0.013871*baseAD2 %>% select(sal_bruto_cat) %>% mutate(sal_bruto_cat=ifelse(sal_bruto_cat=="de610a700",1,0))+
  -0.019472*baseAD2 %>% select(sal_bruto_cat) %>% mutate(sal_bruto_cat=ifelse(sal_bruto_cat=="de700a815",1,0))+
  -0.021973*baseAD2 %>% select(sal_bruto_cat) %>% mutate(sal_bruto_cat=ifelse(sal_bruto_cat=="de815a965",1,0))+
  -0.029897*baseAD2 %>% select(sal_bruto_cat) %>% mutate(sal_bruto_cat=ifelse(sal_bruto_cat=="de965a1150",1,0))+
  -0.030188*baseAD2 %>% select(sal_bruto_cat) %>% mutate(sal_bruto_cat=ifelse(sal_bruto_cat=="sal_bruto_catmayor1150",1,0))  )
  
  
  
  baseAD2$PUNT_MOD31= as.numeric(0.017356 +
    0.002492* baseAD2 %>% select(nivel_academico) %>% mutate(nivel_academico=ifelse(nivel_academico=="SECUNDARIA COMPLETA",1,0)) +
    0.007945* baseAD2 %>% select(nivel_academico) %>% mutate(nivel_academico=ifelse(nivel_academico=="SECUNDARIA INCOMPLETA",1,0)) +
    0.005169*baseAD2 %>% select(nivel_academico) %>% mutate(nivel_academico=ifelse(nivel_academico=="TECNICO",1,0))+
    -0.018026*baseAD2 %>% select(nivel_academico) %>% mutate(nivel_academico=ifelse(nivel_academico=="UNIVERSIDAD COMPLETA",1,0))+
    -0.001014*baseAD2 %>% select(nivel_academico) %>% mutate(nivel_academico=ifelse(nivel_academico=="UNIVERSIDAD INCOMPLETA",1,0))+
    0.006956*baseAD2 %>% select(sal_liquido) %>% mutate(sal_liquido=ifelse(sal_liquido=="De 350 a 600",1,0))+
    0.004043*baseAD2 %>% select(sal_liquido) %>% mutate(sal_liquido=ifelse(sal_liquido=="De 601 a 1 millon",1,0))+
    0.015925*baseAD2 %>% select(sal_liquido) %>% mutate(sal_liquido=ifelse(sal_liquido=="Mayor de millon y medio",1,0))+
    0.009029*baseAD2 %>% select(sal_liquido) %>% mutate(sal_liquido=ifelse(sal_liquido=="Menor 350",1,0)))
  

  
    baseAD2$PUNT_MOD41= as.numeric(0.020031 +
      0.007169 * baseAD2 %>% select(genero) %>% mutate(genero=ifelse(genero=="Masculino",1,0)) +
      -0.002765 * baseAD2 %>% select(edad) %>% mutate(edad=ifelse(edad=="De 36 a 50",1,0)) +
      -0.006284 * baseAD2 %>% select(edad) %>% mutate(edad=ifelse(edad=="Mayor de 51",1,0))+
      -0.004781 * baseAD2 %>% select(edad) %>% mutate(edad=ifelse(edad=="Menor 25",1,0))+
      -0.011113 * baseAD2 %>% select(Cant_propiedades_consolidado) %>% mutate(Cant_propiedades_consolidado=ifelse(Cant_propiedades_consolidado=="prop1",1,0)) +
      -0.016006 * baseAD2 %>% select(Cant_propiedades_consolidado) %>% mutate(Cant_propiedades_consolidado=ifelse(Cant_propiedades_consolidado=="propmas2",1,0)))
    
    
  baseAD2$puntaje3= baseAD2$PUNT_MOD21+baseAD2$PUNT_MOD31+baseAD2$PUNT_MOD41
  baseAD2$puntaje3=rescale(baseAD2$puntaje3,to=c(0,5))



  ##prueba puntos
  baseAD2$perfil3=ifelse(baseAD2$puntaje3>=3.2,'5',
                  ifelse(baseAD2$puntaje3>=2.4 & baseAD2$puntaje3<3.2,'4',
                  ifelse(baseAD2$puntaje3>=1.65 & baseAD2$puntaje3<2.4,'3',
                  ifelse(baseAD2$puntaje3<1.65 & baseAD2$puntaje3>=1.,'2','1'))))

  perfil_final <- data.frame(Identificacion=baseAD2$Identificacion,perfil=baseAD2$perfil3)

  #perfil_final <- data.frame(cod_cliente=perfil$Codigo_cliente,perfil="4",revisar=getwd())
  
  return(perfil_final) 
  },  error = function(err) {
    mi_error <- paste("ERROR:", err)
    return(mi_error)
  })
}
}
