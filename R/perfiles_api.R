options(encoding = "UTF-8")

#setwd("casos-estad-sticos-1")

library(readr)
library(scales)
library(dplyr)
library(caret)

#library(base)

load("modelo2.RData")
load("modelo3.RData")
load("modelo4.RData")



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
                      Codigo_cliente,
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
          Codigo_cliente,
          Genero,
          Mon_sal_nominal, 
          Mon_sal_liquido, 
          Num_dependientes, 
          Nivel_academico, 
          flag_vehiculos, 
          Cant_propiedades_consolidado,
          Num_edad_anos
        )
        
# script original
        
        baseAD2<- data.frame(Codigo_cliente=perfil$Codigo_cliente[1],
                              Genero= perfil$Genero[1],
                              Mon_sal_nominal=  perfil$Mon_sal_nominal[1],
                              Mon_sal_liquido=  perfil$Mon_sal_liquido[1],
                              Num_dependientes=  perfil$Num_dependientes[1],
                              Nivel_academico=  perfil$Nivel_academico[1],
                              flag_vehiculos=  perfil$flag_vehiculos[1],
                              Cant_propiedades_consolidado=  perfil$Cant_propiedades_consolidado[1],
                              Num_edad_anos=  perfil$Num_edad_anos[1])
        
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
  
  colnames(baseAD2)=c('cod_cliente','genero','sal_bruto_cat','sal_liquido','num_dependientes_cat',
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
  
  baseAD2$PUNT_MOD21=predict(mod2,baseAD2)
  baseAD2$PUNT_MOD31=predict(mod3,baseAD2)
  baseAD2$PUNT_MOD41=predict(mod4,baseAD2)


  baseAD2$puntaje3= baseAD2$PUNT_MOD21+baseAD2$PUNT_MOD31+baseAD2$PUNT_MOD41
  baseAD2$puntaje3=rescale(baseAD2$puntaje3,to=c(0,5))



  ##prueba puntos
  baseAD2$perfil3=ifelse(baseAD2$puntaje3>=3.2,'5',
                  ifelse(baseAD2$puntaje3>=2.4 & baseAD2$puntaje3<3.2,'4',
                  ifelse(baseAD2$puntaje3>=1.65 & baseAD2$puntaje3<2.4,'3',
                  ifelse(baseAD2$puntaje3<1.65 & baseAD2$puntaje3>=1.,'2','1'))))

  perfil_final <- data.frame(cod_cliente=baseAD2$cod_cliente,perfil=baseAD2$perfil3)

  perfil_final <- data.frame(cod_cliente=perfil$Codigo_cliente,perfil="4")
  
  return(perfil_final) 
  },  error = function(err) {
    mi_error <- paste("ERROR:", err)
    return(mi_error)
  })
}
}
  

