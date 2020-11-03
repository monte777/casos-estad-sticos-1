options(encoding = "UTF-8")

library(readr)


load("../R/modelo2.Rdata")
load("../R/modelo3.Rdata")
load("../R/modelo4.Rdata")


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
                      edad,
                      genero,
                      sal_bruto,
                      sal_liquido,
                      cant_dependientes,
                      nivel_academico,
                      rel_cuota_ingreso,
                      flag_Vehiculos,
                      cant_propiedades) {
  
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
          Cant_propiedades_consolidado
        )
        
# script original
        
        library(dplyr)
        
        baseAD2<- baseAD2 %>% 
          select(Codigo_cliente,
                 Genero,
                 Mon_sal_nominal, 
                 Mon_sal_liquido, 
                 Num_dependientes, 
                 Nivel_academico, 
                 flag_vehiculos, 
                 Cant_propiedades_consolidado)
        
        
#

  prediccion <- 1
  return(prediccion) 
  },  error = function(err) {
    mi_error <- paste("ERROR:", err)
    return(mi_error)
  })
}
}
  

