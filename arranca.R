options(encoding = "UTF-8")

# arranque del servicio
## encoding

library(plumber)
library(jsonlite)
library(scales)
library(readr)

r <- plumb("R/perfiles_api.R")
# Arranca servicio
r$run(host= "0.0.0.0", port=8080, swagger = FALSE)