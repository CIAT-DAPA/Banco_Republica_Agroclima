# https://data.chc.ucsb.edu/products/CHIRPS-2.0/global_daily/tifs/p05
#------------------------------------------------------------
# Descarga datos de precipitación CHIRPS
# By: H. Achicanoy
# BIOVERSITY and CIAT
# 2022
#------------------------------------------------------------
# Instalacion y carga de librerias
install.packages('raster')
install.packages('geodata')
install.packages('terra')
install.packages('imager')
install.packages('lubridate')

library(raster)
library(geodata)
library(terra)
library(imager)
library(lubridate)
#------------------------------------------------------------
# Rango de fechas que se quieren descargar
ini <- as.Date('1981-01-01')
end <- as.Date('2022-12-31')
dts <- seq(from = ini, to = end, by = 'day'); rm(ini, end)
#------------------------------------------------------------
# Directorio donde se van a guardar los archivos descargados
Out  <- 'D:/OneDrive - CGIAR/Documents/Taller/Chirps'; if(!dir.exists(Out)){dir.create(Out,F,T)}
#------------------------------------------------------------
# Función de descarga
chirps2gray <- function(date = dts[1]){
  # Pagina web de CHIRPS
  chrps <- 'https://data.chc.ucsb.edu/products/CHIRPS-2.0/global_daily/tifs/p05'
  # Dia y Año
  Day  <- date
  Year <- lubridate::year(Day)
  # Archivo objetivo
  tfile <- paste0(chrps,'/',Year,'/chirps-v2.0.',gsub('-','.',Day,fixed=T),'.tif.gz')
  # Destino del archivo
  dfile <- paste0(Out,'/',basename(tfile))
  # Archivo raster 
  rfile <- gsub('.gz','',dfile,fixed = T)
  
  if(!file.exists(rfile)){
    tryCatch(expr = {
      utils::download.file(url = tfile, destfile = dfile)
    },
    error = function(e){
      cat(paste0(basename(tfile),' failed.\n'))
    })
    
  # Se descomprime el archivo
    R.utils::gunzip(dfile)
  }
}
#------------------------------------------------------------
# loop para recorrer todas las fechas y realizar las descargas
dts %>% purrr::map(.f = chirps2gray)
#------------------------------------------------------------