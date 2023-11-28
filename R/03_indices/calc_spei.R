## Calc SPEI index 
## By: Cesar Saavedra
## Junio, 2023

# R options
g <- gc(reset = T); rm(list = ls()) # Empty garbage collector
options(warn = -1, scipen = 999)    # Remove warning alerts and scientific notation
suppressMessages(library(pacman))
suppressMessages(pacman::p_load(tidyverse,terra,gtools,lubridate,fs))
#
root <<- '//CATALOGUE/BanRep_papa' 
rgn <- 'Tolima' #'Antioquia', 'Boyaca', 'Cauca', 'Cundinamarca', 'N_Santander', 'Narino', 'Santander', 'Tolima'

# Calculate SPEI function
calc_SPEI <- function(prc_fls, evt_fls, scl){
  outfile <- paste0(out_dir,'SPEI-',scl,'-',yr_ini,'_',yr_end,'.tif')
  cat(outfile, "\n")
  #
  cnty <- terra::vect(paste0(root,'/1.Data/Shapefile/',rgn,'/Municipios_Paperos/','Municipios_paperos_',rgn,'.shp'))
  # Monthly files
  prc_fls <- fs::dir_ls(prc_pth, regexp = '.tif$') # Precipitation
  prc_fls <- prc_fls[file.exists(prc_fls)]
  evt_fls <- fs::dir_ls(evt_pth, regexp = '.tif$') # Evapotranspiracion
  evt_fls <- evt_fls[file.exists(evt_fls)]
  #
  prec <- terra::rast(prc_fls) %>% terra::crop(terra::ext(cnty)) %>% terra::mask(cnty)
  evt <- terra::rast(evt_fls)
  # Water balance
  W_B <- (prec - evt)
  # Calculate SPEI 
  SPEI <- terra::app(x= W_B,
                     fun = function(x){
                       x <- (data = x, start = c(yr_ini, 1), end = c(yr_end, 12), frequency = 12);
                       sp = SPEI::spei(data = x, scale = scl, na.rm = T)$fitted;
                       return(sp)
                     })
  names(SPEI) <- names(W_B)
  terra::writeRaster(SPEI, outfile, overwrite=T)
  cat("Done:", "\n", ":)", "\n")
}

# Setup
scl <- c(5) # Scale: 5 month
yr_ini <- 1981
yr_end <- 2022
#
prc_pth <- paste0(root,"/7.Results/historical/prec_acum/") # Monthly precipitation 
evt_pth <- paste0(root,"/7.Results/agro_indices/",rgn,"/ETP_monthly/") # Monthly Fao-Penma-Montieth 
#
out_dir <- paste0(root,'/7.Results/agro_indices/',rgn,'/SPEI/')
dir.create(out_dir, FALSE, TRUE)

# Execute the function in loop through the Scale(scl) parameter 
# calc_SPEI(prc_fls = prc_pth, evt_fls = evt_pth, scl = scl)
1:length(scl) %>% purrr::map(.f = function(i){calc_SPEI(scl = scl[i])})


