#   ------------------------------------------
# Daily precipitation data for ROI
# Cesar A Saavedra
# Julio 2023

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, sf, fs, tidyverse, gtools, rgeos, stringr, glue)
g <- gc(reset = T); rm(list = ls()); options(scipen = 999)

# Load data ---------------------------------------------------------------
path <- '//CATALOGUE/WFP_ClimateRiskPr1/1.Data' 
cntr <- terra::vect(paste0(path,'/shps/COL/COL_GADM1.shp')) # shape file region de interes

# Chirps ----------------------------------------------------
fls <- dir_ls(paste0(path,'/Chirps'), regexp = '.tif') %>% as.character()
dtes <- basename(fls) %>% str_split(., pattern = '-') %>% map(., 2) %>% unlist()
mnth <- dtes %>% str_sub(., 6, 16) %>% unique()

purrr::map(.x = 1:length(mnth), .f = function(i){
  
  cat(mnth[i], '\t')
  dte <- mnth[[i]]
  prec <- grep(dte, fls, value = T) %>% terra::rast() %>% terra::crop(., cntr) %>% terra::mask(., cntr)
  prec[prec == -9999] <- NA
  out <- paste0("//CATALOGUE/BanRep_papa/1.Data/Chirps/")
  dir.create(out, FALSE, TRUE)
  yea <- str_sub(dte, 1, 4); mnt <- str_sub(dte, 6, 7); day <- str_sub(dte, 9, 10)
  # names(prec) = mnth[[i]]
  terra::writeRaster(x = prec, filename = glue('{out}/chirps-{yea}_{mnt}_{day}.tif'), overwrite = TRUE)
  cat('Done!\n')
  
})

#   ------------------------------------------
