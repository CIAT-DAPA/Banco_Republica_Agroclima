#   ------------------------------------------
# Daily climate data for roi
# Cesar A Saavedra
# Julio 2023

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, sf, fs, tidyverse, gtools, rgeos, stringr, glue)
g <- gc(reset = T); rm(list = ls()); options(scipen = 999)

# Load data ---------------------------------------------------------------
path <- '//CATALOGUE/WFP_ClimateRiskPr1/1.Data' 
var <- '2m_temperature-24_hour_maximum' # Variable de interes
dirs <- paste0(path, '/ERA5/', var)
cntr <- terra::vect(paste0(path,'/shps/COL/COL_GADM1.shp')) # shape file region de interes
ref <- terra::rast("//CATALOGUE/WFP_ClimateRiskPr1/1.Data/chirps-v2.0.1981.01.1.tif")

# AgERA5 -----------------------------------------------------------
fls <- dir_ls(dirs, regexp = '.nc$') %>% as.character()
dtes <- basename(fls) %>% str_split(., pattern = '_') %>% map(., 4) %>% unlist() #%>% str_sub(., 6, nchar(.) - 4)
mnth <- dtes %>% str_sub(., 1, 8) %>% unique()

purrr::map(.x = 1:length(mnth), .f = function(i){
  cat(mnth[i], '\n')
  dte <- mnth[[i]]
  ppt <- grep(dte, fls, value = T) %>% terra::rast()
  ppt <- terra::resample(ppt, ref) %>% terra::crop(.,cntr) %>% terra::mask(., cntr)
  out <- paste0("//CATALOGUE/BanRep_papa/1.Data/AgERA5/",var)
  dir.create(out, FALSE, TRUE)
  yea <- str_sub(dte, 1, 4); mnt <- str_sub(dte, 5, 6); day <- str_sub(dte, 7, 8)
  # names(ppt) = mnth[[i]]
  terra::writeRaster(x = ppt, filename = glue('{out}/{var}_{yea}_{mnt}_{day}.tif'), overwrite = TRUE)
  cat('Done!\n')
})

#   ------------------------------------------
