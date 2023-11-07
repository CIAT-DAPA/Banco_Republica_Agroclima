# Precipitation indices 
# By: Cesar A
# CIAT
# 2023
# =-----------------------------------------------=
# R options
g <- gc(reset = T); rm(list = ls())    # Empty garbage collector
# .rs.restartR()                       # Restart R session
options(warn = -1, scipen = 999)       # Remove warning alerts and scientific notation
suppressMessages(library(pacman))
suppressMessages(pacman::p_load(tidyverse, terra, gtools, sf, furrr, future))
# =-----------------------------------------------=
OSys <<- Sys.info()[1]
root <<- '//CATALOGUE/BanRep_papa' 

# inputs and outputs
rgn = 'Antioquia' #'Antioquia', 'Boyaca', 'Cauca', 'Cundinamarca', 'N_Santander', 'Narino', 'Santander', 'Tolima'
seasons <- list(s1 = c(3:7),s2 = c(9:12,1)) # s2 = c(10:12,1:2)
shp_fl <- paste0(root, '/1.Data/Shapefile/',rgn,'/Municipios_Paperos/','Municipios_Paperos_',rgn,'.shp')
outfile <- paste0(root, '/7.Results/agro_indices/',rgn, '/'); if(!dir.exists(outfile)){dir.create(outfile, F, T)}
# =-----------------------------------------------=
# source('https://raw.githubusercontent.com/CIAT-DAPA/agro-clim-indices/main/_main_functions.R')
# source('//CATALOGUE/BanRep_papa/4.Scripts/_main_functions.R')
# =-----------------------------------------------=
# Function to compute basic Agro-climatic indices
calc_AgrClm <- function(season = season, shp_fl = shp_fl){
  
  ## ROI: regions of interest
  shp <- terra::vect(shp_fl)
  
  # ## Daily files
  # Precipitation
  chr_pth <- paste0(root, '/1.Data/Chirps')
  chr_fls <- gtools::mixedsort(list.files(chr_pth, pattern = '*.tif$', full.names = T))
  chr_dts <- strsplit(x = chr_fls, split = 'chirps-', fixed = T) %>% purrr::map(2) %>% unlist()
  chr_dts <- strsplit(x = chr_dts, split = '.tif', fixed = T) %>% purrr::map(1) %>% unlist()
  chr_dts <- as.Date(gsub('_', '-', chr_dts, fixed = T))
  
  # ETP
  evt_pth <- paste0(root,"/7.Results/agro_indices/",rgn,"/ETP_daily/")
  evt_fls <- gtools::mixedsort((list.files(evt_pth, pattern = '*.tif$', full.names = T)),decreasing = T)
  evt_dts <- strsplit(x = evt_fls, split = 'ET-', fixed = T) %>% purrr::map(2) %>% unlist()
  evt_dts <- strsplit(x = evt_dts, split = '.tif', fixed = T) %>% purrr::map(1) %>% unlist()
  evt_dts <- as.Date(gsub('_', '-', evt_dts, fixed = T))
  
  
  # Filtering days within the season
  yrs <- lubridate::year(evt_dts)
  yrs <- names(table(yrs)[table(yrs) %in% 365:366])
  #
  evt_fls <- evt_fls[lubridate::year(evt_dts) %in% yrs]
  #
  evt_dts <- evt_dts[lubridate::year(evt_dts) %in% yrs]
  #
  if(length(season) < 12){
    cnd <- lubridate::month(evt_dts) %in% season # Days within the season
    yrs_dts <<- split(evt_dts[cnd],cumsum(c(1,diff(evt_dts[cnd])!=1)))
  } else {
    yrs <- lubridate::year(evt_dts)
    grp <- with(rle(yrs), rep(seq_along(values), lengths))
    yrs_dts <<- split(evt_dts, grp)
  }
  
  # Precipitation indices 
  cat('..... Computing: IWR monthly. \n')
  IWR <- 1:length(yrs_dts) %>%
    purrr::map(.f = function(i){
      prc <- terra::rast(chr_fls[chr_dts %in% yrs_dts[[i]]])
      prc <- prc %>% terra::crop(terra::ext(shp)) %>% terra::mask(shp)
      prc[prc == -9999] <- NA
      
      prc_efc <<- terra::ifel(prc < 8.3, (prc*(4.17-0.2*prc))/(4.17), 
                              ifel(prc>=8.3, 4.17+(0.1*prc), prc))
      
      evt <- terra::rast(evt_fls[evt_dts %in% yrs_dts[[i]]])
      kc <- readxl::read_excel(paste0(root,"/1.Data/Kc.xlsx"))
      IWR <- (evt * kc$Kc) - prc_efc
      IWR_T <- sum(IWR)  
      # TR <- terra::app(x = prc, fun = function(x){ y = sum(x, na.rm = T); return(y) })
      # names(IWR_T) <- lubridate::year(yrs_dts[[i]]) %>% unique() %>% paste0(collapse = '-')
      return(IWR_T)
    }) %>% terra::rast()
  IWR <- IWR %>% terra::mask(shp)
  #
  cat('..... Computing: IWR daily. \n')
  IWR_d <- 1:length(yrs_dts) %>%
    purrr::map(.f = function(i){
      prc <- terra::rast(chr_fls[chr_dts %in% yrs_dts[[i]]])
      prc <- prc %>% terra::crop(terra::ext(shp)) %>% terra::mask(shp)
      prc[prc == -9999] <- NA
      
      prc_efc <<- terra::ifel(prc < 8.3, (prc*(4.17-0.2*prc))/(4.17), 
                              ifel(prc>=8.3, 4.17+(0.1*prc), prc))
      
      evt <- terra::rast(evt_fls[evt_dts %in% yrs_dts[[i]]])
      kc <- readxl::read_excel(paste0(root,"/1.Data/Kc.xlsx"))
      IWR_d <- (evt * kc$Kc) - prc_efc
      # TR <- terra::app(x = prc, fun = function(x){ y = sum(x, na.rm = T); return(y) })
      # names(IWR_d) <- lubridate::year(yrs_dts[[i]]) #%>% unique() %>% paste0(collapse = '-')
      return(IWR_d)
    }) %>% terra::rast()
  IWR_d <- IWR_d %>% terra::mask(shp)
  # 
  cat('..... Computing: WRa. daily Water requirement. \n')
  WR_d <- 1:length(yrs_dts) %>%
    purrr::map(.f = function(i){
      evt <- terra::rast(evt_fls[evt_dts %in% yrs_dts[[i]]])
      # prc <- evt %>% terra::crop(terra::ext(shp)) %>% terra::mask(shp)
      kc <- readxl::read_excel(paste0(root,"/1.Data/Kc.xlsx"))
      # cat(paste0(names(terra::nlyr(evt))))
      # if(nlyr(evt) == length(kc$Kc)){print('TRUE')}
      WR_d <- (evt * kc$Kc)
      # WR <- terra::app(x = prc, fun = function(x){ y = sum(x, na.rm = T); return(y) })
      # names(WR_d) <- lubridate::year(yrs_dts[[i]]) #%>% unique() %>% paste0(collapse = '-')
      return(WR_d)
    }) %>% terra::rast()
  WR_d <- WR_d %>% terra::mask(shp)
  # 
  cat('..... Computing: WRa. monthly water requirement. \n')
  WR <- 1:length(yrs_dts) %>%
    purrr::map(.f = function(i){
      evt <- terra::rast(evt_fls[evt_dts %in% yrs_dts[[i]]])
      # prc <- evt %>% terra::crop(terra::ext(shp)) %>% terra::mask(shp)
      kc <- readxl::read_excel(paste0(root,"/1.Data/Kc.xlsx"))
      # if(nlyr(evt) == length(kc$Kc)){print('TRUE')}
      WR <- (evt * kc$Kc) %>% sum()
      # WR <- terra::app(x = prc, fun = function(x){ y = sum(x, na.rm = T); return(y) })
      # names(WR) <- lubridate::year(yrs_dts[[i]]) %>% unique() %>% paste0(collapse = '-')
      return(WR)
    }) %>% terra::rast()
  WR <- WR %>% terra::mask(shp)
  #
  cat('..... End.\n')
  return(list(WR            = WR,
              WR_d          = WR_d,
              IWR           = IWR,
              IWR_d         = IWR_d))
  
}
# =-----------------------------------------------=
# Loop through seasons
1:length(seasons) %>%
  purrr::map(.f = function(s){
    cat(paste0('Processing season ',names(seasons)[s],':\n'))
    # Indices calculation
    indices <- calc_AgrClm(seasons[[s]], shp_fl)
    # Load 5 km raster template
    tmp <- terra::rast(paste0(root, '/1.Data/chirps-v2.0.2020.01.01.tif'))
    shp <- terra::vect(shp_fl)
    tmp <- tmp %>% terra::crop(terra::ext(shp)) %>% terra::mask(shp)
    tmp[!is.na(tmp)] <- 1
    # Indices resampling
    indices <- indices %>% purrr::map(.f = function(r){r <- r %>% terra::resample(x = ., y = tmp) %>% terra::mask(shp); return(r)})
    # Saving results
    out <- paste0(outfile, names(seasons)[s]); if(!dir.exists(out)){dir.create(out,F,T)}
    1:length(names(indices)) %>%
      purrr::map(.f = function(j){
        terra::writeRaster(x = indices[[j]], filename = paste0(out,'/',names(indices)[j],'.tif'), overwrite = T)
      })
    return(cat('Process finished successfully!\n'))
  })
# =-----------------------------------------------=
