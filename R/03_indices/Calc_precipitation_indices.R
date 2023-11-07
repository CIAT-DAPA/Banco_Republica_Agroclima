# Precipitation indices 
# By: Cesar A
# CIAT
# 2023

# R options
g <- gc(reset = T); rm(list = ls())    # Empty garbage collector
# .rs.restartR()                       # Restart R session
options(warn = -1, scipen = 999)       # Remove warning alerts and scientific notation
suppressMessages(library(pacman))
suppressMessages(pacman::p_load(tidyverse, terra, gtools, sf, furrr, future))

OSys <<- Sys.info()[1]
root <<- '//CATALOGUE/BanRep_papa' 

# inputs and outputs
rgn = 'Tolima' #'Antioquia', 'Boyaca', 'Cauca', 'Cundinamarca', 'N_Santander', 'Narino', 'Santander', 'Tolima'
seasons <- list(s1 = c(3:8),s2 = c(8:12)) # s2 = c(10:12,1:2)
shp_fl <- paste0(root, '/1.Data/Shapefile/',rgn,'/Municipios_Paperos/','Municipios_Paperos_',rgn,'.shp')
outfile <- paste0(root, '/7.Results/agro_indices/',rgn, '/'); if(!dir.exists(outfile)){dir.create(outfile, F, T)}

# source('https://raw.githubusercontent.com/CIAT-DAPA/agro-clim-indices/main/_main_functions.R')
source('//CATALOGUE/BanRep_papa/4.Scripts/_main_functions.R')
calc_SHIMP <- function(TMAX, RH){SHI <- ifelse(TMAX >= 29 & RH > 50, 1, 0); return(SHI)}; calc_SHIMP <- compiler::cmpfun(calc_SHIMP)
calc_THIMP <- function(TMAX, RH){THI <- (1.8 * TMAX + 32) - ((0.55 - 0.0055 * RH) * (1.8 * TMAX - 26.8)); THI_n <- ifelse(test = THI >= 79 & THI < 89 | THI >= 89, yes = 1, no = 0); return(THI_n)}; calc_THIMP <- compiler::cmpfun(calc_THIMP)
calc_HSIMP <- function(TMAX, RH){
  # 2 ~ danger, 3 ~ emergency
  # HSI_n <- ifelse((TMAX <= 26 & RH > 70 |
  #                 TMAX <= 27 & RH >= 40 & RH < 85 |
  #                 TMAX <= 28 & RH < 85 |
  #                 TMAX <= 29 & RH < 60 |
  #                 TMAX <= 30 & RH < 40), 1, 0)
  HSI_n <- ifelse((TMAX <= 27 & RH >= 85 |
                     TMAX <= 28 & RH >= 85 |
                     TMAX <= 29 & RH >= 60 |
                     TMAX <= 30 & RH >= 40 |
                     TMAX > 30), 1, 0)
  return(HSI_n)
}; calc_HSIMP <- compiler::cmpfun(calc_HSIMP)

# Function to compute basic Agro-climatic indices
calc_AgrClm <- function(season = season, shp_fl = shp_fl){
  
  ## ROI: regions of interest
  shp <- terra::vect(shp_fl)
  
  ## Daily files
  # Precipitation
  chr_pth <- paste0(root, '/1.Data/Chirps')
  chr_fls <- gtools::mixedsort(list.files(chr_pth, pattern = '*.tif$', full.names = T))
  chr_dts <- strsplit(x = chr_fls, split = 'chirps-', fixed = T) %>% purrr::map(2) %>% unlist()
  chr_dts <- strsplit(x = chr_dts, split = '.tif', fixed = T) %>% purrr::map(1) %>% unlist()
  chr_dts <- as.Date(gsub('_', '-', chr_dts, fixed = T))
  
  # Tmax
  era5Dir <- paste0(root, '/1.Data/agERA5')
  tmx_pth <- paste0(era5Dir,'/2m_temperature-24_hour_maximum')
  tmx_fls <- gtools::mixedsort(list.files(tmx_pth, pattern = '_maximum_.*.tif$', full.names = T))
  tmx_dts <- strsplit(x = tmx_fls, split = '2m_temperature-24_hour_maximum_', fixed = T) %>% purrr::map(2) %>% unlist()
  tmx_dts <- strsplit(x = tmx_dts, split = '.tif', fixed = T) %>% purrr::map(1) %>% unlist()
  # tmx_dts <- as.Date(tmx_dts, "%Y%m%d")
  tmx_dts <- as.Date(gsub('_', '-', tmx_dts, fixed = T))
  
  # Tmin
  tmn_pth <- paste0(era5Dir,'/2m_temperature-24_hour_minimum')
  tmn_fls <- gtools::mixedsort(list.files(tmn_pth, pattern = '_minimum_.*.tif$', full.names = T))
  tmn_dts <- strsplit(x = tmn_fls, split = '2m_temperature-24_hour_minimum_', fixed = T) %>% purrr::map(2) %>% unlist()
  tmn_dts <- strsplit(x = tmn_dts, split = '.tif', fixed = T) %>% purrr::map(1) %>% unlist()
  # tmn_dts <- as.Date(tmn_dts, "%Y%m%d")
  tmn_dts <- as.Date(gsub('_', '-', tmn_dts, fixed = T))
  
  # Tmean
  # tav_pth <- paste0(era5Dir,'/2m_temperature-24_hour_mean')
  # tav_fls <- gtools::mixedsort(list.files(tav_pth, pattern = '*.nc$', full.names = T))
  # tav_dts <- strsplit(x = tav_fls, split = 'glob-agric_AgERA5_', fixed = T) %>% purrr::map(2) %>% unlist()
  # tav_dts <- strsplit(x = tav_dts, split = '_final-v1.0.nc', fixed = T) %>% purrr::map(1) %>% unlist()
  # tav_dts <- as.Date(tav_dts, "%Y%m%d")
  
  # Solar radiation
  srd_pth <- paste0(era5Dir,'/solar_radiation_flux')
  srd_fls <- gtools::mixedsort(list.files(srd_pth, pattern = '*.tif$', full.names = T))
  srd_dts <- strsplit(x = srd_fls, split = 'solar_radiation_flux_', fixed = T) %>% purrr::map(2) %>% unlist()
  srd_dts <- strsplit(x = srd_dts, split = '.tif', fixed = T) %>% purrr::map(1) %>% unlist()
  # srd_dts <- as.Date(srd_dts, "%Y%m%d")
  srd_dts <- as.Date(gsub('_', '-', srd_dts, fixed = T))
  
  # Relative humidity
  rhy_pth <- paste0(era5Dir,'/2m_relative_humidity')
  rhy_fls <- gtools::mixedsort(list.files(rhy_pth, pattern = '*.tif$', full.names = T))
  rhy_dts <- strsplit(x = rhy_fls, split = '2m_relative_humidity_', fixed = T) %>% purrr::map(2) %>% unlist()
  rhy_dts <- strsplit(x = rhy_dts, split = '.tif', fixed = T) %>% purrr::map(1) %>% unlist()
  # rhy_dts <- as.Date(rhy_dts, "%Y%m%d")
  rhy_dts <- as.Date(gsub('_', '-', rhy_dts, fixed = T))
  
  # Filtering days within the season
  yrs <- lubridate::year(tmx_dts)
  yrs <- names(table(yrs)[table(yrs) %in% 365:366])
  
  tmx_fls <- tmx_fls[lubridate::year(tmx_dts) %in% yrs]
  tmn_fls <- tmn_fls[lubridate::year(tmn_dts) %in% yrs]
  # tav_fls <- tav_fls[lubridate::year(tav_dts) %in% yrs]
  srd_fls <- srd_fls[lubridate::year(srd_dts) %in% yrs]
  rhy_fls <- rhy_fls[lubridate::year(rhy_dts) %in% yrs]
  
  tmx_dts <- tmx_dts[lubridate::year(tmx_dts) %in% yrs]
  tmn_dts <- tmn_dts[lubridate::year(tmn_dts) %in% yrs]
  # tav_dts <- tav_dts[lubridate::year(tav_dts) %in% yrs]
  srd_dts <- srd_dts[lubridate::year(srd_dts) %in% yrs]
  rhy_dts <- rhy_dts[lubridate::year(rhy_dts) %in% yrs]
  
  if(length(season) < 12){
    cnd <- lubridate::month(tmx_dts) %in% season # Days within the season
    yrs_dts <<- split(tmx_dts[cnd],cumsum(c(1,diff(tmx_dts[cnd])!=1)))
  } else {
    yrs <- lubridate::year(tmx_dts)
    grp <- with(rle(yrs), rep(seq_along(values), lengths))
    yrs_dts <<- split(tmx_dts, grp)
  }
  
  # Precipitation indices 
  cat('..... Computing: TR. Total rainfall. \n')
  TR <- 1:length(yrs_dts) %>%
    purrr::map(.f = function(i){
      prc <- terra::rast(chr_fls[chr_dts %in% yrs_dts[[i]]])
      prc <- prc %>% terra::crop(terra::ext(shp)) %>% terra::mask(shp)
      prc[prc == -9999] <- NA
      TR <- terra::app(x = prc, fun = function(x){ y = sum(x, na.rm = T); return(y) })
      names(TR) <- lubridate::year(yrs_dts[[i]]) %>% unique() %>% paste0(collapse = '-')
      return(TR)
    }) %>% terra::rast()
  TR <- TR %>% terra::mask(shp)
  # 
  cat('..... Computing: NDD. Number of dry days. \n')
  NDD <- 1:length(yrs_dts) %>%
    purrr::map(.f = function(i){
      prc <- terra::rast(chr_fls[chr_dts %in% yrs_dts[[i]]])
      prc <- prc %>% terra::crop(terra::ext(shp)) %>% terra::mask(shp)
      prc[prc == -9999] <- 0
      NDD <- terra::app(x = prc, fun = function(x){ y = calc_nddCMP(PREC = x, p_thresh = 1); return(y) })
      names(NDD) <- lubridate::year(yrs_dts[[i]]) %>% unique() %>% paste0(collapse = '-')
      return(NDD)
    }) %>% terra::rast()
  NDD <- NDD %>% terra::mask(shp) 
  # 
  cat('..... Computing: NRD. Number of rainy days. \n')
  NRD <- 1:length(yrs_dts) %>%
    purrr::map(.f = function(i){
      prc <- terra::rast(chr_fls[chr_dts %in% yrs_dts[[i]]])
      prc <- prc %>% terra::crop(terra::ext(shp)) %>% terra::mask(shp)
      prc[prc == -9999] <- 0
      NRD <- terra::app(x = prc, fun = function(x){ y = calc_nrdCMP(PREC = x, p_thresh = 1); return(y) })
      names(NRD) <- lubridate::year(yrs_dts[[i]]) %>% unique() %>% paste0(collapse = '-')
      return(NRD)
    }) %>% terra::rast()
  NRD <- NRD %>% terra::mask(shp)
  #
  # cat('..... Computing: Dry_Spell. \n')
  # Dry_Spell <- 1:length(yrs_dts) %>%
  #   purrr::map(.f = function(i){
  #     prc <- terra::rast(chr_fls[chr_dts %in% yrs_dts[[i]]])
  #     prc <- prc %>% terra::crop(terra::ext(shp)) %>% terra::mask(shp)
  #     prc[prc == -9999] <- NA
  #     p_thresh=1
  #     Dry_Spell <- terra::app(x=prc,  fun = function(x){
  #       x = as.numeric(x)
  #       runs = rle(x <= 1)
  #       Positions = which(runs$values == 1 & runs$lengths > 4)
  #       num.dry.spell = length(Positions)
  #       Dry_Spell = runs$lengths[Positions[1:length(Positions)]]
  #       mean.length.dry.spell = round(mean(Dry_Spell),0)
  #       max.length.dry.spell = round(max(Dry_Spell),0)
  #       return(c(num.dry.spell,mean.length.dry.spell,max.length.dry.spell)) # Dry_Spell,mean.length.dry.spell,max.length.dry.spell
  #     })
  #     # CDD <- terra::app(x = prc, fun = function(x){ y = calc_cddCMP(PREC = x, p_thresh = 1); return(y) })
  #     names(Dry_Spell) <- lubridate::year(yrs_dts[[i]]) %>% unique() %>% paste0(collapse = '-')
  #     return(Dry_Spell)
  #   }) %>% terra::rast()
  # Dry_Spell <- Dry_Spell %>% terra::mask(shp)
  #
  cat('..... Computing: Dry_Spell. Number dry spell. \n')
  Dry_Spell <- 1:length(yrs_dts) %>%
    purrr::map(.f = function(i){
      prc <- terra::rast(chr_fls[chr_dts %in% yrs_dts[[i]]])
      prc <- prc %>% terra::crop(terra::ext(shp)) %>% terra::mask(shp)
      prc[prc == -9999] <- NA
      p_thresh=1
      Dry_Spell <- terra::app(x=prc,  fun = function(x){
        x = as.numeric(x)
        runs = rle(x <= 1)
        Positions = which(runs$values == 1 & runs$lengths > 4)
        num.dry.spell = length(Positions)
        return(num.dry.spell)
      })
      names(Dry_Spell) <- lubridate::year(yrs_dts[[i]]) %>% unique() %>% paste0(collapse = '-')
      return(Dry_Spell)
    }) %>% terra::rast()
  Dry_Spell <- Dry_Spell %>% terra::mask(shp)
  #
  cat('..... Computing: Dry_Spell. Mean lenght dry spell. \n')
  Dry_Spell_mean <- 1:length(yrs_dts) %>%
    purrr::map(.f = function(i){
      prc <- terra::rast(chr_fls[chr_dts %in% yrs_dts[[i]]])
      prc <- prc %>% terra::crop(terra::ext(shp)) %>% terra::mask(shp)
      prc[prc == -9999] <- NA
      p_thresh=1
      Dry_Spell_mean <- terra::app(x=prc,  fun = function(x){
        x = as.numeric(x)
        runs = rle(x <= 1)
        Positions = which(runs$values == 1 & runs$lengths > 4)
        Dry_Spell_mean = runs$lengths[Positions[1:length(Positions)]]
        mean.length.dry.spell = round(mean(Dry_Spell_mean),0)
        return(mean.length.dry.spell) 
      })
      names(Dry_Spell_mean) <- lubridate::year(yrs_dts[[i]]) %>% unique() %>% paste0(collapse = '-')
      return(Dry_Spell_mean)
    }) %>% terra::rast()
  Dry_Spell_mean <- Dry_Spell_mean %>% terra::mask(shp)
  #
  cat('..... Computing: Dry_Spell. Max lenght dry spell. \n')
  Dry_Spell_max <- 1:length(yrs_dts) %>%
    purrr::map(.f = function(i){
      prc <- terra::rast(chr_fls[chr_dts %in% yrs_dts[[i]]])
      prc <- prc %>% terra::crop(terra::ext(shp)) %>% terra::mask(shp)
      prc[prc == -9999] <- NA
      p_thresh=1
      Dry_Spell_max <- terra::app(x=prc,  fun = function(x){
        x = as.numeric(x)
        runs = rle(x <= 1)
        Positions = which(runs$values == 1 & runs$lengths > 4)
        Dry_Spell_max = runs$lengths[Positions[1:length(Positions)]]
        max.length.dry.spell = round(max(Dry_Spell_max),0)
        return(max.length.dry.spell)
      })
      names(Dry_Spell_max) <- lubridate::year(yrs_dts[[i]]) %>% unique() %>% paste0(collapse = '-')
      return(Dry_Spell_max)
    }) %>% terra::rast()
  Dry_Spell_max <- Dry_Spell_max %>% terra::mask(shp)
  #
  cat('..... Computing: Wet_Spell. number wet spell \n')
  Wet_Spell <- 1:length(yrs_dts) %>%
    purrr::map(.f = function(i){
      prc <- terra::rast(chr_fls[chr_dts %in% yrs_dts[[i]]])
      prc <- prc %>% terra::crop(terra::ext(shp)) %>% terra::mask(shp)
      prc[prc == -9999] <- NA
      p_thresh=1
      Wet_Spell <- terra::app(x=prc,  fun = function(x){
        x = as.numeric(x)
        runs = rle(x > 1)
        Positions = which(runs$values == 1 & runs$lengths > 4)
        num.dry.spell = length(Positions)
        return(num.dry.spell)
      })
      names(Wet_Spell) <- lubridate::year(yrs_dts[[i]]) %>% unique() %>% paste0(collapse = '-')
      return(Wet_Spell)
    }) %>% terra::rast()
  Wet_Spell <- Wet_Spell %>% terra::mask(shp)
  #
  cat('..... Computing: Dry_Spell. Mean lenght dry spell. \n')
  Wet_Spell_mean <- 1:length(yrs_dts) %>%
    purrr::map(.f = function(i){
      prc <- terra::rast(chr_fls[chr_dts %in% yrs_dts[[i]]])
      prc <- prc %>% terra::crop(terra::ext(shp)) %>% terra::mask(shp)
      prc[prc == -9999] <- NA
      p_thresh=1
      Wet_Spell_mean <- terra::app(x=prc,  fun = function(x){
        x = as.numeric(x)
        runs = rle(x > 1)
        Positions = which(runs$values == 1 & runs$lengths > 4)
        Wet_Spell_mean = runs$lengths[Positions[1:length(Positions)]]
        mean.length.dry.spell = round(mean(Wet_Spell_mean),0)
        max.length.dry.spell = round(max(Wet_Spell_mean),0)
        return(mean.length.dry.spell) 
      })
      names(Wet_Spell_mean) <- lubridate::year(yrs_dts[[i]]) %>% unique() %>% paste0(collapse = '-')
      return(Wet_Spell_mean)
    }) %>% terra::rast()
  Wet_Spell_mean <- Wet_Spell_mean %>% terra::mask(shp)
  #
  cat('..... Computing: Dry_Spell. Max lenght dry spell. \n')
  Wet_Spell_max <- 1:length(yrs_dts) %>%
    purrr::map(.f = function(i){
      prc <- terra::rast(chr_fls[chr_dts %in% yrs_dts[[i]]])
      prc <- prc %>% terra::crop(terra::ext(shp)) %>% terra::mask(shp)
      prc[prc == -9999] <- NA
      p_thresh=1
      Wet_Spell_max <- terra::app(x=prc,  fun = function(x){
        x = as.numeric(x)
        runs = rle(x > 1)
        Positions = which(runs$values == 1 & runs$lengths > 4)
        Wet_Spell_max = runs$lengths[Positions[1:length(Positions)]]
        max.length.dry.spell = round(max(Wet_Spell_max),0)
        return(max.length.dry.spell) 
      })
      names(Wet_Spell_max) <- lubridate::year(yrs_dts[[i]]) %>% unique() %>% paste0(collapse = '-')
      return(Wet_Spell_max)
    }) %>% terra::rast()
  Wet_Spell_max <- Wet_Spell_max %>% terra::mask(shp)
  #
  cat('..... End.\n')
  return(list(
    TR             = TR,
    NDD            = NDD,
    NRD            = NRD,
    Dry_Spell      = Dry_Spell,
    Dry_Spell_mean = Dry_Spell_mean,
    Dry_Spell_max  = Dry_Spell_max,
    Wet_Spell      = Wet_Spell,
    Wet_Spell_mean = Wet_Spell_mean,
    Wet_Spell_max  = Wet_Spell_max))
  
}

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

