# =-----------------------------------------------=
# CDS dataset description
# https://cds.climate.copernicus.eu/cdsapp#!/dataset/sis-agrometeorological-indicators?tab=overview
# =-----------------------------------------------=
# By: Anny, 2022
# Modify by: Hugo Dorado
# Modify by: Cesar A
# =-----------------------------------------------=
# Install only first time
# devtools::install_github("bluegreen-labs/ecmwfr") 
# https://bluegreen-labs.github.io/ecmwfr/
# register in https://cds.climate.copernicus.eu/user/139257

library(ecmwfr)
# =-----------------------------------------------=
# Registrarse en Copernicus para obtener UID y Key 
# credentials
UID = "129570"
key = "a2dd82bf-3cd2-4216-9f8f-XXXXXXX"

# =-----------------------------------------------=
# save key for CDS
wf_set_key(user = UID,
           key = key,
           service = "cds")

# =-----------------------------------------------=
getERA5 <- function(i, qq, year, month, datadir){
  q <- qq[i,]
  format <- "zip"
  ofile <- paste0(paste(q$variable, q$statistics, year, month, sep = "-"), ".",format)
  
  if(!file.exists(file.path(datadir,ofile))){
    ndays <- lubridate::days_in_month(as.Date(paste0(year, "-" ,month, "-01")))
    ndays <- 1:ndays
    ndays <- sapply(ndays, function(x) ifelse(length(x) == 1, sprintf("%02d", x), x))
    ndays <- dput(as.character(ndays))
    
    cat("Downloading", q[!is.na(q)], "for", year, month, "\n"); flush.console();
    
    request <- list("dataset_short_name" = "sis-agrometeorological-indicators",
                    "version"="1_1" ,
                    "variable" = q$variable,
                    "statistic" = q$statistics,
                    "year" = year,
                    "month" = month,
                    "day" = ndays,
                    "area" = c(90,-180,-90,179.9), # download global ("90/-180/-90/179.9") c(ymax,xmin,ymin,xmax)? 
                    "time" = q$time,
                    "format" = format,
                    "target" = ofile)
    
    request <- Filter(Negate(anyNA), request)
    
    file <- wf_request(user     = UID,   # user ID (for authentification)
                       request  = request,  # the request
                       transfer = TRUE,     # download the file
                       path     = datadir)  
  } else {
    cat("Already exists", q[!is.na(q)], "for", year, month, "\n"); flush.console();
  }
  return(NULL)
}

# =-----------------------------------------------=
# change data directory
datadir <- "//CATALOGUE/WFP_ClimateRiskPr1/1.Data/ERA5/zip" # 'My repository dir'
if(!dir.exists(datadir)){dir.create(datadir,F,T)}
# =-----------------------------------------------=
# combinations to download
# list of variables
# https://cds.climate.copernicus.eu/cdsapp#!/dataset/sis-agrometeorological-indicators?tab=overview
# =-----------------------------------------------=
qq <- data.frame(variable = c("solar_radiation_flux",
                              rep("2m_temperature",3),
                              "10m_wind_speed", 
                              "2m_relative_humidity",
                              "vapour_pressure",
                              "2m_dewpoint_temperature"),
                 statistics = c(NA, 
                                "24_hour_maximum", 
                                "24_hour_mean", 
                                "24_hour_minimum",
                                "24_hour_mean", 
                                NA,
                                "24_hour_mean",
                                "24_hour_mean"),
                 time = c(NA,
                          NA,
                          NA,
                          NA,
                          NA,
                          "12_00",
                          NA,
                          NA))

# =-----------------------------------------------=
# temporal range
years <- as.character(1981:2023)
months <- c(paste0("0", 1:9), 10:12)
# =-----------------------------------------------=
# all download
for (i in 1:nrow(qq)){
  for (year in years){
    for (month in months){
      tryCatch(getERA5(i, qq, year, month, datadir), error = function(e)NULL)
    }
  }
}
# =-----------------------------------------------=
# unzip
datadir <- "//CATALOGUE/WFP_ClimateRiskPr1/1.Data/ERA5/zip"
zz <- list.files(datadir, ".zip$", full.names = TRUE)

vars <- c("solar_radiation_flux",
          "10m_wind_speed",
          "2m_temperature-24_hour_maximum",
          "2m_temperature-24_hour_mean",
          "2m_temperature-24_hour_minimum",
          "2m_relative_humidity",
          "vapour_pressure",
          "2m_dewpoint_temperature")

extractNC <- function(var, zz, datadir, ncores = 1){
  z <- grep(var, zz, value = TRUE)
  fdir <- file.path(dirname(datadir), var)
  dir.create(fdir, showWarnings = FALSE, recursive = TRUE)
  parallel::mclapply(z, function(x){unzip(x, exdir = fdir)}, mc.cores = ncores, mc.preschedule = FALSE)
  return(NULL)
} 

for (var in vars){
  extractNC(var, zz, datadir, ncores = 1)
}
# =-----------------------------------------------=

