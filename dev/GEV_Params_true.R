'Le but de ce fichier va etre de generer les location, shape et scale parameters pour chaque localication'

library(readr)      
library(evd)         
library(stats)       
library(dplyr)      
library(extRemes)
library(SpatialExtremes)


path <- '~/Documents/ECCE/GitHub'
path <- paste(path, '/PDM_Super-Resolution-Precipitation-Extremes', sep='')

# ----------------------------------------------------------------
# Pour chaque position
# ----------------------------------------------------------------

path1 <- paste(path, '/data/data2_summer_present_HR.csv', sep='')
data <- read.csv(path1)

liste_rlat <- numeric(0)
liste_rlon <- numeric(0)
liste_loc <- numeric(0)
liste_scale <- numeric(0)
liste_shape <- numeric(0)
liste_conv <- numeric(0)
liste_pvalue <- numeric(0)

for (lat in unique(data$rlat)) {
  for (lon in unique(data$rlon)) {
    data_local <- data[data$rlat == lat & data$rlon == lon, ]
    result <- tryCatch({
      
      gev_estim <- gev.fit(data_local[, 'TOT_PR'], maxit = 50000, show = FALSE)
      ks_test <- ks.test(data_local[, 'TOT_PR'], "pgev", loc = gev_estim$mle[1], scale = gev_estim$mle[2], shape = gev_estim$mle[3])
      
      if (is.nan(gev_estim$mle[1])){
        print('PB MLE : ')
        print(block)
      }else {
        
        # Stocker les résultats
        liste_conv <- c(liste_conv, gev_estim$conv)
        liste_rlat <- c(liste_rlat, lat)
        liste_rlon <- c(liste_rlon, lon)
        liste_pvalue <- c(liste_pvalue, ks_test$p.value)
        
        if (gev_estim$conv == 1){print('PB convergence : ')
          print(c(rlat, rlon))}
        if (ks_test$p.value < 0.05){print('PB KS test : ')
          print(c(rlat, rlon))}
        
        liste_loc <- c(liste_loc, gev_estim$mle[1])
        liste_scale <- c(liste_scale, gev_estim$mle[2])
        liste_shape <- c(liste_shape, gev_estim$mle[3])
        
      }}, error = function(e) {
        print('Error')
        print(c(rlat, rlon))
      })
  }
}


data_set <- data.frame(rlat = liste_rlat, 
                       rlon = liste_rlon,
                       loc = liste_loc,
                       scale = liste_scale,
                       shape = liste_shape)


path2 <- paste(path, '/output/True_Parameters/gev2_param_true_present.csv', sep='')
write.csv(data_set, file = path2, row.names = FALSE)




# ----------------------------------------------------------------
# Pour chaque block
# ----------------------------------------------------------------

path1 <- paste(path, '/data/data2_summer_present_LR12.csv', sep='')
data <- read.csv(path1)

liste_block <- numeric(0)
liste_loc <- numeric(0)
liste_scale <- numeric(0)
liste_shape <- numeric(0)
liste_conv <- numeric(0)
liste_pvalue <- numeric(0)

for (block in unique(data$block)) {
  data_local <- data[data$block == block, ]
  result <- tryCatch({
    
    gev_estim <- gev.fit(data_local[, 'TOT_PR'], maxit = 50000, show = FALSE)
    ks_test <- ks.test(data_local[, 'TOT_PR'], "pgev", loc = gev_estim$mle[1], scale = gev_estim$mle[2], shape = gev_estim$mle[3])
    
    if (is.nan(gev_estim$mle[1])){
      print('PB MLE : ')
      print(block)
    }else {
      
      # Stocker les résultats
      liste_conv <- c(liste_conv, gev_estim$conv)
      liste_block <- c(liste_block, block)
      liste_pvalue <- c(liste_pvalue, ks_test$p.value)
      
      if (gev_estim$conv == 1){print('PB convergence : ')
        print(block)}
      if (ks_test$p.value < 0.05){print('PB KS test : ')
        print(block)}
      
      liste_loc <- c(liste_loc, gev_estim$mle[1])
      liste_scale <- c(liste_scale, gev_estim$mle[2])
      liste_shape <- c(liste_shape, gev_estim$mle[3])
      
    }}, error = function(e) {
      print('Error')
      print(block)
    })
  
}


data_set <- data.frame(block = liste_block, 
                       loc = liste_loc,
                       scale = liste_scale,
                       shape = liste_shape)

path2 <- paste(path, '/output/True_Parameters/gev2_param_true_present12.csv', sep='')
write.csv(data_set, file = path2, row.names = FALSE)






# ----------------------------------------------------------------
# CI 
# ----------------------------------------------------------------

# Initialisation
liste_lat <- numeric(0)
liste_lon <- numeric(0)

liste_loc <- numeric(0)
liste_scale <- numeric(0)
liste_shape <- numeric(0)

liste_sd_locb <- numeric(0)
liste_sd_scaleb <- numeric(0)
liste_sd_shapeb <- numeric(0)
liste_error <- numeric(0)

# Telechargement des données

path1 <- paste(path, '/data/data2_summer_present_HR.csv', sep='')
data <- read.csv(path1)

confidence_level <- 0.95
z <- qnorm(1 - (1 - confidence_level) / 2)
B <- 200
alpha <- 0.05

liste_pvalue <- numeric(0)
ind_pvalue <- 0

ind <- 0
for (lat in unique(data$rlat)) {
  for (lon in unique(data$rlon)) {
    ind <- ind + 1
      if(ind %% 100 == 0){print(ind)}
      data_local <- data[data$rlat == lat & data$rlon == lon, ]
      
      tryCatch({
        gev_estim <- gev.fit(data_local[, 'TOT_PR'], maxit = 50000, show = FALSE)
        
        liste_loc <- c(liste_loc, gev_estim$mle[1])
        liste_scale <- c(liste_scale, gev_estim$mle[2])
        liste_shape <- c(liste_shape, gev_estim$mle[3])
        
        liste_locb <- rep(NA, B)
        liste_scaleb <- rep(NA, B)
        liste_shapeb <- rep(NA, B)
        
        for (b in 1:B) {
          
          sample_gev <- sample(data_local$TOT_PR, replace = TRUE, size = length(data_local$TOT_PR))
          fit <- gev.fit(sample_gev, maxit = 50000, show = FALSE)

          liste_locb[b] <- fit$mle[1]
          liste_scaleb[b] <- fit$mle[2]
          liste_shapeb[b] <- fit$mle[3]
          
        }
        
        if(sum(is.na(liste_locb))>50){
          print(lat)
          print(lon)}
        
        sd_locb <- sd(liste_locb, na.rm = TRUE)
        sd_scaleb <- sd(liste_scaleb, na.rm = TRUE)
        sd_shapeb <- sd(liste_shapeb, na.rm = TRUE)
        
        liste_sd_locb <- c(liste_sd_locb, sd_locb)
        liste_sd_scaleb <- c(liste_sd_scaleb, sd_scaleb)
        liste_sd_shapeb <- c(liste_sd_shapeb, sd_shapeb)
        
        sd_locb <- abs(gev_estim$mle[1] - 0.00450017276230264)/sd_locb # mean loc over the domain obtained before 
        liste_pvalue <- c(liste_pvalue, sd_locb > 1.96)
        if(sd_locb > 1.96){ind_pvalue = ind_pvalue + 1}
        
        liste_lat <- c(liste_lat, lat)
        liste_lon <- c(liste_lon, lon)
        
      })
  }
}



data_set <- data.frame(rlat = liste_lat, 
                       rlon = liste_lon, 
                       loc = liste_loc,
                       scale = liste_scale,
                       shape = liste_shape,
                       sv = liste_pvalue,
                       sdloc = liste_sd_locb,
                       sdscale = liste_sd_scaleb,
                       sdshape = liste_sd_shapeb)

path2 <- paste(path, '/data/test_sv_present_HR.csv', sep='')
write.csv(data_set, file = path2, row.names = FALSE)

