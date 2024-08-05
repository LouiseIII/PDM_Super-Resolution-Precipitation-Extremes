'Le but de ce fichier va etre de generer les location, shape et scale parameters pour chaque localication'

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


