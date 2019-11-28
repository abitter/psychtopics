# Trends for range input
########################

# original code by M. Ponweiser: https://github.com/mponweiser/thesis-LDA-in-R


trends.ab <- function(von, bis, 
                      theta_year, theta_mean_by_year, 
                      theta_mean_by_year_time, 
                      theta_mean_by_year_ts, years, topic){
  
  #Linear model
  theta_mean_lm <- apply(theta_mean_by_year[von:bis,], 2, function(x) lm(x ~ theta_mean_by_year_time[von:bis])) # 2 is margin for columns
  theta_mean_lm_coef <- lapply(theta_mean_lm,function(x) coef(summary(x)))
  theta_mean_lm_coef_sign <- sapply(theta_mean_lm_coef,'[',"theta_mean_by_year_time[von:bis]","Pr(>|t|)")
  theta_mean_lm_coef_slope <- sapply(theta_mean_lm_coef,'[',"theta_mean_by_year_time[von:bis]","Estimate")
  
  # devide in positive and negative slopes
  theta_mean_lm_coef_slope_pos <- theta_mean_lm_coef_slope[theta_mean_lm_coef_slope >= 0]
  theta_mean_lm_coef_slope_neg <- theta_mean_lm_coef_slope[theta_mean_lm_coef_slope < 0]
  
  
  ### hot & cold
  topics_hot <- as.numeric(names(sort(theta_mean_lm_coef_slope_pos, decreasing=TRUE)))
  topics_cold <- as.numeric(names(sort(theta_mean_lm_coef_slope_neg, decreasing=FALSE)))
  
  hot_ts <- ts(theta_mean_by_year_ts[von:bis,topics_hot[1:10]], start = as.integer(years[von]))
  cold_ts <- ts(theta_mean_by_year_ts[von:bis,topics_cold[1:10]], start = as.integer(years[von]))
  
  # tables
  terms_hot <- topic[topics_hot[1:10],-3]
  terms_hot$rank <- 1:10
  terms_hot[ ,c(1,2,3)] <- terms_hot[ ,c(3,1,2)]
  names(terms_hot) <- c("Rang", "NR", "Thema")
  
  terms_cold <- topic[topics_cold[1:10],-3]
  terms_cold$rank <- 1:10
  terms_cold[ ,c(1,2,3)] <- terms_cold[ ,c(3,1,2)]
  names(terms_cold) <- c("Rang", "NR", "Thema")
  
  # results
  results <- list()
  results[1] <- list(terms_hot)
  results[2] <- list(terms_cold)
  results[3] <- list(hot_ts)
  results[4] <- list(cold_ts)
  
  return(results)
  
}
