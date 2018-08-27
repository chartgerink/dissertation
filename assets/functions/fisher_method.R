fisher_method <- function(pval,
                          threshold = 0,
                          method = 'default')
{  
  if ( method == 'default' ) {
    fish <- -2 * sum(log(pval))    
  } else if ( method == 'reversed' ) {
    fish <- -2 * sum(log(1 - ((pval[pval > threshold] - threshold) / (1 - threshold))))
  } else {
    stop('Invalid method. Please see ?ddfab::fisher_method')
  }

  df_fish <- sum(pval > threshold) * 2
  p_fish <- pchisq(q = fish, df = df_fish, lower.tail = FALSE)
  
  return(cbind(fish, df_fish, p_fish))
}