msw <- function(n, sds, subgroups) {
    res <- NULL
  
      for (i in unique(subgroups)) {
        sel <- which(subgroups == i)
        tmp <- sum((n[sel] - 1) * sds[sel]^2) / sum((n[sel] - 1))
        res <- c(res, tmp)
      }
      
    return(res)
}

z2 <- function(n, sds, subgroups)
{
  res <- NULL
  j <- 1
  numer <- sds^2
  denom <- msw(n, sds, subgroups)

  for (i in unique(subgroups)) {
    sel <- which(subgroups == i)
    tmp <- numer[sel] / denom[j]
    res <- c(res, tmp)
    j <- j + 1
  }

  return(res)    
}

std_var <- function(n, sds,
 subgroups, iter = 1000,
 method = 'sd') {
  # Notation:
  # z2 = standardized variances

  if (length(n) != length(sds) || 
    length(n) != length(subgroups) ||
    length(sds) != length(subgroups)) {
      stop('Unequal vectors supplied.')
  }

  z2_obs <- z2(n, sds, subgroups)
  
  sd2_sim <- apply(t(n - 1), 2, function(x) {
    step1 <- rchisq(n = iter, df = x)
    step2 <- step1 / x
  })
  
  # Compute msw for simulated variances (per row)
  # split for subgroups
  msw_sim <- apply(sqrt(sd2_sim), 1, function(x) {
    msw(n = n, sds = x, subgroups)
  })

  if (length(unique(subgroups)) == 1) msw_sim <- t(msw_sim)

  # Normalize simulated variances
  # DEPENDENT on their subgroup!
  z2_sim <- apply(sqrt(sd2_sim), 1, function (x) {
    z2(n = n, sds = x, subgroups)
  })
  
  # Calculate SD for each iteration
  if ( method == 'sd' ) {
    z2_obs_res <- sd(z2_obs)
    z2_sim_sd <- apply(z2_sim, 2, sd)

    res <- sum(z2_sim_sd < z2_obs_res) / iter
  } else if ( method == 'maxmin' ) {
    z2_obs_res <- max(z2_obs) - min(z2_obs)
    z2_sim_maxmin <- apply(z2_sim, 1, function(x) {
      return(max(x) - min(x))
    })

    res <- sum(z2_sim_maxmin < z2_obs_res) / iter
  } else {
    stop('Incorrect method. Please see ?ddfab::std_var')  
  }
  
  return(res)
}

