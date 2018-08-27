ml3_dat <- read.csv('../data/study_02/ml3_stroop.csv')

# Round all numeric values to milliseconds
ml3_dat$MC <- round(ml3_dat$MC, 0)
ml3_dat$MI <- round(ml3_dat$MI, 0)
ml3_dat$SDC <- round(ml3_dat$SDC, 0)
ml3_dat$SDI <- round(ml3_dat$SDI, 0)

# Compute the results for the ML3 data
df_ml3 <- plyr::ddply(ml3_dat, .(study_name), function (x) {
  res <- data.frame(type = NA, test = NA, result = NA)

  res_t <- t.test(x$StroopEffect, mu = 0, var.equal = TRUE)
  es_r <- sqrt((res_t$statistic / res_t$parameter) / ((res_t$statistic / res_t$parameter) + 1))

  # Add NBL results  
  res <- rbind(res, c('Genuine', 'Benford, congruent means', digit_analysis(x$MC, type = 'benford')))
  res <- rbind(res, c('Genuine', 'Benford, congruent sds', digit_analysis(x$SDC, type = 'benford')))
  res <- rbind(res, c('Genuine', 'Benford, incongruent means', digit_analysis(x$MI, type = 'benford')))
  res <- rbind(res, c('Genuine', 'Benford, incongruent sds', digit_analysis(x$SDI, type = 'benford')))
  # Terminal digit analyses
  res <- rbind(res, c('Genuine', 'Terminal digits, congruent means', digit_analysis(x$MC, type = 'terminal')))
  res <- rbind(res, c('Genuine', 'Terminal digits, congruent sds', digit_analysis(x$SDC, type = 'terminal')))
  res <- rbind(res, c('Genuine', 'Terminal digits, incongruent means', digit_analysis(x$MI, type = 'terminal')))
  res <- rbind(res, c('Genuine', 'Terminal digits, incongruent sds', digit_analysis(x$SDI, type = 'terminal')))
  # Std var
  res <- rbind(res, c('Genuine', 'Variance analysis, congruent sds (maxmin)', std_var(sds = x$SDC, n = x$NC, iter = iter, method = 'maxmin', subgroups = rep(0, length(x$SDC)))))
  res <- rbind(res, c('Genuine', 'Variance analysis, incongruent sds (maxmin)', std_var(sds = x$SDI, n = x$NI, iter = iter, method = 'maxmin', subgroups = rep(0, length(x$SDI)))))
  # Multivariate associations
  res <- rbind(res, c('Genuine', 'Multivariate association, M-SD congruent', cor(x$MC, x$SDC)))
  res <- rbind(res, c('Genuine', 'Multivariate association, M-SD incongruent', cor(x$MI, x$SDI)))
  res <- rbind(res, c('Genuine', 'Multivariate association, M-M across', cor(x$MC, x$MI)))
  res <- rbind(res, c('Genuine', 'Multivariate association, SD-SD across', cor(x$SDC, x$SDI)))
  # Effect size
  res <- rbind(res, c('Genuine', 'Effect size (r)', es_r))
  # Sample size
  res$n <- length(x$MC)
  res$rng <- NA
  # Remove tmp row
  res <- res[-1,]

  return(res)
})

names(df_ml3)[1] <- 'id'
responses <- list.files('../data/study_02/responses')
df_fab <- data.frame(id = NA, type = NA, test = NA, result = NA, n = NA, rng = NA)
# Handcoded from transcripts doi:10.5281/zenodo.832490
# in the order of the object responses
rng <- as.factor(c(1, # 0jg
  1, # 19e
  1,
  0,
  1,
  0,
  0,
  0,
  1,
  0,
  1,
  1,
  1,
  1,
  1,
  0,
  1,
  0,
  1,
  1,
  1,
  1,
  0,
  1,
  1,
  1,
  1,
  0)) # z26
i <- 1

fab_digits <- data.frame(
    mean_congruent = NA,
    sd_congruent = NA,
    mean_incongruent = NA,
    sd_incongruent = NA,
    rng = NA)
  
for (response in responses) {
  fab_dat <- read.table(sprintf('../data/study_02/responses/%s', response),
   sep = '\t', header = TRUE)
        # For clarity sake
  names(fab_dat) <- c('id',
    'mean_congruent',
    'sd_congruent',
    'congruent_trials',
    'mean_incongruent',
    'sd_incongruent',
    'incongruent_trials')
        # Eliminate overprecision in reporting
        # Instructed participants to fabricate milliseconds and 123.45 is result of copy-paste
        # Spreadsheet automatically removed this, but by copy-pasting this was undone by PP
  fab_dat <- as.data.frame(apply(fab_dat, 2, function(x) round(x, 0)))
        # Stroop effect size calculation
  sdpooled <- sqrt(((fab_dat$congruent_trials - 1) * fab_dat$sd_congruent^2 + (fab_dat$incongruent_trials - 1) * fab_dat$sd_incongruent^2) / ((fab_dat$congruent_trials + fab_dat$incongruent_trials - 2)))
  stroopeffect <- (fab_dat$mean_incongruent - fab_dat$mean_congruent) / sdpooled
  x <- t.test(stroopeffect, mu = 0, var.equal = TRUE)
  es_r <- sqrt((x$statistic / x$parameter) / ((x$statistic / x$parameter) + 1))
  
  tmp_fab <- data.frame(id = response,
   type = 'Fabricated', 
   test = c('Benford, congruent means',
    'Benford, congruent sds',
    'Benford, incongruent means',
    'Benford, incongruent sds',
    'Terminal digits, congruent means',
    'Terminal digits, congruent sds',
    'Terminal digits, incongruent means',
    'Terminal digits, incongruent sds',
    'Variance analysis, congruent sds (maxmin)',
    'Variance analysis, incongruent sds (maxmin)',
    'Multivariate association, M-SD congruent',
    'Multivariate association, M-SD incongruent',
    'Multivariate association, M-M across',
    'Multivariate association, SD-SD across',
    'Effect size (r)'), 
   result = c(digit_analysis(fab_dat$mean_congruent, type = 'benford'),
    digit_analysis(fab_dat$sd_congruent, type = 'benford'),
    digit_analysis(fab_dat$mean_incongruent, type = 'benford'),
    digit_analysis(fab_dat$sd_incongruent, type = 'benford'),
    digit_analysis(fab_dat$mean_congruent, type = 'terminal'),
    digit_analysis(fab_dat$sd_congruent, type = 'terminal'),
    digit_analysis(fab_dat$mean_incongruent, type = 'terminal'),
    digit_analysis(fab_dat$sd_incongruent, type = 'terminal'),
    std_var(sds = fab_dat$sd_congruent, n = fab_dat$congruent_trials, iter = iter, method = 'maxmin', subgroups = rep(0, length(fab_dat$sd_congruent))),
    std_var(sds = fab_dat$sd_incongruent, n = fab_dat$incongruent_trials, iter = iter, method = 'maxmin', subgroups = rep(0, length(fab_dat$sd_incongruent))),
    cor(fab_dat$mean_congruent, fab_dat$sd_congruent),
    cor(fab_dat$mean_incongruent, fab_dat$sd_incongruent),
    cor(fab_dat$mean_congruent, fab_dat$mean_incongruent),
    cor(fab_dat$sd_congruent, fab_dat$sd_incongruent),
    es_r), 
   n = length(fab_dat$mean_congruent),
   rng = rng[i])

  # Add to digits object for later counts
  fab_digits <- rbind(fab_digits, data.frame(
    mean_congruent = fab_dat$mean_congruent,
    sd_congruent = fab_dat$sd_congruent,
    mean_incongruent = fab_dat$mean_incongruent,
    sd_incongruent = fab_dat$sd_incongruent,
    rng = rng[i]))

  i <- i + 1
  df_fab <- rbind(df_fab, tmp_fab)
}

# Remove tmp row
fab_digits <- fab_digits[-1,]
df_fab <- df_fab[-1,]
# Combine the two
df <- rbind(df_ml3, df_fab)
df$result <- as.numeric(df$result)

# Meta-analyze the genuine multivariate associations to acquire the parametric estimate
sel <- df$type == 'Genuine' & df$test == 'Multivariate association, M-SD congruent'
x <- metafor::rma(atanh(df$result[sel]), sei = 1 / sqrt(df$n[sel] - 3), method = 'REML')
mean_m_sd_congruent <- x$b[1]
tau_m_sd_congruent <- sqrt(x$tau2[1])
sel <- df$type == 'Genuine' & df$test == 'Multivariate association, M-SD incongruent'
x <- metafor::rma(atanh(df$result[sel]), sei = 1 / sqrt(df$n[sel] - 3), method = 'REML')
mean_m_sd_incongruent <- x$b[1]
tau_m_sd_incongruent <- sqrt(x$tau2[1])
sel <- df$type == 'Genuine' & df$test == 'Multivariate association, M-M across'
x <- metafor::rma(atanh(df$result[sel]), sei = 1 / sqrt(df$n[sel] - 3), method = 'REML')
mean_m_m_across <- x$b[1]
tau_m_m_across <- sqrt(x$tau2[1])
sel <- df$type == 'Genuine' & df$test == 'Multivariate association, SD-SD across'
x <- metafor::rma(atanh(df$result[sel]), sei = 1 / sqrt(df$n[sel] - 3), method = 'REML')
mean_sd_sd_across <- x$b[1]
tau_sd_sd_across <- sqrt(x$tau2[1])

# Compute the p-values for the multivariate associations per dataset
for (ds in unique(df$id)) {
  for (tst in c('Multivariate association, M-SD congruent',
    'Multivariate association, M-SD incongruent',
    'Multivariate association, M-M across',
    'Multivariate association, SD-SD across')) {
    sel <- df$id == ds & df$test == tst

    mean <- ifelse(tst == 'Multivariate association, M-SD congruent', mean_m_sd_congruent,
      ifelse(tst == 'Multivariate association, M-SD incongruent', mean_m_sd_incongruent, 
        ifelse(tst == 'Multivariate association, M-M across', mean_m_m_across, 
          ifelse(tst == 'Multivariate association, SD-SD across', mean_sd_sd_across, stop('Error')))))
    tau <- ifelse(tst == 'Multivariate association, M-SD congruent', tau_m_sd_congruent,
      ifelse(tst == 'Multivariate association, M-SD incongruent', tau_m_sd_incongruent, 
        ifelse(tst == 'Multivariate association, M-M across', tau_m_m_across, 
          ifelse(tst == 'Multivariate association, SD-SD across', tau_sd_sd_across, stop('Error')))))

    z_val <- (df$result[sel] - mean) / tau
    df <- rbind(df, data.frame(id = ds, 
      type = unique(df$type[sel])[1], 
      test = sprintf('Parametric test of %s', tst), 
      result = 2 * pnorm(q = abs(z_val), mean = 0, sd = 1, lower = FALSE), 
      n = unique(df$n[sel])[1], 
      rng = unique(df$rng[sel])[1]))
  }

  sel <- df$id == ds & grepl(df$test, pattern = '(Parametric|Variance|Terminal)')
  df <- rbind(df, data.frame(id = ds, 
      type = unique(df$type[sel])[1], 
      test = sprintf('Combination w Fisher method (k=10, 2x variance, 4x terminal, 4x associations)', tst), 
      result = fisher_method(df$result[sel])[,3], 
      n = unique(df$n[sel])[1], 
      rng = unique(df$rng[sel])[1]))
}


# Write out datafile
write.csv(df, file = '../data/study_02/ml3_fabricated_processed_collated.csv', row.names = FALSE)

# Digit distributions
df <- data.frame(type = NA, analysis = NA, digit = NA, measure = NA, count = NA)
# First digit
# ML3
x <- digit_counter(ml3_dat$MC, 'benford')
df <- rbind(df, data.frame(type = 'Genuine', 
  analysis = 'benford', 
  digit = names(x), 
  measure = 'Mean congruent', 
  count = x))
x <- digit_counter(ml3_dat$SDC, 'benford')
df <- rbind(df, data.frame(type = 'Genuine', 
  analysis = 'benford', 
  digit = names(x), 
  measure = 'SD congruent', 
  count = x))
x <- digit_counter(ml3_dat$MI, 'benford')
df <- rbind(df, data.frame(type = 'Genuine', 
  analysis = 'benford', 
  digit = names(x), 
  measure = 'Mean incongruent', 
  count = x))
x <- digit_counter(ml3_dat$SDI, 'benford')
df <- rbind(df, data.frame(type = 'Genuine', 
  analysis = 'benford', 
  digit = names(x), 
  measure = 'SD incongruent', 
  count = x))
# Fabricated
x <- digit_counter(fab_digits$mean_congruent, 'benford')
df <- rbind(df, data.frame(type = 'Fabricated', 
  analysis = 'benford', 
  digit = names(x), 
  measure = 'Mean congruent', 
  count = x))
x <- digit_counter(fab_digits$sd_congruent, 'benford')
df <- rbind(df, data.frame(type = 'Fabricated', 
  analysis = 'benford', 
  digit = names(x), 
  measure = 'SD congruent', 
  count = x))
x <- digit_counter(fab_digits$mean_incongruent, 'benford')
df <- rbind(df, data.frame(type = 'Fabricated', 
  analysis = 'benford', 
  digit = names(x), 
  measure = 'Mean incongruent', 
  count = x))
x <- digit_counter(fab_digits$sd_incongruent, 'benford')
df <- rbind(df, data.frame(type = 'Fabricated', 
  analysis = 'benford', 
  digit = names(x), 
  measure = 'SD incongruent', 
  count = x))
# Expected
x <- expected_digit_counter(fab_dat$mean_congruent, 'benford')
df <- rbind(df, data.frame(type = 'Expected', 
  analysis = 'benford', 
  digit = 1:9, 
  measure = 'Mean congruent', 
  count = x))
x <- expected_digit_counter(fab_dat$sd_congruent, 'benford')
df <- rbind(df, data.frame(type = 'Expected', 
  analysis = 'benford', 
  digit = 1:9, 
  measure = 'SD congruent', 
  count = x))
x <- expected_digit_counter(fab_dat$mean_incongruent, 'benford')
df <- rbind(df, data.frame(type = 'Expected', 
  analysis = 'benford', 
  digit = 1:9, 
  measure = 'Mean incongruent', 
  count = x))
x <- expected_digit_counter(fab_dat$sd_incongruent, 'benford')
df <- rbind(df, data.frame(type = 'Expected', 
  analysis = 'benford', 
  digit = 1:9, 
  measure = 'SD incongruent', 
  count = x))

# Last digit
# ML3
x <- digit_counter(ml3_dat$MC, 'terminal')
df <- rbind(df, data.frame(type = 'Genuine', 
  analysis = 'terminal', 
  digit = names(x), 
  measure = 'Mean congruent', 
  count = x))
x <- digit_counter(ml3_dat$SDC, 'terminal')
df <- rbind(df, data.frame(type = 'Genuine', 
  analysis = 'terminal', 
  digit = names(x), 
  measure = 'SD congruent', 
  count = x))
x <- digit_counter(ml3_dat$MI, 'terminal')
df <- rbind(df, data.frame(type = 'Genuine', 
  analysis = 'terminal', 
  digit = names(x), 
  measure = 'Mean incongruent', 
  count = x))
x <- digit_counter(ml3_dat$SDI, 'terminal')
df <- rbind(df, data.frame(type = 'Genuine', 
  analysis = 'terminal', 
  digit = names(x), 
  measure = 'SD incongruent', 
  count = x))
# Fabricated
x <- digit_counter(fab_digits$mean_congruent, 'terminal')
df <- rbind(df, data.frame(type = 'Fabricated', 
  analysis = 'terminal', 
  digit = names(x), 
  measure = 'Mean congruent', 
  count = x))
x <- digit_counter(fab_digits$sd_congruent, 'terminal')
df <- rbind(df, data.frame(type = 'Fabricated', 
  analysis = 'terminal', 
  digit = names(x), 
  measure = 'SD congruent', 
  count = x))
x <- digit_counter(fab_digits$mean_incongruent, 'terminal')
df <- rbind(df, data.frame(type = 'Fabricated', 
  analysis = 'terminal', 
  digit = names(x), 
  measure = 'Mean incongruent', 
  count = x))
x <- digit_counter(fab_digits$sd_incongruent, 'terminal')
df <- rbind(df, data.frame(type = 'Fabricated', 
  analysis = 'terminal', 
  digit = names(x), 
  measure = 'SD incongruent', 
  count = x))
# Expected
x <- expected_digit_counter(fab_dat$mean_congruent, 'terminal')
df <- rbind(df, data.frame(type = 'Expected', 
  analysis = 'terminal', 
  digit = 0:9, 
  measure = 'Mean congruent', 
  count = x))
x <- expected_digit_counter(fab_dat$sd_congruent, 'terminal')
df <- rbind(df, data.frame(type = 'Expected', 
  analysis = 'terminal', 
  digit = 0:9, 
  measure = 'SD congruent', 
  count = x))
x <- expected_digit_counter(fab_dat$mean_incongruent, 'terminal')
df <- rbind(df, data.frame(type = 'Expected', 
  analysis = 'terminal', 
  digit = 0:9, 
  measure = 'Mean incongruent', 
  count = x))
x <- expected_digit_counter(fab_dat$sd_incongruent, 'terminal')
df <- rbind(df, data.frame(type = 'Expected', 
  analysis = 'terminal', 
  digit = 0:9, 
  measure = 'SD incongruent', 
  count = x))

# Add the fabricated split for RNG and no RNG
# Fabricated w rng
x <- digit_counter(fab_digits$mean_congruent[fab_digits$rng == 1], 'benford')
df <- rbind(df, data.frame(type = 'Fabricated w RNG', 
  analysis = 'benford', 
  digit = names(x), 
  measure = 'Mean congruent', 
  count = x))
x <- digit_counter(fab_digits$sd_congruent[fab_digits$rng == 1], 'benford')
df <- rbind(df, data.frame(type = 'Fabricated w RNG', 
  analysis = 'benford', 
  digit = names(x), 
  measure = 'SD congruent', 
  count = x))
x <- digit_counter(fab_digits$mean_incongruent[fab_digits$rng == 1], 'benford')
df <- rbind(df, data.frame(type = 'Fabricated w RNG', 
  analysis = 'benford', 
  digit = names(x), 
  measure = 'Mean incongruent', 
  count = x))
x <- digit_counter(fab_digits$sd_incongruent[fab_digits$rng == 1], 'benford')
df <- rbind(df, data.frame(type = 'Fabricated w RNG', 
  analysis = 'benford', 
  digit = names(x), 
  measure = 'SD incongruent', 
  count = x))
x <- digit_counter(fab_digits$mean_congruent[fab_digits$rng == 1], 'terminal')
df <- rbind(df, data.frame(type = 'Fabricated w RNG', 
  analysis = 'terminal', 
  digit = names(x), 
  measure = 'Mean congruent', 
  count = x))
x <- digit_counter(fab_digits$sd_congruent[fab_digits$rng == 1], 'terminal')
df <- rbind(df, data.frame(type = 'Fabricated w RNG', 
  analysis = 'terminal', 
  digit = names(x), 
  measure = 'SD congruent', 
  count = x))
x <- digit_counter(fab_digits$mean_incongruent[fab_digits$rng == 1], 'terminal')
df <- rbind(df, data.frame(type = 'Fabricated w RNG', 
  analysis = 'terminal', 
  digit = names(x), 
  measure = 'Mean incongruent', 
  count = x))
x <- digit_counter(fab_digits$sd_incongruent[fab_digits$rng == 1], 'terminal')
df <- rbind(df, data.frame(type = 'Fabricated w RNG', 
  analysis = 'terminal', 
  digit = names(x), 
  measure = 'SD incongruent', 
  count = x))
# W/o RNG
x <- digit_counter(fab_digits$mean_congruent[fab_digits$rng == 0], 'benford')
df <- rbind(df, data.frame(type = 'Fabricated w/o RNG', 
  analysis = 'benford', 
  digit = names(x), 
  measure = 'Mean congruent', 
  count = x))
x <- digit_counter(fab_digits$sd_congruent[fab_digits$rng == 0], 'benford')
df <- rbind(df, data.frame(type = 'Fabricated w/o RNG', 
  analysis = 'benford', 
  digit = names(x), 
  measure = 'SD congruent', 
  count = x))
x <- digit_counter(fab_digits$mean_incongruent[fab_digits$rng == 0], 'benford')
df <- rbind(df, data.frame(type = 'Fabricated w/o RNG', 
  analysis = 'benford', 
  digit = names(x), 
  measure = 'Mean incongruent', 
  count = x))
x <- digit_counter(fab_digits$sd_incongruent[fab_digits$rng == 0], 'benford')
df <- rbind(df, data.frame(type = 'Fabricated w/o RNG', 
  analysis = 'benford', 
  digit = names(x), 
  measure = 'SD incongruent', 
  count = x))
x <- digit_counter(fab_digits$mean_congruent[fab_digits$rng == 0], 'terminal')
df <- rbind(df, data.frame(type = 'Fabricated w/o RNG', 
  analysis = 'terminal', 
  digit = names(x), 
  measure = 'Mean congruent', 
  count = x))
x <- digit_counter(fab_digits$sd_congruent[fab_digits$rng == 0], 'terminal')
df <- rbind(df, data.frame(type = 'Fabricated w/o RNG', 
  analysis = 'terminal', 
  digit = names(x), 
  measure = 'SD congruent', 
  count = x))
x <- digit_counter(fab_digits$mean_incongruent[fab_digits$rng == 0], 'terminal')
df <- rbind(df, data.frame(type = 'Fabricated w/o RNG', 
  analysis = 'terminal', 
  digit = names(x), 
  measure = 'Mean incongruent', 
  count = x))
x <- digit_counter(fab_digits$sd_incongruent[fab_digits$rng == 0], 'terminal')
df <- rbind(df, data.frame(type = 'Fabricated w/o RNG', 
  analysis = 'terminal', 
  digit = names(x), 
  measure = 'SD incongruent', 
  count = x))

# Remove tmp row
df <- df[-1,]

df <- plyr::ddply(df, .(type, analysis, measure), function (x) {
  prop <- x$count / sum(x$count)

  return(data.frame(digit = x$digit, prop))
})

write.csv(df, file = '../data/study_02/digit_counts.csv', row.names = FALSE)
