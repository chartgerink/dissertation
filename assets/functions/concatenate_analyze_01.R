options(digits = 20)

# Read the data
# This is run from within the Rmarkdown file
ml <- read.csv('../data/study_01/ml_summary_stats.csv', stringsAsFactors = FALSE)
qualtrics <- read.csv('../data/study_01/qualtrics_processed.csv', stringsAsFactors = FALSE)

iter <- 100000

ml$stat_know <- NA
ml$spss <- NA
ml$rcran <- NA
ml$stata <- NA
ml$sas <- NA
ml$matlab <- NA
ml$python <- NA
ml$other <- NA
ml$nopro <- NA
ml$noanch <- NA
ml$rng <- NA
ml$full <- NA
ml$tries <- NA
ml$descr <- NA
ml$bonus <- NA
ml$type <- 'ml'
qualtrics$type <- 'qualtrics'

dat <- rbind(qualtrics, ml)
write_df <- NULL

for (pp in unique(dat$referrer)) {
  print(pp)
  sel <- dat$referrer == pp

# VARIANCE ANALYSIS
  for (met in c('sd', 'maxmin')) {
    write_df <- rbind(write_df, data.frame(id = pp, 
    rng = dat$rng[sel],
    noanch = dat$noanch[sel],
    bonus = dat$bonus[sel],
    study = 'Overall', 
    test = sprintf('Variance analysis %s [homogeneity]', met), 
    result = std_var(method = met, n = dat$anch_n[sel], sds = dat$anch_sd[sel], 
      subgroups = c(rep(1, 4), rep(2, 4), rep(3, 4), rep(4, 4)), iter)))

    j <- 1
    selecta <- cbind(seq(1, 16, 4), seq(4 , 16, 4))
    for (i in 1:4) {
      start <- selecta[i,1]
      end <- selecta[i,2]
      write_df <- rbind(write_df, data.frame(id = pp, 
      rng = dat$rng[sel],
      noanch = dat$noanch[sel],
      bonus = dat$bonus[sel],
        study = sprintf('Study %s', j), 
        test = sprintf('Variance analysis %s [homogeneity]', met), 
        result = std_var(method = met, n = dat$anch_n[sel][start:end], 
          sds = dat$anch_sd[sel][start:end], 
          subgroups = rep(1, 4), iter)))
        j <- j + 1
    }

    write_df <- rbind(write_df, data.frame(id = pp, 
    rng = dat$rng[sel],
    noanch = dat$noanch[sel],
    bonus = dat$bonus[sel],
      study = 'Overall', 
      test = sprintf('Variance analysis %s [heterogeneity]', met), 
      result = std_var(method = met, n = dat$anch_n[sel], sds = dat$anch_sd[sel], 
        subgroups = c(rep(1, 2), rep(2, 2), rep(3, 2), rep(4, 2),
          rep(5, 2), rep(6, 2), rep(7, 2), rep(8, 2)), iter)))
    
    j <- 1
    selecta <- cbind(seq(1, 16, 2), seq(2 , 16, 2))
    for (i in 1:8) {
      start <- selecta[i,1]
      end <- selecta[i,2]
      write_df <- rbind(write_df, data.frame(id = pp, 
      rng = dat$rng[sel],
      noanch = dat$noanch[sel],
      bonus = dat$bonus[sel],
        study = sprintf('Study %s', j), 
        test = sprintf('Variance analysis %s, %s anchoring condition [heterogeneity]',
         met, ifelse(dat$anchoring[sel][start:end][1] == 0, 'low', 'high')), 
        result = std_var(method = met, n = dat$anch_n[sel][start:end], 
          sds = dat$anch_sd[sel][start:end], 
          subgroups = c(1,1), iter)))
        if (i == 2 || i == 4 || i == 6 ) j <- j + 1
    }
    
  }

# EFFECT SIZES
  for (j in 1:4) {
    indexor <- dat$referrer == pp & dat$study == j
    
    harm_mean <- 4^2/(sum(1/dat$anch_n[indexor]))
    male_mean <- mean(dat$anch_m[indexor & dat$partgender == 0])
    female_mean <- mean(dat$anch_m[indexor & dat$partgender == 1])
    control_mean <- mean(dat$anch_m[indexor & dat$anchoring == 0])
    exp_mean <- mean(dat$anch_m[indexor & dat$anchoring == 1])
    
    ss_error <- sum((dat$anch_n[indexor] - 1) * dat$anch_sd[indexor]^2)
    ss_gender <- (harm_mean * var(c(male_mean, female_mean))) / 2
    ss_condition <- (harm_mean * var(c(control_mean, exp_mean))) / 2
    ss_interaction <- harm_mean * var(dat$anch_m[indexor]) * 3/4 - ss_gender - ss_condition
    ss_total <- ss_error + ss_gender + ss_condition + ss_interaction
    
    ms_error <- ss_error / (sum(dat$anch_n[indexor]) - 4)
    ms_gender <- ss_gender / (2 - 1)
    ms_condition <- ss_condition / (2 - 1)
    ms_interaction <- ss_interaction / 1
    
    dat$f_gender[indexor] <- ms_gender / ms_error
    dat$df1_gender[indexor] <- 2 - 1
    dat$df2_gender[indexor] <- sum(dat$anch_n[indexor]) - 4
    dat$p_gender[indexor] <- pf(q = ms_gender / ms_error,
                                df1 = 2 - 1,
                                df2 = sum(dat$anch_n[indexor]) - 4,
                                lower.tail = FALSE)
    write_df <- rbind(write_df, data.frame(id = pp, 
    rng = dat$rng[sel],
    noanch = dat$noanch[sel],
    bonus = dat$bonus[sel],
      study = sprintf('Study %s', j), 
      test = 'P-value gender', 
      result = unique(dat$p_gender[indexor])))
    dat$es2_gender[indexor] <- 
      (((ms_gender / ms_error) * (2 -1)) / (sum(dat$anch_n[indexor]) - 4)) / 
      ((((ms_gender / ms_error) * (2 -1)) / (sum(dat$anch_n[indexor]) - 4)) + 1)
      write_df <- rbind(write_df, data.frame(id = pp, 
      rng = dat$rng[sel],
      noanch = dat$noanch[sel],
      bonus = dat$bonus[sel],
      study = sprintf('Study %s', j), 
      test = 'Effect size (r2) gender', 
      result = unique(dat$es2_gender[indexor])))
    
    dat$f_condition[indexor] <- ms_condition / ms_error
    dat$df1_condition[indexor] <- 2 - 1
    dat$df2_condition[indexor] <- sum(dat$anch_n[indexor]) - 4
    dat$p_condition[indexor] <- pf(q = ms_condition / ms_error,
                                   df1 = 2 - 1,
                                   df2 = sum(dat$anch_n[indexor]) - 4,
                                   lower.tail = FALSE)
    write_df <- rbind(write_df, data.frame(id = pp, 
    rng = dat$rng[sel],
    noanch = dat$noanch[sel],
    bonus = dat$bonus[sel],
      study = sprintf('Study %s', j), 
      test = 'P-value anchoring', 
      result = unique(dat$p_condition[indexor])))

    dat$es2_condition[indexor] <- 
      (((ms_condition / ms_error) * (2 -1)) / (sum(dat$anch_n[indexor]) - 4)) / 
      ((((ms_condition / ms_error) * (2 -1)) / (sum(dat$anch_n[indexor]) - 4)) + 1)
    write_df <- rbind(write_df, data.frame(id = pp, 
    rng = dat$rng[sel],
    noanch = dat$noanch[sel],
    bonus = dat$bonus[sel],
    study = sprintf('Study %s', j), 
    test = 'Effect size (r2) anchoring', 
    result = unique(dat$es2_condition[indexor])))
    
    dat$f_interaction[indexor] <- ms_interaction / ms_error
    dat$df1_interaction[indexor] <- 1
    dat$df2_interaction[indexor] <- sum(dat$anch_n[indexor]) - 4
    dat$p_interaction[indexor] <- pf(q = ms_interaction / ms_error,
                                     df1 = 1,
                                     df2 = sum(dat$anch_n[indexor]) - 4,
                                     lower.tail = FALSE)
    write_df <- rbind(write_df, data.frame(id = pp, 
    rng = dat$rng[sel],
    noanch = dat$noanch[sel],
    bonus = dat$bonus[sel],
      study = sprintf('Study %s', j), 
      test = 'P-value interaction', 
      result = unique(dat$p_interaction[indexor])))
    dat$es2_interaction[indexor] <- 
      (((ms_interaction / ms_error) * 1) / (sum(dat$anch_n[indexor]) - 4)) / 
      ((((ms_interaction / ms_error) * 1) / (sum(dat$anch_n[indexor]) - 4)) + 1)
    write_df <- rbind(write_df, data.frame(id = pp, 
    rng = dat$rng[sel],
    noanch = dat$noanch[sel],
    bonus = dat$bonus[sel],
      study = sprintf('Study %s', j), 
      test = 'Effect size (r2) interaction', 
      result = unique(dat$es2_interaction[indexor])))
  }

  # P-VALUE ANALYSIS
  gen_sel <- write_df$id == pp & write_df$test == 'P-value gender'
  int_sel <- write_df$id == pp & write_df$test == 'P-value interaction'
  
  write_df <- rbind(write_df, data.frame(id = pp,
  rng = dat$rng[sel],
  noanch = dat$noanch[sel],
  bonus = dat$bonus[sel],
    study = 'Overall',
    test = 'Fisher method gender p-values',
    result = fisher_method(pval = write_df$result[gen_sel], 
      threshold = .05, 
      method = 'reversed')[3]))
  write_df <- rbind(write_df, data.frame(id = pp,
  rng = dat$rng[sel],
  noanch = dat$noanch[sel],
  bonus = dat$bonus[sel],
    study = 'Overall',
    test = 'Fisher method interaction p-values',
    result = fisher_method(pval = write_df$result[int_sel],
     threshold = .05,
     method = 'reversed')[3]))
  
  # COMBINING METHODS
  write_df <- rbind(write_df, data.frame(id = pp,
  rng = dat$rng[sel],
  noanch = dat$noanch[sel],
  bonus = dat$bonus[sel],
    study = 'Overall',
    test = 'Combination Fisher method (gender, interaction, variance sd [homogeneity, combined])',
    result = fisher_method(pval = write_df$result[(write_df$id == pp & write_df$study == 'Overall' & write_df$test == 'Fisher method gender p-values') | 
      (write_df$id == pp & write_df$study == 'Overall' & write_df$test == 'Fisher method interaction p-values') |
      (write_df$id == pp & write_df$study == 'Overall' & write_df$test == 'Variance analysis sd [homogeneity]')],
      threshold = .05)[3]))
  
  write_df <- rbind(write_df, data.frame(id = pp,
  rng = dat$rng[sel],
  noanch = dat$noanch[sel],
  bonus = dat$bonus[sel],
    study = 'Overall',
    test = 'Combination Fisher method (gender, interaction, variance sd [heterogeneity, combined])',
    result = fisher_method(pval = write_df$result[(write_df$id == pp & write_df$study == 'Overall' & write_df$test == 'Fisher method gender p-values') | 
      (write_df$id == pp & write_df$study == 'Overall' & write_df$test == 'Fisher method interaction p-values') |
      (write_df$id == pp & write_df$study == 'Overall' & write_df$test == 'Variance analysis sd [heterogeneity]')],
      threshold = .05)[3]))
  
  write_df <- rbind(write_df, data.frame(id = pp,
  rng = dat$rng[sel],
  noanch = dat$noanch[sel],
  bonus = dat$bonus[sel],
    study = 'Overall',
    test = 'Combination Fisher method (gender, interaction, variance sd [homogeneity, split])',
    result = fisher_method(pval = write_df$result[(write_df$id == pp & write_df$study == 'Overall' & write_df$test == 'Fisher method gender p-values') | 
      (write_df$id == pp & write_df$study == 'Overall' & write_df$test == 'Fisher method interaction p-values') |
      (write_df$id == pp & write_df$study == 'Study 1' & write_df$test == 'Variance analysis sd [homogeneity]') |
      (write_df$id == pp & write_df$study == 'Study 2' & write_df$test == 'Variance analysis sd [homogeneity]') |
      (write_df$id == pp & write_df$study == 'Study 3' & write_df$test == 'Variance analysis sd [homogeneity]') |
      (write_df$id == pp & write_df$study == 'Study 4' & write_df$test == 'Variance analysis sd [homogeneity]')],
      threshold = .05)[3]))
  
  write_df <- rbind(write_df, data.frame(id = pp,
  rng = dat$rng[sel],
  noanch = dat$noanch[sel],
  bonus = dat$bonus[sel],
    study = 'Overall',
    test = 'Combination Fisher method (gender, interaction, variance sd [heterogeneity, split])',
    result = fisher_method(pval = write_df$result[(write_df$id == pp & write_df$study == 'Overall' & write_df$test == 'Fisher method gender p-values') | 
      (write_df$id == pp & write_df$study == 'Overall' & write_df$test == 'Fisher method interaction p-values') |
      (write_df$id == pp & write_df$study == 'Study 1' & write_df$test == 'Variance analysis sd, low anchoring condition [heterogeneity]') |
      (write_df$id == pp & write_df$study == 'Study 1' & write_df$test == 'Variance analysis sd, high anchoring condition [heterogeneity]') |
      (write_df$id == pp & write_df$study == 'Study 2' & write_df$test == 'Variance analysis sd, low anchoring condition [heterogeneity]') |
      (write_df$id == pp & write_df$study == 'Study 2' & write_df$test == 'Variance analysis sd, high anchoring condition [heterogeneity]') |
      (write_df$id == pp & write_df$study == 'Study 3' & write_df$test == 'Variance analysis sd, low anchoring condition [heterogeneity]') |
      (write_df$id == pp & write_df$study == 'Study 3' & write_df$test == 'Variance analysis sd, high anchoring condition [heterogeneity]') |
      (write_df$id == pp & write_df$study == 'Study 4' & write_df$test == 'Variance analysis sd, low anchoring condition [heterogeneity]') |
      (write_df$id == pp & write_df$study == 'Study 4' & write_df$test == 'Variance analysis sd, high anchoring condition [heterogeneity]')],
      threshold = .05)[3]))
}

write.csv(write_df,
          '../data/study_01/study1_res.csv', row.names = FALSE)