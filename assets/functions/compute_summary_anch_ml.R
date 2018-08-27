if(!require(foreign)){install.packages('foreign')}
if(!require(car)){install.packages('car')}
if(!require(plyr)){install.packages('plyr')}
library(foreign)
library(car)
library(plyr)

# GO
ml_dat <- read.spss(ml_dat_file,
                    to.data.frame = TRUE,
                    use.value.labels = FALSE)

# Correct the gender variable to see 1 as NA
# partgender = 1 described as no response in ML syntax
ml_dat$partgender <- ifelse(ml_dat$partgender == 1, NA,
                            ifelse(ml_dat$partgender == 3, 0, 1))
# 0 = male
# 1 = female

# listwise deletion of participants without gender
ml_dat <- ml_dat[!is.na(ml_dat$partgender), ]

summary_stat_1 <- ddply(ml_dat, .(referrer, anch1group, partgender), function(x){
  anch1_n <- length(x$anchoring1[!is.na(x$anchoring1)])
  anch1_m <- mean(x$anchoring1, na.rm = TRUE)
  anch1_sd <- sd(x$anchoring1, na.rm = TRUE)
  study <- 1
  
  return(cbind(anch1_n,
               anch1_m,
               anch1_sd,
               study))
})
names(summary_stat_1)[c(2,4:6)] <- c('anchoring',
                                'anch_n',
                                'anch_m',
                                'anch_sd')

summary_stat_2 <- ddply(ml_dat, .(referrer, anch2group, partgender), function(x){
  anch2_n <- length(x$anchoring2[!is.na(x$anchoring2)])
  anch2_m <- mean(x$anchoring2, na.rm = TRUE)
  anch2_sd <- sd(x$anchoring2, na.rm = TRUE)
  study <- 2
  
  return(cbind(anch2_n,
               anch2_m,
               anch2_sd,
               study))
})
names(summary_stat_2)[c(2,4:6)] <- c('anchoring',
                                'anch_n',
                                'anch_m',
                                'anch_sd')

summary_stat_3 <- ddply(ml_dat, .(referrer, anch3group, partgender), function(x){
  anch3_n <- length(x$anchoring3[!is.na(x$anchoring3)])
  anch3_m <- mean(x$anchoring3, na.rm = TRUE)
  anch3_sd <- sd(x$anchoring3, na.rm = TRUE)
  study <- 3
  
  return(cbind(anch3_n,
               anch3_m,
               anch3_sd,
               study))
})
names(summary_stat_3)[c(2,4:6)] <- c('anchoring',
                                'anch_n',
                                'anch_m',
                                'anch_sd')

summary_stat_4 <- ddply(ml_dat, .(referrer, anch4group, partgender), function(x){
  anch4_n <- length(x$anchoring4[!is.na(x$anchoring4)])
  anch4_m <- mean(x$anchoring4, na.rm = TRUE)
  anch4_sd <- sd(x$anchoring4, na.rm = TRUE)
  study <- 4
  
  return(cbind(anch4_n,
               anch4_m,
               anch4_sd,
               study))
})
names(summary_stat_4)[c(2,4:6)] <- c('anchoring',
                                'anch_n',
                                'anch_m',
                                'anch_sd')

summary_stat <- rbind(summary_stat_1,
                      summary_stat_2,
                      summary_stat_3,
                      summary_stat_4)
summary_stat <- summary_stat[,c('referrer', 'partgender', 'anchoring', 'anch_n', 'anch_m', 'anch_sd', 'study')]


write.csv(summary_stat,
          summary_stat_file,
          row.names = FALSE)

refer <- unique(ml_dat$referrer)

gender_p_1 <- NULL
inter_p_1 <- NULL
gender_p_2 <- NULL
inter_p_2 <- NULL
gender_p_3 <- NULL
inter_p_3 <- NULL
gender_p_4 <- NULL
inter_p_4 <- NULL

for (i in 1:length(refer)){
  # Anchoring study 1
  mod <- lm(anchoring1 ~ as.factor(anch1group) +
              as.factor(partgender) +
              as.factor(anch1group) * as.factor(partgender), data = ml_dat[ml_dat$referrer == refer[i],])
  anova_mod <- Anova(mod)
  
  gender_p_1 <- c(gender_p_1, anova_mod$`Pr(>F)`[2])
  inter_p_1 <-c(inter_p_1, anova_mod$`Pr(>F)`[3])
  
  # Anchoring study 2
  mod <- lm(anchoring1 ~ as.factor(anch2group) +
              as.factor(partgender) +
              as.factor(anch2group) * as.factor(partgender), data = ml_dat[ml_dat$referrer == refer[i],])
  anova_mod <- Anova(mod)
  
  gender_p_2 <- c(gender_p_2, anova_mod$`Pr(>F)`[2])
  inter_p_2 <-c(inter_p_2, anova_mod$`Pr(>F)`[3])
  
  # Anchoring study 3
  mod <- lm(anchoring1 ~ as.factor(anch3group) +
              as.factor(partgender) +
              as.factor(anch3group) * as.factor(partgender), data = ml_dat[ml_dat$referrer == refer[i],])
  anova_mod <- Anova(mod)
  
  gender_p_3 <- c(gender_p_3, anova_mod$`Pr(>F)`[2])
  inter_p_3 <-c(inter_p_3, anova_mod$`Pr(>F)`[3])
  
  # Anchoring study 4
  mod <- lm(anchoring1 ~ as.factor(anch4group) +
              as.factor(partgender) +
              as.factor(anch4group) * as.factor(partgender), data = ml_dat[ml_dat$referrer == refer[i],])
  anova_mod <- Anova(mod)
  
  gender_p_4 <- c(gender_p_4, anova_mod$`Pr(>F)`[2])
  inter_p_4 <-c(inter_p_4, anova_mod$`Pr(>F)`[3])
}

chi2 <- -2*sum(log(inter_p_1))
degr_f <- 2*length(inter_p_1)
pval <- 1 - pchisq(-2*sum(log(inter_p_1)), df = 2*length(inter_p_1))
sprintf('chi2(%s)=%s, p=%s', degr_f, chi2, pval)

chi2 <- -2*sum(log(inter_p_2))
degr_f <- 2*length(inter_p_2)
pval <- 1 - pchisq(-2*sum(log(inter_p_2)), df = 2*length(inter_p_2))
sprintf('chi2(%s)=%s, p=%s', degr_f, chi2, pval)

chi2 <- -2*sum(log(inter_p_3))
degr_f <- 2*length(inter_p_3)
pval <- 1 - pchisq(-2*sum(log(inter_p_3)), df = 2*length(inter_p_3))
sprintf('chi2(%s)=%s, p=%s', degr_f, chi2, pval)

chi2 <- -2*sum(log(inter_p_4))
degr_f <- 2*length(inter_p_4)
pval <- 1 - pchisq(-2*sum(log(inter_p_4)), df = 2*length(inter_p_4))
sprintf('chi2(%s)=%s, p=%s', degr_f, chi2, pval)

chi2 <- -2*sum(log(c(inter_p_1,
                     inter_p_2,
                     inter_p_3,
                     inter_p_4)))
degr_f <- 2*length(c(inter_p_1,
                     inter_p_2,
                     inter_p_3,
                     inter_p_4))
pval <- 1 - pchisq(-2*sum(log(c(inter_p_1,
                                inter_p_2,
                                inter_p_3,
                                inter_p_4))), df = 2*(length(inter_p_1) +
                                                        length(inter_p_2) +
                                                        length(inter_p_3) +
                                                        length(inter_p_4)))
sprintf('chi2(%s)=%s, p=%s', degr_f, chi2, pval)