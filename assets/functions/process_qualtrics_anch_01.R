if(!require(stringr)) install.packages('stringr')
library(stringr)

dat_qualtrics <- read.csv(file, stringsAsFactors = FALSE, na.strings = '')

# Remove the description of the question
dat_qualtrics <- dat_qualtrics[-1,]

# Remove the redundant variables
dat <- data.frame(id = dat_qualtrics$V1,
                  anch_1 = dat_qualtrics$res_01,
                  anch_2 = dat_qualtrics$res_02,
                  anch_3 = dat_qualtrics$res_03,
                  anch_4 = dat_qualtrics$res_04,
                  stat_know = dat_qualtrics$deb_02,
                  spss = dat_qualtrics$deb_03_1,
                  rcran = dat_qualtrics$deb_03_2,
                  stata = dat_qualtrics$deb_03_3,
                  sas = dat_qualtrics$deb_03_4,
                  matlab = dat_qualtrics$deb_03_5,
                  python = dat_qualtrics$deb_03_6,
                  other = dat_qualtrics$deb_03_7,
                  nopro = dat_qualtrics$deb_03_8,
                  noanch = ifelse(dat_qualtrics$deb_01 == 2,
                                  0,
                                  ifelse(dat_qualtrics$deb_01 == 1,
                                         1,
                                         NA)),
                  rng = ifelse(dat_qualtrics$deb_04 == 2,
                               0,
                               ifelse(dat_qualtrics$deb_04 == 1,
                                      1,
                                      NA)),
                  full = ifelse(dat_qualtrics$deb_05 == 2,
                                0,
                                ifelse(dat_qualtrics$deb_05 == 1,
                                       1,
                                       NA)),
                  tries = dat_qualtrics$deb_06,
                  descr = dat_qualtrics$deb_07,
                  bonus = dat_qualtrics$Q38,
                  stringsAsFactors = FALSE)

rm(dat_qualtrics)

# Remove incomplete responses
dat <- dat[!is.na(dat$anch_1) & 
             !is.na(dat$anch_2) &
             !is.na(dat$anch_3) &
             !is.na(dat$anch_4), ]

# Correct one respondent copy pasting entire spreadsheet
s1 <- str_match_all(string = dat$anch_1[30],
                    pattern = '[0-9]{1,}.?[0-9]{1,}.?[0-9]{2} [0-9]{1,}.?[0-9]{2}')[[1]]
s2 <- as.vector(s1)
dat$anch_1[30] <- paste(s2, collapse = ' / ')

s1 <- str_match_all(string = dat$anch_2[30],
                    pattern = '([0-9]{1,}.?){1,}[0-9]{1,}.?[0-9]{2} ([0-9]{1,}.?){1,}[0-9]{2}')[[1]][,1]
s2 <- as.vector(s1)
dat$anch_2[30] <- paste(s2, collapse = ' / ')

s1 <- str_match_all(string = dat$anch_3[30],
                    pattern = '([0-9]{1,}.?){1,}[0-9]{1,}.?[0-9]{2} ([0-9]{1,}.?){1,}[0-9]{2}')[[1]][,1]
s2 <- as.vector(s1)
dat$anch_3[30] <- paste(s2, collapse = ' / ')

s1 <- str_match_all(string = dat$anch_4[30],
                    pattern = '([0-9]{1,}.?){1,}[0-9]{1,}.?[0-9]{2} ([0-9]{1,}.?){1,}[0-9]{2}')[[1]][,1]
s2 <- as.vector(s1)
dat$anch_4[30] <- paste(s2, collapse = ' / ')

# Adjust one response based on email conversation
# Participant incorrectly specified results
# Emailed to correct, which was allowed.
# Documentation available upon request
dat$anch_4[dat$id == 'R_4Jf3zUJrydSIasB'] <-
  "9,811.23 999.52 / 9,950.33 1,013.22 / 25,327.30 4,523.63 / 26,109.03 5,231.93"

# Ensure variables are empty
referrer <- NULL
partgender <- NULL
anchoring <- NULL
anch_n <- NULL
anch_m <- NULL
anch_sd <- NULL
study <- NULL

id <- NULL
anch_1 <- NULL
anch_2 <- NULL
anch_3 <- NULL
anch_4 <- NULL
stat_know <- NULL
spss <- NULL
rcran <- NULL
stata <- NULL
sas <- NULL
matlab <- NULL
python <- NULL
other <- NULL
nopro <- NULL
noanch <- NULL
rng <- NULL
full <- NULL
tries <- NULL
descr <- NULL
bonus <- NULL


# Start loop row data
for (i in 1:dim(dat)[1])
{
  referrer <- c(referrer, rep(dat$id[i], 16))
  partgender <- c(partgender, rep(c(1, 0, 1, 0), 4))
  # 0 = male
  # 1 = female
  anchoring <- c(anchoring, rep(rep(c(0, 0, 1, 1), 4)))
  anch_n <- c(anch_n, rep(25, 16))
  study <- c(study, sort(rep(1:4, 4)))
  
  # Add the participant variables
  stat_know <- c(stat_know, rep(dat$stat_know[i], 16))
  spss <- c(spss, rep(dat$spss[i], 16))
  rcran <- c(rcran, rep(dat$rcran[i], 16))
  stata <- c(stata, rep(dat$stata[i], 16))
  sas <- c(sas, rep(dat$sas[i], 16))
  matlab <- c(matlab, rep(dat$matlab[i], 16))
  python <- c(python, rep(dat$python[i], 16))
  other <- c(other, rep(dat$other[i], 16))
  nopro <- c(nopro, rep(dat$nopro[i], 16))
  noanch <- c(noanch, rep(dat$noanch[i], 16))
  rng <- c(rng, rep(dat$rng[i], 16))
  full <- c(full, rep(dat$full[i], 16))
  tries <- c(tries, rep(dat$tries[i], 16))
  descr <- c(descr, rep(dat$descr[i], 16))
  bonus <- c(bonus, rep(dat$bonus[i], 16))
  
  
  # Ensure the results are split properly
  txt1 <- gsub(pattern = '\n', replacement = '', dat$anch_1[i])
  txt1 <- strsplit(x = txt1, split = ' / ')[[1]]
  txt1 <- strsplit(x = txt1, split = ' ')
  
  txt2 <- gsub(pattern = '\n', replacement = '', dat$anch_2[i])
  txt2 <- strsplit(x = txt2, split = ' / ')[[1]]
  txt2 <- strsplit(x = txt2, split = ' ')
  
  txt3 <- gsub(pattern = '\n', replacement = '', dat$anch_3[i])
  txt3 <- strsplit(x = txt3, split = ' / ')[[1]]
  txt3 <- strsplit(x = txt3, split = ' ')
  
  txt4 <- gsub(pattern = '\n', replacement = '', dat$anch_4[i])
  txt4 <- strsplit(x = txt4, split = ' / ')[[1]]
  txt4 <- strsplit(x = txt4, split = ' ')
  
  # Make vars to check for comma or point decimal
  x <- gregexpr(text = txt1[[1]][1], pattern = '\\.')[[1]][1]
  y <- nchar(txt1[[1]][1])
  
  # Get all the individual means and sds
  
  # If dec = .
  if (x == (y - 2))
  {
    anch_m <- c(anch_m, as.numeric(gsub(x = txt1[[1]][1], pattern = ',', replacement = '')))
    anch_sd <- c(anch_sd, as.numeric(gsub(x = txt1[[1]][length(txt1[[1]])], pattern = ',', replacement = '')))
    anch_m <- c(anch_m, as.numeric(gsub(x = txt1[[2]][1], pattern = ',', replacement = '')))
    anch_sd <- c(anch_sd, as.numeric(gsub(x = txt1[[2]][length(txt1[[2]])], pattern = ',', replacement = '')))
    anch_m <- c(anch_m, as.numeric(gsub(x = txt1[[3]][1], pattern = ',', replacement = '')))
    anch_sd <- c(anch_sd, as.numeric(gsub(x = txt1[[3]][length(txt1[[3]])], pattern = ',', replacement = '')))
    anch_m <- c(anch_m, as.numeric(gsub(x = txt1[[4]][1], pattern = ',', replacement = '')))
    anch_sd <- c(anch_sd, as.numeric(gsub(x = txt1[[4]][length(txt1[[4]])], pattern = ',', replacement = '')))
    
    anch_m <- c(anch_m, as.numeric(gsub(x = txt2[[1]][1], pattern = ',', replacement = '')))
    anch_sd <- c(anch_sd, as.numeric(gsub(x = txt2[[1]][length(txt2[[1]])], pattern = ',', replacement = '')))
    anch_m <- c(anch_m, as.numeric(gsub(x = txt2[[2]][1], pattern = ',', replacement = '')))
    anch_sd <- c(anch_sd, as.numeric(gsub(x = txt2[[2]][length(txt2[[2]])], pattern = ',', replacement = '')))
    anch_m <- c(anch_m, as.numeric(gsub(x = txt2[[3]][1], pattern = ',', replacement = '')))
    anch_sd <- c(anch_sd, as.numeric(gsub(x = txt2[[3]][length(txt2[[3]])], pattern = ',', replacement = '')))
    anch_m <- c(anch_m, as.numeric(gsub(x = txt2[[4]][1], pattern = ',', replacement = '')))
    anch_sd <- c(anch_sd, as.numeric(gsub(x = txt2[[4]][length(txt2[[4]])], pattern = ',', replacement = '')))
    
    anch_m <- c(anch_m, as.numeric(gsub(x = txt3[[1]][1], pattern = ',', replacement = '')))
    anch_sd <- c(anch_sd, as.numeric(gsub(x = txt3[[1]][length(txt3[[1]])], pattern = ',', replacement = '')))
    anch_m <- c(anch_m, as.numeric(gsub(x = txt3[[2]][1], pattern = ',', replacement = '')))
    anch_sd <- c(anch_sd, as.numeric(gsub(x = txt3[[2]][length(txt3[[2]])], pattern = ',', replacement = '')))
    anch_m <- c(anch_m, as.numeric(gsub(x = txt3[[3]][1], pattern = ',', replacement = '')))
    anch_sd <- c(anch_sd, as.numeric(gsub(x = txt3[[3]][length(txt3[[3]])], pattern = ',', replacement = '')))
    anch_m <- c(anch_m, as.numeric(gsub(x = txt3[[4]][1], pattern = ',', replacement = '')))
    anch_sd <- c(anch_sd, as.numeric(gsub(x = txt3[[4]][length(txt3[[4]])], pattern = ',', replacement = '')))
    
    anch_m <- c(anch_m, as.numeric(gsub(x = txt4[[1]][1], pattern = ',', replacement = '')))
    anch_sd <- c(anch_sd, as.numeric(gsub(x = txt4[[1]][length(txt4[[1]])], pattern = ',', replacement = '')))
    anch_m <- c(anch_m, as.numeric(gsub(x = txt4[[2]][1], pattern = ',', replacement = '')))
    anch_sd <- c(anch_sd, as.numeric(gsub(x = txt4[[2]][length(txt4[[2]])], pattern = ',', replacement = '')))
    anch_m <- c(anch_m, as.numeric(gsub(x = txt4[[3]][1], pattern = ',', replacement = '')))
    anch_sd <- c(anch_sd, as.numeric(gsub(x = txt4[[3]][length(txt4[[3]])], pattern = ',', replacement = '')))
    anch_m <- c(anch_m, as.numeric(gsub(x = txt4[[4]][1], pattern = ',', replacement = '')))
    anch_sd <- c(anch_sd, as.numeric(gsub(x = txt4[[4]][length(txt4[[4]])], pattern = ',', replacement = '')))
  } else # if dec = ,
  {
    s1 <- gsub(x = txt1[[1]][1], pattern = '\\.', replacement = '')
    s2 <- as.numeric(gsub(x= s1, pattern = ',', '.'))
    anch_m <- c(anch_m, s2)
    s1 <- gsub(x = txt1[[1]][length(txt1[[1]])], pattern = '\\.', replacement = '')
    s2 <- as.numeric(gsub(x= s1, pattern = ',', '.'))
    anch_sd <- c(anch_sd, s2)
    s1 <- gsub(x = txt1[[2]][1], pattern = '\\.', replacement = '')
    s2 <- as.numeric(gsub(x= s1, pattern = ',', '.'))
    anch_m <- c(anch_m, s2)
    s1 <- gsub(x = txt1[[2]][length(txt1[[2]])], pattern = '\\.', replacement = '')
    s2 <- as.numeric(gsub(x= s1, pattern = ',', '.'))
    anch_sd <- c(anch_sd, s2)
    s1 <- gsub(x = txt1[[3]][1], pattern = '\\.', replacement = '')
    s2 <- as.numeric(gsub(x= s1, pattern = ',', '.'))
    anch_m <- c(anch_m, s2)
    s1 <- gsub(x = txt1[[3]][length(txt1[[3]])], pattern = '\\.', replacement = '')
    s2 <- as.numeric(gsub(x= s1, pattern = ',', '.'))
    anch_sd <- c(anch_sd, s2)
    s1 <- gsub(x = txt1[[4]][1], pattern = '\\.', replacement = '')
    s2 <- as.numeric(gsub(x= s1, pattern = ',', '.'))
    anch_m <- c(anch_m, s2)
    s1 <- gsub(x = txt1[[4]][length(txt1[[4]])], pattern = '\\.', replacement = '')
    s2 <- as.numeric(gsub(x= s1, pattern = ',', '.'))
    anch_sd <- c(anch_sd, s2)
    
    s1 <- gsub(x = txt2[[1]][1], pattern = '\\.', replacement = '')
    s2 <- as.numeric(gsub(x= s1, pattern = ',', '.'))
    anch_m <- c(anch_m, s2)
    s1 <- gsub(x = txt2[[1]][length(txt2[[1]])], pattern = '\\.', replacement = '')
    s2 <- as.numeric(gsub(x= s1, pattern = ',', '.'))
    anch_sd <- c(anch_sd, s2)
    s1 <- gsub(x = txt2[[2]][1], pattern = '\\.', replacement = '')
    s2 <- as.numeric(gsub(x= s1, pattern = ',', '.'))
    anch_m <- c(anch_m, s2)
    s1 <- gsub(x = txt2[[2]][length(txt2[[2]])], pattern = '\\.', replacement = '')
    s2 <- as.numeric(gsub(x= s1, pattern = ',', '.'))
    anch_sd <- c(anch_sd, s2)
    s1 <- gsub(x = txt2[[3]][1], pattern = '\\.', replacement = '')
    s2 <- as.numeric(gsub(x= s1, pattern = ',', '.'))
    anch_m <- c(anch_m, s2)
    s1 <- gsub(x = txt2[[3]][length(txt2[[3]])], pattern = '\\.', replacement = '')
    s2 <- as.numeric(gsub(x= s1, pattern = ',', '.'))
    anch_sd <- c(anch_sd, s2)
    s1 <- gsub(x = txt2[[4]][1], pattern = '\\.', replacement = '')
    s2 <- as.numeric(gsub(x= s1, pattern = ',', '.'))
    anch_m <- c(anch_m, s2)
    s1 <- gsub(x = txt2[[4]][length(txt2[[4]])], pattern = '\\.', replacement = '')
    s2 <- as.numeric(gsub(x= s1, pattern = ',', '.'))
    anch_sd <- c(anch_sd, s2)
    
    s1 <- gsub(x = txt3[[1]][1], pattern = '\\.', replacement = '')
    s2 <- as.numeric(gsub(x= s1, pattern = ',', '.'))
    anch_m <- c(anch_m, s2)
    s1 <- gsub(x = txt3[[1]][length(txt3[[1]])], pattern = '\\.', replacement = '')
    s2 <- as.numeric(gsub(x= s1, pattern = ',', '.'))
    anch_sd <- c(anch_sd, s2)
    s1 <- gsub(x = txt3[[2]][1], pattern = '\\.', replacement = '')
    s2 <- as.numeric(gsub(x= s1, pattern = ',', '.'))
    anch_m <- c(anch_m, s2)
    s1 <- gsub(x = txt3[[2]][length(txt3[[2]])], pattern = '\\.', replacement = '')
    s2 <- as.numeric(gsub(x= s1, pattern = ',', '.'))
    anch_sd <- c(anch_sd, s2)
    s1 <- gsub(x = txt3[[3]][1], pattern = '\\.', replacement = '')
    s2 <- as.numeric(gsub(x= s1, pattern = ',', '.'))
    anch_m <- c(anch_m, s2)
    s1 <- gsub(x = txt3[[3]][length(txt3[[3]])], pattern = '\\.', replacement = '')
    s2 <- as.numeric(gsub(x= s1, pattern = ',', '.'))
    anch_sd <- c(anch_sd, s2)
    s1 <- gsub(x = txt3[[4]][1], pattern = '\\.', replacement = '')
    s2 <- as.numeric(gsub(x= s1, pattern = ',', '.'))
    anch_m <- c(anch_m, s2)
    s1 <- gsub(x = txt3[[4]][length(txt3[[4]])], pattern = '\\.', replacement = '')
    s2 <- as.numeric(gsub(x= s1, pattern = ',', '.'))
    anch_sd <- c(anch_sd, s2)
    
    s1 <- gsub(x = txt4[[1]][1], pattern = '\\.', replacement = '')
    s2 <- as.numeric(gsub(x= s1, pattern = ',', '.'))
    anch_m <- c(anch_m, s2)
    s1 <- gsub(x = txt4[[1]][length(txt4[[1]])], pattern = '\\.', replacement = '')
    s2 <- as.numeric(gsub(x= s1, pattern = ',', '.'))
    anch_sd <- c(anch_sd, s2)
    s1 <- gsub(x = txt4[[2]][1], pattern = '\\.', replacement = '')
    s2 <- as.numeric(gsub(x= s1, pattern = ',', '.'))
    anch_m <- c(anch_m, s2)
    s1 <- gsub(x = txt4[[2]][length(txt4[[2]])], pattern = '\\.', replacement = '')
    s2 <- as.numeric(gsub(x= s1, pattern = ',', '.'))
    anch_sd <- c(anch_sd, s2)
    s1 <- gsub(x = txt4[[3]][1], pattern = '\\.', replacement = '')
    s2 <- as.numeric(gsub(x= s1, pattern = ',', '.'))
    anch_m <- c(anch_m, s2)
    s1 <- gsub(x = txt4[[3]][length(txt4[[3]])], pattern = '\\.', replacement = '')
    s2 <- as.numeric(gsub(x= s1, pattern = ',', '.'))
    anch_sd <- c(anch_sd, s2)
    s1 <- gsub(x = txt4[[4]][1], pattern = '\\.', replacement = '')
    s2 <- as.numeric(gsub(x= s1, pattern = ',', '.'))
    anch_m <- c(anch_m, s2)
    s1 <- gsub(x = txt4[[4]][length(txt4[[4]])], pattern = '\\.', replacement = '')
    s2 <- as.numeric(gsub(x= s1, pattern = ',', '.'))
    anch_sd <- c(anch_sd, s2)
  }
}

res <- data.frame(referrer,
                  partgender,
                  anchoring,
                  anch_n, 
                  anch_m, 
                  anch_sd, 
                  study,
                  stat_know,
                  spss,
                  rcran,
                  stata,
                  sas,
                  matlab,
                  python,
                  other,
                  nopro,
                  noanch,
                  rng,
                  full,
                  tries,
                  descr,
                  bonus)

write.csv(res, res_file, row.names = FALSE)
