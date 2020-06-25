## scoring for MINI

# for this code to work 
# "yes" answers must download as "2" and "no" must download as "1"
# this code is for a shortened version of the MINI
# so there may be a few categories missing if you use the entire MINI

# change to your csv name

df <- read.csv('miniinsta.csv')

View(df)

library(dplyr)
library(data.cube)

## delete rows for test cases

df<-df[!(df$Q1=="test"),]

## ONLY RUN ONCE drop first two rows which are just extra col descriptors
# aka since our first participant is row number 3, this puts them at row number 1

df = df[-1,]
df = df[-1,]

## create new columns for yes/no diagnoses

df <- mutate(df,curdepression = NA)
df <- mutate(df,curmanic = NA)
df <- mutate(df,pastmanic = NA)
df <- mutate(df,curpanic = NA)
df <- mutate(df,curagoraphobia = NA)
df <- mutate(df,cursocialanx = NA)
df <- mutate(df,curocd = NA)
df <- mutate(df,curptsd = NA)
df <- mutate(df,curalc = NA)
df <- mutate(df,curstim = NA)
df <- mutate(df,curcoc = NA)
df <- mutate(df,curopi = NA)
df <- mutate(df,curhal = NA)
df <- mutate(df,curdisd = NA)
df <- mutate(df,curinh = NA)
df <- mutate(df,curcan = NA)
df <- mutate(df,curtranq = NA)
df <- mutate(df,curmisc = NA)
df <- mutate(df,curpsychotic = NA)
df <- mutate(df,curanorexia = NA)
df <- mutate(df,curbulimia = NA)
df <- mutate(df,curbinge = NA)
df <- mutate(df,curanx = NA)

## assess current depression

# first make values numeric

sapply(19:26, function(i) {
  df[,i] <<- as.numeric(as.character(df[,i]))
})

# then for each row if certain conditions are met
# they are marked as having depression or not by 1/0

for (i in 1:dim(df)[1]) {
  if ((df$A1b[i]==2 | df$A2b[i]==2) & 
      ((df$A1b[i] + df$A2b[i] + df$a3_1[i] + df$a3_2[i] + df$a3_3[i] + df$a3_4[i] 
        + df$a3_5[i] + df$a3_6[i]) >= 13) & df$A4c[i]==2) {
    df$curdepression[i] <- '1'
    } else {
    df$curdepression[i] <- '0'
    }
}

## assess current mania

# first make numeric

sapply(29:43, function(i) {
  df[,i] <<- as.numeric(as.character(df[,i]))
})

sapply(226:241, function(i) {
  df[,i] <<- as.numeric(as.character(df[,i]))
})

# this creates a sum of some items that we will need in the "if" statement below

sumcurmania <- rowSums(df[33:39],na.rm = TRUE)

# this turns NA values into 0 because otherwise code logic won't work

df[,29:43][is.na(df[,29:43])] <- 0
df[,226:241][is.na(df[,226:241])] <- 0

# this is the code that determines diagnosis or not

for (i in 1:dim(df)[1]) {
  if ((df$c1a[i]==2 | df$c2a[i]==2) & 
      ((df$c1b[i]==1 & (sumcurmania[i] >= 11)) | 
      (df$c1b[i]==2 & (sumcurmania[i] >= 10))) & df$c5_3[i]==2 &
      (df$c4[i]==3 | df$c5_1[i]==2 | df$c5_2[i]==2 | df$k1a[i]==2 | df$k1b[i]==2 | 
       df$k2a[i]==2 | df$k2b[i]==2 | df$k3a[i]==2 | df$k3b[i]==2 | df$k4a[i]==2 | 
       df$k4b[i]==2 | df$k5a[i]==2 | df$k5b[i]==2 | df$k6a[i]==2 | df$k6a2[i]==2 | 
       df$k6b[i]==2 | df$k6b2[i]==2 | df$k7a[i]==2 | df$k7b[i]==2)) {
    df$curmanic[i] <- '1'
  } else {
    df$curmanic[i] <- '0'
  }
}

## assess past mania

# make numeric

sapply(44:54, function(i) {
  df[,i] <<- as.numeric(as.character(df[,i]))
})

# create a sum that we will need in "if else" code block

sumpastmania <- rowSums(df[44:50],na.rm = TRUE)

# remove NAs

df[,44:50][is.na(df[,44:50])] <- 0

# determine past mania presence

for (i in 1:dim(df)[1]) {
  if ((df$c1a[i]==2 | df$c2a[i]==2) & 
      ((df$c1a[i]==1 & (sumpastmania[i] >= 11)) | 
       (df$c1a[i]==2 & (sumpastmania[i] >= 10))) & df$Q272_3[i]==2 &
      (df$Q271[i]==3 | df$Q272_1[i]==2 | df$Q272_2[i]==2 | df$k1a[i]==2 | df$k1b[i]==2 | 
       df$k2a[i]==2 | df$k2b[i]==2 | df$k3a[i]==2 | df$k3b[i]==2 | df$k4a[i]==2 | 
       df$k4b[i]==2 | df$k5a[i]==2 | df$k5b[i]==2 | df$k6a[i]==2 | df$k6a2[i]==2 | 
       df$k6b[i]==2 | df$k6b2[i]==2 | df$k7a[i]==2 | df$k7b[i]==2)) {
    df$pastmanic[i] <- '1'
  } else {
    df$pastmanic[i] <- '0'
  }
}

## assess current panic

# make numeric

sapply(55:72, function(i) {
  df[,i] <<- as.numeric(as.character(df[,i]))
})

# remove NAs

df[,55:72][is.na(df[,55:72])] <- 0

# create a sum that we will need in "if else" code block

sumcurpanic <- rowSums(df[59:71],na.rm = TRUE)

# determine panic disorder presence

for (i in 1:dim(df)[1]) {
  if ((df$d1a[i]==2 & df$d1b[i]==2 & df$d2[i]==2 & df$d3[i]==2 & df$d6[i]==2) & 
    (sumcurpanic[i] >= 17)) {
    df$curpanic[i] <- '1'
  } else {
    df$curpanic[i] <- '0'
  }
}

## assess current agoraphobia

# make numeric

sapply(73:78, function(i) {
  df[,i] <<- as.numeric(as.character(df[,i]))
})

# remove NAs

df[,73:78][is.na(df[,73:78])] <- 0

# determine agoraphobia presence

for (i in 1:dim(df)[1]) {
  if (df$e1[i]==2 & df$e2[i]==2 & df$e3[i]==2 & df$e4[i]==2 & df$e5[i]==2 & df$e6[i]==2) {
    df$curagoraphobia[i] <- '1'
  } else {
    df$curagoraphobia[i] <- '0'
  }
}

## assess current social anxiety

# make numeric

sapply(79:84, function(i) {
  df[,i] <<- as.numeric(as.character(df[,i]))
})

# remove NAs

df[,79:84][is.na(df[,79:84])] <- 0

# determine social anxiety presence

for (i in 1:dim(df)[1]) {
  if (df$f1[i]==2 & df$f2[i]==2 & df$f3[i]==2 & df$f4[i]==2 & df$f5[i]==2 & df$f6[i]==2) {
    df$cursocialanx[i] <- '1'
  } else {
    df$cursocialanx[i] <- '0'
  }
}

## assess ocd

# make numeric

sapply(85:90, function(i) {
  df[,i] <<- as.numeric(as.character(df[,i]))
})

# remove NAs

df[,85:90][is.na(df[,85:90])] <- 0

# determine ocd presence

for (i in 1:dim(df)[1]) {
  if (((df$g1a[i]==2 & df$g1b[i]==2 & df$g2[i]==2) | (df$g3a[i]==2 & df$g3b[i]==2)) & 
      df$g4[i]==2) {
    df$curocd[i] <- '1'
  } else {
    df$curocd[i] <- '0'
  }
}

## assess ptsd

# make numeric

sapply(91:109, function(i) {
  df[,i] <<- as.numeric(as.character(df[,i]))
})

# remove NAs

df[,91:109][is.na(df[,91:109])] <- 0

# create sums that we will need in "if else" code block

sumh4 <- rowSums(df[95:101],na.rm = TRUE)

sumh5 <- rowSums(df[102:107],na.rm = TRUE)

# determine ptsd presence

for (i in 1:dim(df)[1]) {
  if ((df$h1[i]==2 & df$h2[i]==2) & (df$h3_1[i] + df$h3_2[i] >= 3) & 
      (sumh4[i] >= 9) & (sumh5[i] >= 8) & df$h7[i]==2) {
    df$curptsd[i] <- '1'
  } else {
    df$curptsd[i] <- '0'
  }
}

## assess alcohol 

# convert i2k1 to total count so we can score alc module

sapply(121, function(i) {
  df[,i] <<- as.character(df[,i])
})

for (i in 1:dim(df)[1]) {
  df$i2k1[i] <- length(strsplit(df$i2k1[i], ',')[[1]])
}

# make numeric 

sapply(110:122, function(i) {
  df[,i] <<- as.numeric(as.character(df[,i]))
})

# remove NAs

df[,110:122][is.na(df[,110:122])] <- 0

# create sum that we will need in "if else" code block

sumcuralc <- rowSums(df[111:120],na.rm = TRUE)

# determine alcohol disorder presence

for (i in 1:dim(df)[1]) {
  if (df$i1[i]==2 & sumcuralc[i]>=12 & (df$i2k1[i]>=2 | df$i2k2[i]==2)) {
    df$curalc[i] <- '1'
  } else {
    df$curalc[i] <- '0'
  }
}

## assess stimulants

# convert j2sk1 to total count so we can score stim module

sapply(224, function(i) {
  df[,i] <<- as.character(df[,i])
})

for (i in 1:dim(df)[1]) {
  df$j2sk1[i] <- length(strsplit(df$j2sk1[i], ',')[[1]])
}

# make numeric 

sapply(214:225, function(i) {
  df[,i] <<- as.numeric(as.character(df[,i]))
})

# remove NAs

df[,214:225][is.na(df[,214:225])] <- 0

# create j2k summary

j2kssum <- rep(NA, 199)

for (i in 1:dim(df)[1]) {
  if (df$j2sk1[i]>=2 | df$j2sk2[i]==2) {
    j2kssum[i] <- '1'
  } else {
    j2kssum[i] <- '0'
  }
}

j2kssum <- as.numeric(j2kssum)

# create sum that we will need in "if else" code block

sumstim <- (df$j2s_1 + df$j2s_2 + df$j2s_3 + df$j2s_4 + df$j2s_5 + df$j2s_6 +
              df$j2s_7 + df$j2s_8 + df$j2s_9 + df$j2s_10 + j2kssum)

# determine stimulant use disorder presence

for (i in 1:dim(df)[1]) {
  if (sumstim[i] >= 13) {
    df$curstim[i] <- '1'
  } else {
    df$curstim[i] <- '0'
  }
}

## assess cocaine

# convert j2ck1 to total count so we can score cocaine module

sapply(212, function(i) {
  df[,i] <<- as.character(df[,i])
})

for (i in 1:dim(df)[1]) {
  df$j2ck1[i] <- length(strsplit(df$j2ck1[i], ',')[[1]])
}

# make numeric 

sapply(202:213, function(i) {
  df[,i] <<- as.numeric(as.character(df[,i]))
})

# remove NAs

df[,202:213][is.na(df[,202:213])] <- 0

# create j2k summary

j2kcsum <- rep(NA, 199)

for (i in 1:dim(df)[1]) {
  if (df$j2ck1[i]>=2 | df$j2ck2[i]==2) {
    j2kcsum[i] <- '1'
  } else {
    j2kcsum[i] <- '0'
  }
}

j2kcsum <- as.numeric(j2kcsum)

# create sum that we will need in "if else" code block

sumcoc <- (df$j2c_1 + df$j2c_2 + df$j2c_3 + df$j2c_4 + df$j2c_5 + df$j2c_6 +
              df$j2c_7 + df$j2c_8 + df$j2c_9 + df$j2c_10 + j2kcsum)

# determine cocaine use disorder presence

for (i in 1:dim(df)[1]) {
  if (sumcoc[i] >= 13) {
    df$curcoc[i] <- '1'
  } else {
    df$curcoc[i] <- '0'
  }
}

## assess opiates

# convert j2ok1 to total count so we can score opiate module

sapply(200, function(i) {
  df[,i] <<- as.character(df[,i])
})

for (i in 1:dim(df)[1]) {
  df$j2ok1[i] <- length(strsplit(df$j2ok1[i], ',')[[1]])
}

# make numeric 

sapply(190:201, function(i) {
  df[,i] <<- as.numeric(as.character(df[,i]))
})

# remove NAs

df[,190:201][is.na(df[190:201])] <- 0

# create j2k summary

j2kosum <- rep(NA, 199)

for (i in 1:dim(df)[1]) {
  if (df$j2ok1[i]>=3 | df$j2ok2[i]==2) {
    j2kosum[i] <- '1'
  } else {
    j2kosum[i] <- '0'
  }
}

j2kosum <- as.numeric(j2kosum)

# create sum that we will need in "if else" code block

sumopi <- (df$j2o_1 + df$j2o_2 + df$j2o_3 + df$j2o_4 + df$j2o_5 + df$j2o_6 +
             df$j2o_7 + df$j2o_8 + df$j2o_9 + df$j2o_10 + j2kosum)

# determine opiate use disorder presence

for (i in 1:dim(df)[1]) {
  if (sumopi[i] >= 13) {
    df$curopi[i] <- '1'
  } else {
    df$curopi[i] <- '0'
  }
}

## assess hallucinogens

# hallucinogens dont have withdrawal symptoms so we can skip some things here

# make numeric 

sapply(180:189, function(i) {
  df[,i] <<- as.numeric(as.character(df[,i]))
})

# remove NAs

df[,180:189][is.na(df[180:189])] <- 0

# create sum that we will need in "if else" code block

sumhal <- (df$j2h_1 + df$j2h_2 + df$j2h_3 + df$j2h_4 + df$j2h_5 + df$j2h_6 +
             df$j2h_7 + df$j2h_8 + df$j2h_9 + df$j2h_10)

# determine hallucinogen use disorder presence

for (i in 1:dim(df)[1]) {
  if (sumhal[i] >= 12) {
    df$curhal[i] <- '1'
  } else {
    df$curhal[i] <- '0'
  }
}

## assess dissociative drugs

# dissociatives dont have withdrawal symptoms so we can skip some things here

# make numeric 

sapply(170:179, function(i) {
  df[,i] <<- as.numeric(as.character(df[,i]))
})

# remove NAs

df[,170:179][is.na(df[170:179])] <- 0

# create sum that we will need in "if else" code block

sumdis <- (df$j2d_1 + df$j2d_2 + df$j2d_3 + df$j2d_4 + df$j2d_5 + df$j2d_6 +
             df$j2d_7 + df$j2d_8 + df$j2d_9 + df$j2d_10)

# determine dissociative drug use disorder presence

for (i in 1:dim(df)[1]) {
  if (sumdis[i] >= 12) {
    df$curdisd[i] <- '1'
  } else {
    df$curdisd[i] <- '0'
  }
}

## assess inhalant 

# inhalants dont have withdrawal symptoms so we can skip some things here

# make numeric 

sapply(160:169, function(i) {
  df[,i] <<- as.numeric(as.character(df[,i]))
})

# remove NAs

df[,160:169][is.na(df[160:169])] <- 0

# create sum that we will need in "if else" code block

suminh <- (df$j2i_1 + df$j2i_2 + df$j2i_3 + df$j2i_4 + df$j2i_5 + df$j2i_6 +
             df$j2i_7 + df$j2i_8 + df$j2i_9 + df$j2i_10)

# determine inhalant use disorder presence

for (i in 1:dim(df)[1]) {
  if (suminh[i] >= 12) {
    df$curinh[i] <- '1'
  } else {
    df$curinh[i] <- '0'
  }
}

## assess cannabis

# convert j2cak1 to total count so we can score cannabis module

sapply(158, function(i) {
  df[,i] <<- as.character(df[,i])
})

for (i in 1:dim(df)[1]) {
  df$j2cak1[i] <- length(strsplit(df$j2cak1[i], ',')[[1]])
}

# make numeric 

sapply(148:159, function(i) {
  df[,i] <<- as.numeric(as.character(df[,i]))
})

# remove NAs

df[,148:159][is.na(df[148:159])] <- 0

# create j2k summary

j2kcasum <- rep(NA, 199)

for (i in 1:dim(df)[1]) {
  if (df$j2cak1[i]>=3 | df$j2cak2[i]==2) {
    j2kcasum[i] <- '1'
  } else {
    j2kcasum[i] <- '0'
  }
}

j2kcasum <- as.numeric(j2kcasum)

# create sum that we will need in "if else" code block

sumcan <- (df$j2ca_1 + df$j2ca_2 + df$j2ca_3 + df$j2ca_4 + df$j2ca_5 + df$j2ca_6 +
             df$j2ca_7 + df$j2ca_8 + df$j2ca_9 + df$j2ca_10 + j2kcasum)

# determine cannabis use disorder presence

for (i in 1:dim(df)[1]) {
  if (sumcan[i] >= 13) {
    df$curcan[i] <- '1'
  } else {
    df$curcan[i] <- '0'
  }
}

## assess tranquilizers

# convert j2tk1 to total count so we can score cannabis module

sapply(146, function(i) {
  df[,i] <<- as.character(df[,i])
})

for (i in 1:dim(df)[1]) {
  df$j2tk1[i] <- length(strsplit(df$j2tk1[i], ',')[[1]])
}

# make numeric 

sapply(136:147, function(i) {
  df[,i] <<- as.numeric(as.character(df[,i]))
})

# remove NAs

df[,136:147][is.na(df[136:147])] <- 0

# create j2k summary

j2ktsum <- rep(NA, 199)

for (i in 1:dim(df)[1]) {
  if (df$j2tk1[i]>=2 | df$j2tk2[i]==2) {
    j2ktsum[i] <- '1'
  } else {
    j2ktsum[i] <- '0'
  }
}

j2ktsum <- as.numeric(j2ktsum)

# create sum that we will need in "if else" code block

sumtran <- (df$j2t_1 + df$j2t_2 + df$j2t_3 + df$j2t_4 + df$j2t_5 + df$j2t_6 +
             df$j2t_7 + df$j2t_8 + df$j2t_9 + df$j2t_10 + j2ktsum)

# determine tranquilizer use disorder presence

for (i in 1:dim(df)[1]) {
  if (sumtran[i] >= 13) {
    df$curtranq[i] <- '1'
  } else {
    df$curtranq[i] <- '0'
  }
}

## assess misc 

# misc doesnt have withdrawal symptoms so we can skip some things here

# make numeric 

sapply(126:135, function(i) {
  df[,i] <<- as.numeric(as.character(df[,i]))
})

# remove NAs

df[,126:135][is.na(df[126:135])] <- 0

# create sum that we will need in "if else" code block

summisc <- (df$j2m_1 + df$j2m_2 + df$j2m_3 + df$j2m_4 + df$j2m_5 + df$j2m_6 +
             df$j2m_7 + df$j2m_8 + df$j2m_9 + df$j2m_10)

# determine misc use disorder presence

for (i in 1:dim(df)[1]) {
  if (summisc[i] >= 12) {
    df$curmisc[i] <- '1'
  } else {
    df$curmisc[i] <- '0'
  }
}

# done with substance use assessment! 

## assess psychoticism

for (i in 1:dim(df)[1]) {
  if (df$k1b[i]==2 | df$k2b[i]==2 | df$k3b[i]==2 | df$k4b[i]==2 | df$k5b[i]==2 | 
      df$k6b[i]==2 | df$k7b[i]==2) {
    df$curpsychotic[i] <- '1'
  } else {
    df$curpsychotic[i] <- '0'
  }
}

## assess anorexia 

# first make numeric 

sapply(262:266, function(i) {
  df[,i] <<- as.numeric(as.character(df[,i]))
})

# remove NAs

df[,262:266][is.na(df[262:266])] <- 0

# determine anorexia presence

for (i in 1:dim(df)[1]) {
  if ((df$l2.4_1[i]==2 & df$l2.4_2[i]==2) & 
      (df$l2.4_3[i]==2 | df$l2.4_4[i]==2 | df$l2.4_5[i]==2)) {
    df$curanorexia[i] <- '1'
  } else {
    df$curanorexia[i] <- '0'
  }
}

## assess bulimia 

# first make numeric 

sapply(273:274, function(i) {
  df[,i] <<- as.numeric(as.character(df[,i]))
})

# remove NAs

df[,273:274][is.na(df[273:274])] <- 0

# determine bulimia presence

for (i in 1:dim(df)[1]) {
  if (df$m4[i]==2 & df$m5[i]==2 & (df$m7[i]==1 | df$curanorexia[i]==0)) {
    df$curbulimia[i] <- '1'
  } else {
    df$curbulimia[i] <- '0'
  }
}

## assess binge eating

# first make numeric

sapply(275:280, function(i) {
  df[,i] <<- as.numeric(as.character(df[,i]))
})

# remove NAs

df[,275:280][is.na(df[,275:280])] <- 0

# create a sum that we will need in "if else" code block

sumbinge <- df$mb6_1 + df$mb6_2 + df$mb6_3 + df$mb6_4 + df$mb6_5

# determine binge eating presence

for (i in 1:dim(df)[1]) {
  if (df$curanorexia[i]==0 & df$curbulimia[i]==0 & df$m2[i]==2 & df$m3[i]==1 & 
      df$m4[i]==2 & (sumbinge[i] >= 8) & df$mb6_6[i]==2) {
    df$curbinge[i] <- '1'
  } else {
    df$curbinge[i] <- '0'
  }
}

## assess anxiety

# first make numeric

sapply(283:292, function(i) {
  df[,i] <<- as.numeric(as.character(df[,i]))
})

# remove NAs

df[,283:292][is.na(df[,283:292])] <- 0

# create a sum that we will need in "if else" code block

sumanx <- df$n3_1 + df$n3_2 + df$n3_3 + df$n3_4 + df$n3_5 + df$n3_6

# determine binge eating presence

for (i in 1:dim(df)[1]) {
  if (sumanx[i] >= 9 & df$n4[i]==2) {
    df$curanx[i] <- '1'
  } else {
    df$curanx[i] <- '0'
  }
}

# woo! we did it. time to write a new csv with our scores. 

write.csv(df, 'miniscored.csv')
