## scoring for the PSQI (sleep quality assessment)
# this code was written with the help of Hannah Bosley at UC Berkeley
# and Arjun Savel at U Maryland

# to populate later

df <- mutate(df,psqitotal = NA)

# change to your own column indexes
# you can use which(colnames(df)=='yourcolname') to figure out the right index

psqi <- df[,439:457]

# change NAs to 0 for q14 so we can compute totals

for(i in 1:nrow(psqi)){
  if (is.na(psqi$psqi14[i])){
    psqi$psqi14[i] = 0
  }
}

# only include complete cases

psqi = psqi[complete.cases(psqi),] 

# calculate each dimension of PSQI

# calculate duration

for(i in 1:nrow(psqi)){
  if (psqi[i,4] >= 7){
    psqi$durat[i] = 0
  } else if (psqi[i,4] < 7 && psqi[i,4] >= 6){
    psqi$durat[i] = 1
  } else if (psqi[i,4] < 6 && psqi[i,4] >= 5){
    psqi$durat[i] = 2
  }else if (psqi[i,4] < 5){
    psqi$durat[i] = 3
  }
}

# calculate disturbance

for(i in 1:nrow(psqi)){
  if ((sum(psqi[i,6:14])) == 0){
    psqi$disturb[i] = 0
  } else if ((sum(psqi[i,6:14]) >= 1 && (sum(psqi[i,6:14]) <= 9))){
    psqi$disturb[i] = 1
  } else if (sum(psqi[i,6:14]) > 9 && (sum(psqi[i,6:14]) <= 18)){
    psqi$disturb[i] = 2
  }else if (sum(psqi[i,6:14]) > 18){
    psqi$disturb[i] = 3
  }
}

# calculate latency

for(i in 1:nrow(psqi)){
  if (psqi[i,2] >= 0 && psqi[i,2] <=15){
    psqi$item2new[i] = 0
  } else if (psqi[i,2] > 15 && psqi[i,2] <= 30){
    psqi$item2new[i] = 1
  } else if (psqi[i,2] > 30 && psqi[i,2] <= 60){
    psqi$item2new[i] = 2
  } else if (psqi[i,2] > 60){
    psqi$item2new[i] = 3
  }
}

for(i in 1:nrow(psqi)){
  if ((psqi[i,5] + psqi$item2new[i]) == 0){
    psqi$latency[i] = 0
  } else if ((psqi[i,5] + psqi$item2new[i]) >=1 && (psqi[i,5] + psqi$item2new[i]) <=2){
    psqi$latency[i] = 1
  } else if ((psqi[i,5] + psqi$item2new[i]) >=3 && (psqi[i,5] + psqi$item2new[i]) <=4){
    psqi$latency[i] = 2
  } else if ((psqi[i,5] + psqi$item2new[i]) >=5 && (psqi[i,5] + psqi$item2new[i]) <=6){
    psqi$latency[i] = 3
  }
}

# calculate dysfunction

for(i in 1:nrow(psqi)){
  if ((psqi$psqi17[i] + psqi$psqi18[i]) == 0){
    psqi$daydys[i] = 0
  } else if ((psqi$psqi17[i] + psqi$psqi18[i]) >=1 && (psqi$psqi17[i] + psqi$psqi18[i]) <=2){
    psqi$daydys[i] = 1
  } else if ((psqi$psqi17[i] + psqi$psqi18[i]) >=3 && (psqi$psqi17[i] + psqi$psqi18[i]) <=4){
    psqi$daydys[i] = 2
  } else if ((psqi$psqi17[i] + psqi$psqi18[i]) >=5 && (psqi$psqi17[i] + psqi$psqi18[i]) <=6){
    psqi$daydys[i] = 3
  }
}

# calculate efficiency

library(lubridate)
newcol1 = hms(psqi$psqi1)
newcol2 = hms(psqi$psqi3)
diff_col <- newcol2 - newcol1

# convert hours and minutes to numerics for calculation

for (i in 1:nrow(psqi)){
  if (hour(diff_col[i]) < 0 && minute(diff_col[i]) >= 0){ 
    hour(diff_col[i]) <- 24 + hour(diff_col[i])
  }
  else if (hour(diff_col[i]) < 0 && minute(diff_col[i]) < 0){ 
    hour(diff_col[i]) <- 23 + hour(diff_col[i])
    minute(diff_col[i]) <- 60 + minute(diff_col[i])
  }
  else if (hour(diff_col[i]) >= 0 && minute(diff_col[i]) < 0){
    hour(diff_col[i]) <- hour(diff_col[i]) - 1
    minute(diff_col[i]) <- 60 + minute(diff_col[i])
  }
}

psqi$hrsInBed = hour(diff_col) + minute(diff_col) / 60

for (i in 1:nrow(psqi)){
  psqi$slpTime[i] = (psqi$psqi4[i]/psqi$hrsInBed[i])*100
}

psqi = psqi[complete.cases(psqi),]
for(i in 1:nrow(psqi)){
  if (psqi$slpTime[i] >= 85){
    psqi$efficiency[i] = 0
  } else if (psqi$slpTime[i] <85 && psqi$slpTime[i] >=75){
    psqi$efficiency[i] = 1
  } else if (psqi$slpTime[i] <75 && psqi$slpTime[i] >=65){
    psqi$efficiency[i] = 2
  } else if (psqi$slpTime[i] < 65){
    psqi$efficiency[i] = 3
  }
}

# total score

for (i in 1:nrow(psqi)){
  psqi$total[i] = (psqi$durat[i] + psqi$disturb[i] + psqi$latency[i] + psqi$daydys[i] + psqi$efficiency[i] + 
                     psqi$psqi17[i] + psqi$psqi18[i])
}

rows = as.numeric(rownames(psqi))
df$psqitotal[rows] <- psqi$total