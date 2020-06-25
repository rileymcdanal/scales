## scoring for the RPA (response to positive affect assessment)

# change to your own column names

rpa <- df[ , c("rpa1", "rpa2", "rpa3", "rpa4", "rpa5", 
               "rpa6", "rpa7", 'rpa8', "rpa9", "rpa10", "rpa11", 
               "rpa12", "rpa13", "rpa14", "rpa15", "rpa16", 
               "rpa17")] 

# change to your own column indexes
# you can use where(colnames(df)=='yourcolname') to find the correct index

rpa <- rpa[c(248:319), ]

# for loop to convert NAs to 0 so we can sum

for(col in names(rpa)){
  rpa[col][is.na(rpa[col])] <- 0
}

# calculate total score for rpa and put into df

rpatotal <- rpa$rpa1 + rpa$rpa2 + rpa$rpa3 + 
  rpa$rpa4 + rpa$rpa5 + rpa$rpa6 + 
  rpa$rpa7 + rpa$rpa8 + rpa$rpa9 + 
  rpa$rpa10 + rpa$rpa11 + rpa$rpa12 + 
  rpa$rpa13 + rpa$rpa14 + rpa$rpa15 + 
  rpa$rpa16 + rpa$rpa17

df <- mutate(df,rpatotal = rpatotal)