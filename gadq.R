## scoring for the GADQ-5 (anxiety assessment)

# change to your own column names

gadq <- df[ , c("gadq1", "gadq2", "gadq3", "gadq4", "gadq6")] 

gadq5 <- df[ , c('gadq5a', 'gadq5b', 'gadq5c', 'gadq5d', 'gadq5e', 'gadq5f')]

# convert NAs to 0 so we can sum

for(col in names(gadq)){
  gadq[col][is.na(gadq[col])] <- 0
}

for(col in names(gadq5)){
  gadq5[col][is.na(gadq5[col])] <- 0
}

df['gadq7'][is.na(df['gadq7'])] <- 0

df['gadq8'][is.na(df['gadq8'])] <- 0

df['gadq9'][is.na(df['gadq9'])] <- 0

# calculate values for subsets of the scale

gadq5u <- (gadq5$gadq5a + gadq5$gadq5b + gadq5$gadq5c + gadq5$gadq5d + gadq5$gadq5e + 
             gadq5$gadq5f) / 3

gadq7 <- df$gadq7 / 3

gadq8 <- df$gadq8 / 4

gadq9 <- df$gadq9 / 4

# sum total score and add into df

gadqtotal <- gadq$gadq1 + gadq$gadq2 + gadq$gadq3 + 
  gadq$gadq4 + gadq$gadq6 + gadq5u + gadq7 + gadq8 + gadq9

df <- mutate(df,gadqtotal = gadqtotal)