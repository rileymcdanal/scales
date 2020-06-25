## scoring for the MSI BPD (boderline personality disorder assessment)

# change to your own column names

msibpd <- df[ , c("msibpd1", "msibpd2", "msibpd3", "msibpd4",
                  "msibpd5", "msibpd6", "msibpd7", 'msibpd8',
                  "msibpd9", "msibpd10")] 

# change to your own column indexes
# you can use where(colnames(df)=='yourcolname') to find the correct index

msibpd <- msibpd[c(248:319), ]

# for loop to convert NAs to 0 so we can sum

for(col in names(msibpd)){
  msibpd[col][is.na(msibpd[col])] <- 0
}

# calculate total score for msi bpd and put into df

msibpdtotal <- msibpd$msibpd1 + msibpd$msibpd2 + msibpd$msibpd3 + 
  msibpd$msibpd4 + msibpd$msibpd5 + msibpd$msibpd6 + 
  msibpd$msibpd7 + msibpd$msibpd8 + msibpd$msibpd9 + 
  msibpd$msibpd10

df <- mutate(df,msibpdtotal = msibpdtotal)
