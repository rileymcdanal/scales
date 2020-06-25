## score INCOM (social comparison assessment)

# change to your own col names

incom <- df[ , c("incom_1", 'incom_2', 'incom_3', 'incom_4', 'incom_5', 'incom_6',
                 'incom_7', 'incom_8', 'incom_9', 'incom_10', 'incom_11')]

# recode items 6, 10 (reverse scored)

incom$incom_6 <- recode(incom$incom_6, "1" = "5", "2" = "4", '3' = '3', "4" = "2", "5" = "1")
incom$incom_10 <- recode(incom$incom_10, "1" = "5", "2" = "4", '3' = '3', "4" = "2", "5" = "1")

df$incom_6 <- incom$incom_6
df$incom_10 <- incom$incom_10

# make numeric for summing
# change to your own indexes
# you can use which(colnames(df)=='yourcolname') to find the right index

sapply(105:115, function(i) {
  df[,i] <<- as.numeric(df[,i])
})

# calculate total and put into df

incomtotal <- (df$incom_1 + df$incom_2 + df$incom_3 + df$incom_4 + df$incom_5
               + df$incom_6 + df$incom_7 + df$incom_8 + df$incom_9 + df$incom_10
               + df$incom_11) 

df <- mutate(df,incomtotal = incomtotal)