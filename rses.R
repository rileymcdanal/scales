## score RSES (self-esteem assessment)

# change to your own column names

rses <- df[ , c("rses_1", 'rses_2', 'rses_3', 'rses_4', 'rses_5', 'rses_6',
                'rses_7', 'rses_8', 'rses_9', 'rses_10')]

# recode items 2, 5, 6, 8, 9 (reverse scored)

rses$rses_2 <- recode(rses$rses_2, "1" = "4", "2" = "3", '3' = '2', "4" = "1")
rses$rses_5 <- recode(rses$rses_5, "1" = "4", "2" = "3", '3' = '2', "4" = "1")
rses$rses_6 <- recode(rses$rses_6, "1" = "4", "2" = "3", '3' = '2', "4" = "1")
rses$rses_8 <- recode(rses$rses_8, "1" = "4", "2" = "3", '3' = '2', "4" = "1")
rses$rses_9 <- recode(rses$rses_9, "1" = "4", "2" = "3", '3' = '2', "4" = "1")

df$rses_2 <- rses$rses_2
df$rses_5 <- rses$rses_5
df$rses_6 <- rses$rses_6
df$rses_8 <- rses$rses_8
df$rses_9 <- rses$rses_9

# make numeric for summing
# change to your own indexes
# you can use which(colnames(df)=='yourcolname') to find the right index

sapply(95:104, function(i) {
  df[,i] <<- as.numeric(df[,i])
})

# calculate total and put into df

rsestotal <- (df$rses_1 + df$rses_2 + df$rses_3 + df$rses_4 + df$rses_5
              + df$rses_6 + df$rses_7 + df$rses_8 + df$rses_9 + df$rses_10) 

df <- mutate(df,rsestotal = rsestotal)