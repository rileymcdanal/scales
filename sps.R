## score SPS (social provisions assessment)

# change to your own column names

sps <- df[ , c('sps_1', 'sps_2', 'sps_3', 'sps_4', 'sps_5', 'sps_6', 'sps_7', 
               'sps_8', 'sps_9', 'sps_10', 'sps_11', 'sps_12', 'sps_13', 
               'sps_14', 'sps_15', 'sps_16', 'sps_17', 'sps_18', 'sps_19',
               'sps_20', 'sps_21', 'sps_22', 'sps_23', 'sps_24')]

# recode items 2, 3, 6, 9, 10, 14, 15, 18, 19, 21, 22 (reverse scored)

sps$sps_2 <- recode(sps$sps_2, "1" = "4", "2" = "3", '3' = '2', "4" = "1")
sps$sps_3 <- recode(sps$sps_3, "1" = "4", "2" = "3", '3' = '2', "4" = "1")
sps$sps_6 <- recode(sps$sps_6, "1" = "4", "2" = "3", '3' = '2', "4" = "1")
sps$sps_9 <- recode(sps$sps_9, "1" = "4", "2" = "3", '3' = '2', "4" = "1")
sps$sps_10 <- recode(sps$sps_10, "1" = "4", "2" = "3", '3' = '2', "4" = "1")
sps$sps_14 <- recode(sps$sps_14, "1" = "4", "2" = "3", '3' = '2', "4" = "1")
sps$sps_15 <- recode(sps$sps_15, "1" = "4", "2" = "3", '3' = '2', "4" = "1")
sps$sps_18 <- recode(sps$sps_18, "1" = "4", "2" = "3", '3' = '2', "4" = "1")
sps$sps_19 <- recode(sps$sps_19, "1" = "4", "2" = "3", '3' = '2', "4" = "1")
sps$sps_21 <- recode(sps$sps_21, "1" = "4", "2" = "3", '3' = '2', "4" = "1")
sps$sps_22 <- recode(sps$sps_22, "1" = "4", "2" = "3", '3' = '2', "4" = "1")

df$sps_2 <- sps$sps_2
df$sps_3 <- sps$sps_3
df$sps_6 <- sps$sps_6
df$sps_9 <- sps$sps_9
df$sps_10 <- sps$sps_10
df$sps_14 <- sps$sps_14
df$sps_15 <- sps$sps_15
df$sps_18 <- sps$sps_18
df$sps_19 <- sps$sps_19
df$sps_21 <- sps$sps_21
df$sps_22 <- sps$sps_22

# make numeric for summing
# change to your own indexes
# you can use which(colnames(df)=='yourcolname') to find the right index

sapply(116:139, function(i) {
  df[,i] <<- as.numeric(df[,i])
})

# calculate total and put in df

spstotal = rowSums(df[116:139])

df <- mutate(df,spstotal = spstotal)