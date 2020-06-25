## scoring for the ERQ (assessment of emotion regulation)

# change to your own column names

erqr <- df[ , c("erq_1", "erq_3", "erq_5", "erq_7", "erq_8", "erq_10")] 

erqs <- df[ , c('erq_2', 'erq_4', 'erq_6', 'erq_9')]

# make values numeric for summing
# change to your own column indexes
# you can use which(colnames(df)=='yourcolname') to find the right index

sapply(85:94, function(i) {
  df[,i] <<- as.numeric(df[,i])
})

# calculate total score for reappraisal subscale and suppression subscale respectively

erqrtotal <- df$erq_1 + df$erq_3 + df$erq_5 + df$erq_7 + df$erq_8 + df$erq_10
erqstotal <- df$erq_2 + df$erq_4 + df$erq_6 + df$erq_9

# add into df

df <- mutate(df,erqrtotal = erqrtotal)
df <- mutate(df,erqstotal = erqstotal)