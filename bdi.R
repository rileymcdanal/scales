## scoring for bdi (depression assessment) minus q9
# you can add in q9 where appropriate if you used it

# change to your own col names

bdi <- df[ , c('bdi1', 'bdi2', 'bdi3', 'bdi4', 'bdi5', 'bdi6', 'bdi7', 'bdi8',
               'bdi10', 'bdi11', 'bdi12', 'bdi13', 'bdi14', 'bdi15', 'bdi16', 'bdi17',
               'bdi18', 'bdi19', 'bdi20', 'bdi21')]

# you don't need to do this if participants selected only one option for each question
# but if they could select more than one answer per question you would need this

df$bdi1 <- str_sub(df$bdi1, start = -1L, end = -1L)
df$bdi2 <- str_sub(df$bdi2, start = -1L, end = -1L)
df$bdi3 <- str_sub(df$bdi3, start = -1L, end = -1L)
df$bdi4 <- str_sub(df$bdi4, start = -1L, end = -1L)
df$bdi5 <- str_sub(df$bdi5, start = -1L, end = -1L)
df$bdi6 <- str_sub(df$bdi6, start = -1L, end = -1L)
df$bdi7 <- str_sub(df$bdi7, start = -1L, end = -1L)
df$bdi8 <- str_sub(df$bdi8, start = -1L, end = -1L)
df$bdi10 <- str_sub(df$bdi10, start = -1L, end = -1L)
df$bdi11 <- str_sub(df$bdi11, start = -1L, end = -1L)
df$bdi12 <- str_sub(df$bdi12, start = -1L, end = -1L)
df$bdi13 <- str_sub(df$bdi13, start = -1L, end = -1L)
df$bdi14 <- str_sub(df$bdi14, start = -1L, end = -1L)
df$bdi15 <- str_sub(df$bdi15, start = -1L, end = -1L)
df$bdi16 <- str_sub(df$bdi16, start = -1L, end = -1L)
df$bdi17 <- str_sub(df$bdi17, start = -1L, end = -1L)
df$bdi18 <- str_sub(df$bdi18, start = -1L, end = -1L)
df$bdi19 <- str_sub(df$bdi19, start = -1L, end = -1L)
df$bdi20 <- str_sub(df$bdi20, start = -1L, end = -1L)
df$bdi21 <- str_sub(df$bdi21, start = -1L, end = -1L)

# make values numeric so we can sum

sapply(65:84, function(i) {
  df[,i] <<- as.numeric(df[,i])
})

# calculate total score

bditotal <- (df$bdi1 + df$bdi2 + df$bdi3 + df$bdi4 + df$bdi5
             + df$bdi6 + df$bdi7 + df$bdi8 + df$bdi10
             + df$bdi11 + df$bdi12 + df$bdi13 + df$bdi14
             + df$bdi15 + df$bdi16 + df$bdi17 + df$bdi18
             + df$bdi19 + df$bdi20 + df$bdi21) 

# put in df

df <- mutate(df,bditotal = bditotal)
