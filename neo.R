## scoring for the NEO (personality assessment)

# change to your own column names

neo <- df[ , c("neo1", 'neo2', 'neo3', 'neo4', 'neo5', 'neo6', 'neo7',
               'neo8', 'neo9', 'neo10', 'neo11', 'neo12', 'neo13',
               'neo14', 'neo15', 'neo16',"neo17", 'neo18', 'neo19', 
               'neo20', 'neo21', 'neo22', 'neo23', 'neo24', 'neo25', 
               'neo26', 'neo27', 'neo28', 'neo29', 'neo30', 'neo31',
               'neo32', "neo33", 'neo34', 'neo35', 'neo36', 'neo37',
               'neo38', 'neo39', 'neo40', 'neo41', 'neo42', 'neo43', 
               'neo44', 'neo45', 'neo46', 'neo47', 'neo48', "neo49", 
               'neo50', 'neo51', 'neo52', 'neo53', 'neo54', 'neo55',
               'neo56', 'neo57', 'neo58', 'neo59', 'neo60')]

# recode values (some items reverse scored)

neo$neo1 <- recode(neo$neo1, "1" = "5", "2" = "4", '3' = '3', "4" = "2", "5" = "1")
neo$neo16 <- recode(neo$neo16, "1" = "5", "2" = "4", '3' = '3', "4" = "2", "5" = "1")
neo$neo31 <- recode(neo$neo31, "1" = "5", "2" = "4", '3' = '3', "4" = "2", "5" = "1")
neo$neo46 <- recode(neo$neo46, "1" = "5", "2" = "4", '3' = '3', "4" = "2", "5" = "1")
neo$neo27 <- recode(neo$neo27, "1" = "5", "2" = "4", '3' = '3', "4" = "2", "5" = "1")
neo$neo42 <- recode(neo$neo42, "1" = "5", "2" = "4", '3' = '3', "4" = "2", "5" = "1")
neo$neo57 <- recode(neo$neo57, "1" = "5", "2" = "4", '3' = '3', "4" = "2", "5" = "1")
neo$neo3 <- recode(neo$neo3, "1" = "5", "2" = "4", '3' = '3', "4" = "2", "5" = "1")
neo$neo8 <- recode(neo$neo8, "1" = "5", "2" = "4", '3' = '3', "4" = "2", "5" = "1")
neo$neo18 <- recode(neo$neo18, "1" = "5", "2" = "4", '3' = '3', "4" = "2", "5" = "1")
neo$neo23 <- recode(neo$neo23, "1" = "5", "2" = "4", '3' = '3', "4" = "2", "5" = "1")
neo$neo33 <- recode(neo$neo33, "1" = "5", "2" = "4", '3' = '3', "4" = "2", "5" = "1")
neo$neo38 <- recode(neo$neo38, "1" = "5", "2" = "4", '3' = '3', "4" = "2", "5" = "1")
neo$neo48 <- recode(neo$neo48, "1" = "5", "2" = "4", '3' = '3', "4" = "2", "5" = "1")
neo$neo9 <- recode(neo$neo9, "1" = "5", "2" = "4", '3' = '3', "4" = "2", "5" = "1")
neo$neo14 <- recode(neo$neo14, "1" = "5", "2" = "4", '3' = '3', "4" = "2", "5" = "1")
neo$neo24 <- recode(neo$neo24, "1" = "5", "2" = "4", '3' = '3', "4" = "2", "5" = "1")
neo$neo29 <- recode(neo$neo29, "1" = "5", "2" = "4", '3' = '3', "4" = "2", "5" = "1")
neo$neo39 <- recode(neo$neo39, "1" = "5", "2" = "4", '3' = '3', "4" = "2", "5" = "1")
neo$neo44 <- recode(neo$neo44, "1" = "5", "2" = "4", '3' = '3', "4" = "2", "5" = "1")
neo$neo54 <- recode(neo$neo54, "1" = "5", "2" = "4", '3' = '3', "4" = "2", "5" = "1")
neo$neo59 <- recode(neo$neo59, "1" = "5", "2" = "4", '3' = '3', "4" = "2", "5" = "1")
neo$neo15 <- recode(neo$neo15, "1" = "5", "2" = "4", '3' = '3', "4" = "2", "5" = "1")
neo$neo30 <- recode(neo$neo30, "1" = "5", "2" = "4", '3' = '3', "4" = "2", "5" = "1")
neo$neo45 <- recode(neo$neo45, "1" = "5", "2" = "4", '3' = '3', "4" = "2", "5" = "1")
neo$neo55 <- recode(neo$neo55, "1" = "5", "2" = "4", '3' = '3', "4" = "2", "5" = "1")

# make numeric so we can sum

neo$neo1 <- as.numeric(neo$neo1) 
neo$neo16 <- as.numeric(neo$neo16) 
neo$neo31 <- as.numeric(neo$neo31) 
neo$neo46 <- as.numeric(neo$neo46) 
neo$neo27 <- as.numeric(neo$neo27) 
neo$neo42 <- as.numeric(neo$neo42) 
neo$neo57 <- as.numeric(neo$neo57) 
neo$neo3 <- as.numeric(neo$neo3) 
neo$neo8 <- as.numeric(neo$neo8) 
neo$neo18 <- as.numeric(neo$neo18) 
neo$neo23 <- as.numeric(neo$neo23) 
neo$neo33 <- as.numeric(neo$neo33) 
neo$neo38 <- as.numeric(neo$neo38) 
neo$neo48 <- as.numeric(neo$neo48) 
neo$neo9 <- as.numeric(neo$neo9) 
neo$neo14 <- as.numeric(neo$neo14) 
neo$neo24 <- as.numeric(neo$neo24) 
neo$neo29 <- as.numeric(neo$neo29) 
neo$neo39 <- as.numeric(neo$neo39) 
neo$neo44 <- as.numeric(neo$neo44) 
neo$neo54 <- as.numeric(neo$neo54) 
neo$neo59 <- as.numeric(neo$neo59) 
neo$neo15 <- as.numeric(neo$neo15) 
neo$neo30 <- as.numeric(neo$neo30) 
neo$neo45 <- as.numeric(neo$neo45) 
neo$neo55 <- as.numeric(neo$neo55) 

# calculate score for neuroticism and put into df

neon <- (neo$neo6 + neo$neo11 + neo$neo21 + neo$neo26 + neo$neo36
         + neo$neo41 + neo$neo51 + neo$neo56 + neo$neo1 + neo$neo16
         + neo$neo31 + neo$neo46)

df <- mutate(df,neon = neon)

# calculate score for extraversion and put into df

neoe <- (neo$neo2 + neo$neo7 + neo$neo12 + neo$neo17 + neo$neo22
         + neo$neo32 + neo$neo37 + neo$neo47 + neo$neo52 + neo$neo27
         + neo$neo42 + neo$neo57)

df <- mutate(df,neoe = neoe)

# calculate score for openness and put into df

neoo <- (neo$neo13 + neo$neo28 + neo$neo43 + neo$neo53 + neo$neo58
         + neo$neo3 + neo$neo8 + neo$neo18 + neo$neo23 + neo$neo33
         + neo$neo38 + neo$neo48)

df <- mutate(df,neoo = neoo)

# calculate score for agreeableness and put into df

neoa <- (neo$neo4 + neo$neo19 + neo$neo34 + neo$neo49 + neo$neo9
         + neo$neo14 + neo$neo24 + neo$neo29 + neo$neo39 + neo$neo44
         + neo$neo54 + neo$neo59)

df <- mutate(df,neoa = neoa)

# calculate score for conscientiousness and put into df

neoc <- (neo$neo5 + neo$neo10 + neo$neo20 + neo$neo25 + neo$neo35
         + neo$neo40 + neo$neo50 + neo$neo60 + neo$neo15 + neo$neo30
         + neo$neo45 + neo$neo55)

df <- mutate(df,neoc = neoc)
