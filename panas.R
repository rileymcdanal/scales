## scoring for the PANAS (positive and negative affect assessment)

# change to your own column names

panasp <- df[ , c("panas1", "panas3", "panas5", "panas9", "panas10", "panas12",
                  'panas14', 'panas16', 'panas17', 'panas19')] 

panasn <- df[ , c("panas2", "panas4", "panas6", "panas7", "panas8", "panas11",
                  'panas13', 'panas15', 'panas18', 'panas20')]

# calculate total scores for positive affect and negative affect respectively
# and put into df

panastotalp <- (panasp$panas1 + panasp$panas3 + panasp$panas5 + panasp$panas9 + 
                  panasp$panas10 + panasp$panas12 + panasp$panas14 + panasp$panas16 + 
                  panasp$panas17 + panasp$panas19)
panastotaln <- (panasn$panas2 + panasn$panas4 + panasn$panas6 + panasn$panas7 + 
                  panasn$panas8 + panasn$panas11 + panasn$panas13 + panasn$panas15 + 
                  panasn$panas18 + panasn$panas20)

df <- mutate(df,panastotalp = panastotalp)
df <- mutate(df,panastotaln = panastotaln)