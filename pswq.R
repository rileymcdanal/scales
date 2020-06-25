## scoring for the PSWQ (worry assessment)

# change to your own column names

pswq <- df[ , c("pswq1", 'pswq2', 'pswq3', 'pswq4', 'pswq5', 'pswq6', 'pswq7',
                'pswq8', 'pswq9', 'pswq10', 'pswq11', 'pswq12', 'pswq13',
                'pswq14', 'pswq15', 'pswq16')]

#recode items 1, 3, 8, 10, 11 (reverse scored)

pswq$pswq1 <- dplyr::recode(pswq$pswq1, "1" = "5", "2" = "4", '3' = '3', "4" = "2", "5" = "1")
pswq$pswq3 <- dplyr::recode(pswq$pswq3, "1" = "5", "2" = "4", '3' = '3', "4" = "2", "5" = "1")
pswq$pswq8 <- dplyr::recode(pswq$pswq8, "1" = "5", "2" = "4", '3' = '3', "4" = "2", "5" = "1")
pswq$pswq10 <- dplyr::recode(pswq$pswq10, "1" = "5", "2" = "4", '3' = '3', "4" = "2", "5" = "1")
pswq$pswq11 <- dplyr::recode(pswq$pswq11, "1" = "5", "2" = "4", '3' = '3', "4" = "2", "5" = "1")

pswq$pswq1 <- as.numeric(pswq$pswq1) 
pswq$pswq3 <- as.numeric(pswq$pswq3) 
pswq$pswq8 <- as.numeric(pswq$pswq8) 
pswq$pswq10 <- as.numeric(pswq$pswq10) 
pswq$pswq11 <- as.numeric(pswq$pswq11) 

#generate total score, this will not compute values for rows with NA's
# pass na.omit=true to sum function to change this

pswqtotal <- (pswq$pswq1 + pswq$pswq2 + pswq$pswq3 + pswq$pswq4 + pswq$pswq5
              + pswq$pswq6 + pswq$pswq7 + pswq$pswq8 + pswq$pswq9 + pswq$pswq10
              + pswq$pswq11 + pswq$pswq12 + pswq$pswq13 + pswq$pswq14 + pswq$pswq15
              + pswq$pswq16) 

# put scores into df

df <- mutate(df,pswqtotal = pswqtotal)

