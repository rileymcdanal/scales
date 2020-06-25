## scoring for the PCL5 (PTSD assessment)

# change to your own column names

pcl5 <- df[ , c("pcl51", "pcl52", "pcl53", "pcl54", "pcl55", 
                "pcl56", "pcl57", 'pcl58', "pcl59", "pcl510", "pcl511", 
                "pcl512", "pcl513", "pcl514", "pcl515", "pcl516", 
                "pcl517", 'pcl518', 'pcl519', 'pcl520')] 

# change to your own column indexes
# you can use where(colnames(df)=='yourcolname') to find the correct index

pcl5 <- pcl5[c(248:319), ]

# for loop to convert NAs to 0 so we can sum

for(col in names(pcl5)){
  pcl5[col][is.na(pcl5[col])] <- 0
}

# calculate total score for pcl5 and put into df

pcl5total <- pcl5$pcl51 + pcl5$pcl52 + pcl5$pcl53 + 
  pcl5$pcl54 + pcl5$pcl55 + pcl5$pcl56 + 
  pcl5$pcl57 + pcl5$pcl58 + pcl5$pcl59 + 
  pcl5$pcl510 + pcl5$pcl511 + pcl5$pcl512 + 
  pcl5$pcl513 + pcl5$pcl514 + pcl5$pcl515 + 
  pcl5$pcl516 + pcl5$pcl517 + pcl5$pcl518 + 
  pcl5$pcl519 + pcl5$pcl520

df <- mutate(df,pcl5total = pcl5total)