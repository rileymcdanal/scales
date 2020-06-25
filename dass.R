## scoring for the DASS (assessment of depression anxiety and stress)

# change to your own column names

dass <- df[ , c("dass1", "dass2", "dass3", "dass4", "dass5", "dass6", 'dass7', 
                "dass8", "dass9", "dass10", 'dass11', "dass12", "dass13", 'dass14', "dass15",
                "dass16", "dass17", 'dass18', "dass19", "dass20", 'dass21', "dass22", "dass23",
                "dass24", "dass25", 'dass26', "dass27", "dass28", 'dass29', "dass30", "dass31",  
                "dass32", "dass33", 'dass34', "dass35", "dass36", 'dass37', "dass38", "dass39", 
                "dass40", "dass41", 'dass42')] 

# to populate later

dass <- mutate(dass,totalD = NA)
dass <- mutate(dass,totalA = NA)
dass <- mutate(dass,totalS = NA)

# for loop to convert NAs to 0 so we can sum

for(col in names(dass)){
  dass[col][is.na(dass[col])] <- 0
  df[col] <- dass[col]
}

# calculate total score for dass across each subscale

for (i in 1:nrow(dass)){
  dass$totalD[i] = ((dass$dass3[i])+(dass$dass5[i])+(dass$dass10[i])+(dass$dass13[i])+
                      (dass$dass16[i])+(dass$dass17[i])+(dass$dass21[i])+(dass$dass24[i])+
                      (dass$dass26[i])+(dass$dass31[i])+(dass$dass34[i])+(dass$dass37[i])+
                      (dass$dass38[i])+(dass$dass42[i]))
  
  dass$totalA[i] = ((dass$dass2[i])+(dass$dass4[i])+(dass$dass7[i])+(dass$dass9[i])+
                      (dass$dass15[i])+(dass$dass19[i])+(dass$dass20[i])+(dass$dass23[i])+
                      (dass$dass25[i])+(dass$dass28[i])+(dass$dass30[i])+(dass$dass36[i])+
                      (dass$dass40[i])+(dass$dass41[i]))
  
  dass$totalS[i] = ((dass$dass1[i])+(dass$dass6[i])+(dass$dass8[i])+(dass$dass11[i])+
                      (dass$dass12[i])+(dass$dass14[i])+(dass$dass18[i])+(dass$dass22[i])+
                      (dass$dass27[i])+(dass$dass29[i])+(dass$dass32[i])+(dass$dass33[i])+
                      (dass$dass35[i])+(dass$dass39[i]))
}

# put scores into df

df <- mutate(df,dassd = dass$totalD)
df <- mutate(df,dassa = dass$totalA)
df <- mutate(df,dasss = dass$totalS)