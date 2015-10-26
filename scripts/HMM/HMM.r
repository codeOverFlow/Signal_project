source('newHMM.r')
source('../tools/usefullTools.r')

classify <- function(hmm0, hmm1, hmm2, hmm3, hmm4, hmm5, hmm6, hmm7, hmm8, hmm9, obs)
{
  l0 = loglikelihood(hmm0, obs)
  l1 = loglikelihood(hmm1, obs)
  l2 = loglikelihood(hmm2, obs)
  l3 = loglikelihood(hmm3, obs)
  l4 = loglikelihood(hmm4, obs)
  l5 = loglikelihood(hmm5, obs)
  l6 = loglikelihood(hmm6, obs)
  l7 = loglikelihood(hmm7, obs)
  l8 = loglikelihood(hmm8, obs)
  l9 = loglikelihood(hmm9, obs)  
  
  maxl = max(l0, l1, l2, l3, l4, l5, l6, l7, l8, l9)
  
  if(maxl == l0)
    return("0")
  else if(maxl == l1)
    return("1")
  else if(maxl == l2)
    return("2")
  else if(maxl == l3)
    return("3")
  else if(maxl == l4)
    return("4")
  else if(maxl == l5)
    return("5")
  else if(maxl == l6)
    return("6")
  else if(maxl == l7)
    return("7")
  else if(maxl == l8)
    return("8")
  else if(maxl == l9)
    return("9")
}

train0 = Load_Obs('../../data/Data5X3/Train_compute_symbol_5_3Digit0.txt')
train1 = Load_Obs('../../data/Data5X3/Train_compute_symbol_5_3Digit1.txt')
train2 = Load_Obs('../../data/Data5X3/Train_compute_symbol_5_3Digit2.txt')
train3 = Load_Obs('../../data/Data5X3/Train_compute_symbol_5_3Digit3.txt')
train4 = Load_Obs('../../data/Data5X3/Train_compute_symbol_5_3Digit4.txt')
train5 = Load_Obs('../../data/Data5X3/Train_compute_symbol_5_3Digit5.txt')
train6 = Load_Obs('../../data/Data5X3/Train_compute_symbol_5_3Digit6.txt')
train7 = Load_Obs('../../data/Data5X3/Train_compute_symbol_5_3Digit7.txt')
train8 = Load_Obs('../../data/Data5X3/Train_compute_symbol_5_3Digit8.txt')
train9 = Load_Obs('../../data/Data5X3/Train_compute_symbol_5_3Digit9.txt')
feat0=train0[1:10,]
feat1=train1[1:10,]
feat2=train2[1:10,]
feat3=train3[1:10,]
feat4=train4[1:10,]
feat5=train5[1:10,]
feat6=train6[1:10,]
feat7=train7[1:10,]
feat8=train8[1:10,]
feat9=train9[1:10,]

states = c("s1", "s2", "s3")
symbols = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15)
startProbs = c(1,0,0)
transProbs = matrix(c(0.9, 0.1, 0, 0, 0.9, 0.1, 0, 0, 1), nrow = 3, ncol = 3, byrow=T)

hmm = initHMM(states, symbols, startProbs=startProbs, transProbs=transProbs)

cat("Train\n\n")
hmm_train_0 = baumWelchList(hmm, feat0)$hmm
hmm_train_1 = baumWelchList(hmm, feat1)$hmm
hmm_train_2 = baumWelchList(hmm, feat2)$hmm
hmm_train_3 = baumWelchList(hmm, feat3)$hmm
hmm_train_4 = baumWelchList(hmm, feat4)$hmm
hmm_train_5 = baumWelchList(hmm, feat5)$hmm
hmm_train_6 = baumWelchList(hmm, feat6)$hmm
hmm_train_7 = baumWelchList(hmm, feat7)$hmm
hmm_train_8 = baumWelchList(hmm, feat8)$hmm
hmm_train_9 = baumWelchList(hmm, feat9)$hmm


test0 = Load_Obs('../../data/Data5X3/Test_compute_symbol_5_3Digit0.txt')
test1 = Load_Obs('../../data/Data5X3/Test_compute_symbol_5_3Digit1.txt')
test2 = Load_Obs('../../data/Data5X3/Test_compute_symbol_5_3Digit2.txt')
test3 = Load_Obs('../../data/Data5X3/Test_compute_symbol_5_3Digit3.txt')
test4 = Load_Obs('../../data/Data5X3/Test_compute_symbol_5_3Digit4.txt')
test5 = Load_Obs('../../data/Data5X3/Test_compute_symbol_5_3Digit5.txt')
test6 = Load_Obs('../../data/Data5X3/Test_compute_symbol_5_3Digit6.txt')
test7 = Load_Obs('../../data/Data5X3/Test_compute_symbol_5_3Digit7.txt')
test8 = Load_Obs('../../data/Data5X3/Test_compute_symbol_5_3Digit8.txt')
test9 = Load_Obs('../../data/Data5X3/Test_compute_symbol_5_3Digit9.txt')

for(j in 1:10)
{
  cat("Test : 0 => ", classify(hmm_train_0, hmm_train_1, hmm_train_2, hmm_train_3, hmm_train_4, hmm_train_5, hmm_train_6, hmm_train_7, hmm_train_8, hmm_train_9, test0[j,]), "\n")
}
cat("\n")

for(j in 1:10)
{
  cat("Test : 1 => ", classify(hmm_train_0, hmm_train_1, hmm_train_2, hmm_train_3, hmm_train_4, hmm_train_5, hmm_train_6, hmm_train_7, hmm_train_8, hmm_train_9, test1[j,]), "\n")
}
cat("\n")

for(j in 1:10)
{
  cat("Test : 2 => ", classify(hmm_train_0, hmm_train_1, hmm_train_2, hmm_train_3, hmm_train_4, hmm_train_5, hmm_train_6, hmm_train_7, hmm_train_8, hmm_train_9, test2[j,]), "\n")
}
cat("\n")

for(j in 1:10)
{
  cat("Test : 3 => ", classify(hmm_train_0, hmm_train_1, hmm_train_2, hmm_train_3, hmm_train_4, hmm_train_5, hmm_train_6, hmm_train_7, hmm_train_8, hmm_train_9, test3[j,]), "\n")
}
cat("\n")

for(j in 1:10)
{
  cat("Test : 4 => ", classify(hmm_train_0, hmm_train_1, hmm_train_2, hmm_train_3, hmm_train_4, hmm_train_5, hmm_train_6, hmm_train_7, hmm_train_8, hmm_train_9, test4[j,]), "\n")
}
cat("\n")

for(j in 1:10)
{
  cat("Test : 5 => ", classify(hmm_train_0, hmm_train_1, hmm_train_2, hmm_train_3, hmm_train_4, hmm_train_5, hmm_train_6, hmm_train_7, hmm_train_8, hmm_train_9, test5[j,]), "\n")
}
cat("\n")

for(j in 1:10)
{
  cat("Test : 6 => ", classify(hmm_train_0, hmm_train_1, hmm_train_2, hmm_train_3, hmm_train_4, hmm_train_5, hmm_train_6, hmm_train_7, hmm_train_8, hmm_train_9, test6[j,]), "\n")
}
cat("\n")

for(j in 1:10)
{
  cat("Test : 7 => ", classify(hmm_train_0, hmm_train_1, hmm_train_2, hmm_train_3, hmm_train_4, hmm_train_5, hmm_train_6, hmm_train_7, hmm_train_8, hmm_train_9, test7[j,]), "\n")
}
cat("\n")

for(j in 1:10)
{
  cat("Test : 8 => ", classify(hmm_train_0, hmm_train_1, hmm_train_2, hmm_train_3, hmm_train_4, hmm_train_5, hmm_train_6, hmm_train_7, hmm_train_8, hmm_train_9, test8[j,]), "\n")
}
cat("\n")

for(j in 1:10)
{
  cat("Test : 9 => ", classify(hmm_train_0, hmm_train_1, hmm_train_2, hmm_train_3, hmm_train_4, hmm_train_5, hmm_train_6, hmm_train_7, hmm_train_8, hmm_train_9, test9[j,]), "\n")
}
cat("\n")
