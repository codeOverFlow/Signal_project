source('newHMM.r')
source('../tools/usefullTools.r')

classify <- function(hmms, obs)
{
  lls=c()
  for(i in c(0,1,2,3,4,5,6,7,8,9))
  {
    lls = c(lls, loglikelihood(hmms[i+1,], obs))
  }
  
  maxl = max(lls)
  
  for(i in c(0,1,2,3,4,5,6,7,8,9))
  {
    if(maxl == lls[i+1])
      return(i)
  }
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
indexTrain=c(0, 
              dim(train0)[1], 
              dim(train0)[1]+dim(train1)[1],
              dim(train0)[1]+dim(train1)[1]+dim(train2)[1],
              dim(train0)[1]+dim(train1)[1]+dim(train2)[1]+dim(train3)[1],
              dim(train0)[1]+dim(train1)[1]+dim(train2)[1]+dim(train3)[1]+dim(train4)[1],
              dim(train0)[1]+dim(train1)[1]+dim(train2)[1]+dim(train3)[1]+dim(train4)[1]+dim(train5)[1],
              dim(train0)[1]+dim(train1)[1]+dim(train2)[1]+dim(train3)[1]+dim(train4)[1]+dim(train5)[1]+dim(train6)[1],
              dim(train0)[1]+dim(train1)[1]+dim(train2)[1]+dim(train3)[1]+dim(train4)[1]+dim(train5)[1]+dim(train6)[1]+dim(train7)[1],
              dim(train0)[1]+dim(train1)[1]+dim(train2)[1]+dim(train3)[1]+dim(train4)[1]+dim(train5)[1]+dim(train6)[1]+dim(train7)[1]+dim(train8)[1],
              dim(train0)[1]+dim(train1)[1]+dim(train2)[1]+dim(train3)[1]+dim(train4)[1]+dim(train5)[1]+dim(train6)[1]+dim(train7)[1]+dim(train8)[1]+dim(train9)[1])

#indexTrain=c(0,10,20,30,40,50,60,70,80,90,100)
indexTrain
feats=rbind(train0, train1, train2, train3, train4, train5, train6, train7, train8, train9)
#feats=rbind(train0[1:10,], train1[1:10,], train2[1:10,], train3[1:10,], train4[1:10,], train5[1:10,], train6[1:10,], train7[1:10,], train8[1:10,], train9[1:10,])

states = c("s1", "s2", "s3")
symbols = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15)
startProbs = c(1,0,0)
transProbs = matrix(c(0.9, 0.1, 0, 0, 0.9, 0.1, 0, 0, 1), nrow = 3, ncol = 3, byrow=T)

hmm = initHMM(states, symbols, startProbs=startProbs, transProbs=transProbs)

cat("\n\n")

hmm_train=rbind()
indexHmmTrain=c(0)
sum=0

for(i in 0:9)
{
  cat("Train ",i,"\n")
  hmm_tmp=baumWelchList(hmm, feats[(indexTrain[i+1]+1):(indexTrain[i+2]),])$hmm
  dput(hmm_tmp, file=paste('data/hmm',i,sep=''))
  hmm_train=rbind(hmm_train, hmm_tmp)
  indexHmmTrain=c(indexHmmTrain, sum+dim(hmm_tmp)[1])
  sum=sum+dim(hmm_tmp)[1]
}

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
tests = rbind(test0, test1, test2, test3, test4, test5, test6, test7, test8, test9)
indexTest=c(0, 
              dim(test0)[1], 
              dim(test0)[1]+dim(test1)[1],
              dim(test0)[1]+dim(test1)[1]+dim(test2)[1],
              dim(test0)[1]+dim(test1)[1]+dim(test2)[1]+dim(test3)[1],
              dim(test0)[1]+dim(test1)[1]+dim(test2)[1]+dim(test3)[1]+dim(test4)[1],
              dim(test0)[1]+dim(test1)[1]+dim(test2)[1]+dim(test3)[1]+dim(test4)[1]+dim(test5)[1],
              dim(test0)[1]+dim(test1)[1]+dim(test2)[1]+dim(test3)[1]+dim(test4)[1]+dim(test5)[1]+dim(test6)[1],
              dim(test0)[1]+dim(test1)[1]+dim(test2)[1]+dim(test3)[1]+dim(test4)[1]+dim(test5)[1]+dim(test6)[1]+dim(test7)[1],
              dim(test0)[1]+dim(test1)[1]+dim(test2)[1]+dim(test3)[1]+dim(test4)[1]+dim(test5)[1]+dim(test6)[1]+dim(test7)[1]+dim(test8)[1],
              dim(test0)[1]+dim(test1)[1]+dim(test2)[1]+dim(test3)[1]+dim(test4)[1]+dim(test5)[1]+dim(test6)[1]+dim(test7)[1]+dim(test8)[1]+dim(test9)[1])


scoresTrain = c()
for(i in 0:9)
{
  score = 0
  for(j in (indexTrain[i+1]+1):(indexTrain[i+2]))
  {
    res = classify(hmm_train, feats[j,])
    #cat("Train : ", i, " => ", res, "\n")
    if(res == i)
    {
      score <- score +1
    }
  }
  scoresTrain = c(scoresTrain, score/(indexTrain[i+2]-indexTrain[i+1]))
  cat("\n\n")
}

cat("Scores Train : ", scoresTrain, "\n")

scoresTest = c()
for(i in 0:9)
{
  score = 0
  for(j in (indexTest[i+1]+1):(indexTest[i+2]))
  {
    res = classify(hmm_train, tests[j,])
    #cat("Test : ", i, " => ", res, "\n")
    if(res == i)
    {
      score <- score +1
    }
  }
  scoresTest = c(scoresTest, score/(indexTest[i+2]-indexTest[i+1]))
  cat("\n\n")
}

cat("Scores Test : ", scoresTest, "\n")
