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

loadFiles <- function(type, X, Y)
{
  file0 = Load_Obs(paste('../../data/Data', X, 'X', Y, '/', type, '_compute_symbol_', X, '_', Y, 'Digit0.txt', sep=''))
  file1 = Load_Obs(paste('../../data/Data', X, 'X', Y, '/', type, '_compute_symbol_', X, '_', Y, 'Digit1.txt', sep=''))
  file2 = Load_Obs(paste('../../data/Data', X, 'X', Y, '/', type, '_compute_symbol_', X, '_', Y, 'Digit2.txt', sep=''))
  file3 = Load_Obs(paste('../../data/Data', X, 'X', Y, '/', type, '_compute_symbol_', X, '_', Y, 'Digit3.txt', sep=''))
  file4 = Load_Obs(paste('../../data/Data', X, 'X', Y, '/', type, '_compute_symbol_', X, '_', Y, 'Digit4.txt', sep=''))
  file5 = Load_Obs(paste('../../data/Data', X, 'X', Y, '/', type, '_compute_symbol_', X, '_', Y, 'Digit5.txt', sep=''))
  file6 = Load_Obs(paste('../../data/Data', X, 'X', Y, '/', type, '_compute_symbol_', X, '_', Y, 'Digit6.txt', sep=''))
  file7 = Load_Obs(paste('../../data/Data', X, 'X', Y, '/', type, '_compute_symbol_', X, '_', Y, 'Digit7.txt', sep=''))
  file8 = Load_Obs(paste('../../data/Data', X, 'X', Y, '/', type, '_compute_symbol_', X, '_', Y, 'Digit8.txt', sep=''))
  file9 = Load_Obs(paste('../../data/Data', X, 'X', Y, '/', type, '_compute_symbol_', X, '_', Y, 'Digit9.txt', sep=''))
  
  indexFile=c(0, 
              dim(file0)[1], 
              dim(file0)[1]+dim(file1)[1],
              dim(file0)[1]+dim(file1)[1]+dim(file2)[1],
              dim(file0)[1]+dim(file1)[1]+dim(file2)[1]+dim(file3)[1],
              dim(file0)[1]+dim(file1)[1]+dim(file2)[1]+dim(file3)[1]+dim(file4)[1],
              dim(file0)[1]+dim(file1)[1]+dim(file2)[1]+dim(file3)[1]+dim(file4)[1]+dim(file5)[1],
              dim(file0)[1]+dim(file1)[1]+dim(file2)[1]+dim(file3)[1]+dim(file4)[1]+dim(file5)[1]+dim(file6)[1],
              dim(file0)[1]+dim(file1)[1]+dim(file2)[1]+dim(file3)[1]+dim(file4)[1]+dim(file5)[1]+dim(file6)[1]+dim(file7)[1],
              dim(file0)[1]+dim(file1)[1]+dim(file2)[1]+dim(file3)[1]+dim(file4)[1]+dim(file5)[1]+dim(file6)[1]+dim(file7)[1]+dim(file8)[1],
              dim(file0)[1]+dim(file1)[1]+dim(file2)[1]+dim(file3)[1]+dim(file4)[1]+dim(file5)[1]+dim(file6)[1]+dim(file7)[1]+dim(file8)[1]+dim(file9)[1])

  #indexFile=c(0,10,20,30,40,50,60,70,80,90,100)
  indexFile
  feats=rbind(file0, file1, file2, file3, file4, file5, file6, file7, file8, file9)
  #feats=rbind(file0[1:10,], file1[1:10,], file2[1:10,], file3[1:10,], file4[1:10,], file5[1:10,], file6[1:10,], file7[1:10,], file8[1:10,], file9[1:10,])
  
  return(list(feats=feats, index=indexFile))
}

computeScores <- function(hmms, feats, indexFeats)
{
  scores = c()
  for(i in 0:9)
  {
    score = 0
    for(j in (indexFeats[i+1]+1):(indexFeats[i+2]))
    {
      res = classify(hmms, feats[j,])
      if(res == i)
      {
        score <- score +1
      }
    }
    scores = c(scores, score/(indexFeats[i+2]-indexFeats[i+1]))
  }

  return(scores)
}

rTrain = loadFiles('Train', 5, 3)
feats = rTrain$feats
indexTrain = rTrain$index

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

cat("\n\n")

rTest = loadFiles('Test', 5, 3)
tests = rTest$feats
indexTest = rTest$index

cat("\n\n")

scoresTrain = computeScores(hmm_train, feats, indexTrain)
cat("Scores Train : ", scoresTrain, "\n")

scoresTest = computeScores(hmm_train, tests, indexTest)
cat("Scores Test : ", scoresTest, "\n")
