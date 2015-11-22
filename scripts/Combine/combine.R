source('../HMM/newHMM.r')
source('../NN/ANN.r')
source('../tools/usefullTools.r')

classifyHMM <- function(hmms, obs)
{
   lls=c()
   for(i in c(0,1,2,3,4,5,6,7,8,9))
   {
      lls = c(lls, loglikelihood(hmms[i+1,], obs))
   }

   return(lls)
}

loadFiles <- function(type, dir, file)
{
   file0 = Load_Obs(paste('../../data/',dir, '/', type, '_compute_symbol_', file, 'Digit0.txt', sep=''))
   file1 = Load_Obs(paste('../../data/',dir, '/', type, '_compute_symbol_', file, 'Digit1.txt', sep=''))
   file2 = Load_Obs(paste('../../data/',dir, '/', type, '_compute_symbol_', file, 'Digit2.txt', sep=''))
   file3 = Load_Obs(paste('../../data/',dir, '/', type, '_compute_symbol_', file, 'Digit3.txt', sep=''))
   file4 = Load_Obs(paste('../../data/',dir, '/', type, '_compute_symbol_', file, 'Digit4.txt', sep=''))
   file5 = Load_Obs(paste('../../data/',dir, '/', type, '_compute_symbol_', file, 'Digit5.txt', sep=''))
   file6 = Load_Obs(paste('../../data/',dir, '/', type, '_compute_symbol_', file, 'Digit6.txt', sep=''))
   file7 = Load_Obs(paste('../../data/',dir, '/', type, '_compute_symbol_', file, 'Digit7.txt', sep=''))
   file8 = Load_Obs(paste('../../data/',dir, '/', type, '_compute_symbol_', file, 'Digit8.txt', sep=''))
   file9 = Load_Obs(paste('../../data/',dir, '/', type, '_compute_symbol_', file, 'Digit9.txt', sep=''))

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

computeAllScores <- function(hmms, nndigit, feats, indexFeats, combine="sum")
{
   scores = c()
   sum = 0
   for(i in 0:9)
   {
      score = 0
      for(j in (indexFeats[i+1]+1):(indexFeats[i+2]))
      {
         if(combine == "sum")
         {
            res = classifySum(hmms, nndigits, feats[j,])
         }
         else
         {
            res = classifyBorda(hmms, nndigits, feats[j,])
         }
         if(res == i)
         {
            score <- score +1
         }
      }
      sum = sum + score
      scores = c(scores, score/(indexFeats[i+2]-indexFeats[i+1]))
   }

   globalReco = sum/indexFeats[11]

   return(list(globalReco=globalReco, scores=scores))
}

classifySum <- function(hmms, nndigits, obs)
{
   reshmm <- sapply(classifyHMM(hmms, obs), function(x) { return(exp(x)) })
   sumhmm <- sum(reshmm)
   reshmm <- sapply(reshmm, function(x) { return(x/sumhmm)})
   resnn <- predict(nndigit, createFeatures2(obs, 5, 4))
   res <- resnn + reshmm
   
   maxl = max(res)
  
   for(i in c(0,1,2,3,4,5,6,7,8,9))
   {
     if(maxl == res[i+1])
       return(i)
   }
}

classifyBorda <- function(hmms, nndigits, obs)
{
   return(0)
}



rTest = loadFiles('Test', 'Data5X4', '5_4')
tests = rTest$feats
indexTest = rTest$index

bestHmms = dget('../HMM/data/Data5X4_7_optimal_hmms')
probsHmm = classifyHMM(bestHmms, tests[1,])
print(probsHmm)

probsNN = predict(nndigit, createFeatures2(tests[1,], 5, 4))
print(probsNN)

computeAllScores(bestHmms, nndigit, tests, indexTest)

#TODO : Load RNN, classify RNN, get all probabilities for every number, then 2 choices :
# - Sum the probabilities for each number, then take the max
# - A Borda count (Voir ce que c'est)


