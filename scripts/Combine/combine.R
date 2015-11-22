source('../HMM/newHMM.r')
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

rTest = loadFiles('Test', 'Data5X4', '5_4')
tests = rTest$feats
indexTest = rTest$index

bestHmms = dget('../HMM/data/Data5X4_9_optimal_hmms')
probsHmm = classifyHMM(bestHmms, tests[1,])
print(probsHmm)

#TODO : Load RNN, classify RNN, get all probabilities for every number, then 2 choices :
# - Sum the probabilities for each number, then take the max
# - A Borda count (Voir ce que c'est)
