loadFile <- function(data, nbState, matrice)
{
  train=dget(paste("data/", paste(data, nbState, matrice, "scoresTrain", sep='_'), sep=''))
  test=dget(paste("data/", paste(data, nbState, matrice, "scoresTest", sep='_'), sep=''))
  
  return(list(train=train, test=test))
}

graphOptimalVsUniform <- function(nbStates)
{
  a=loadFile("Data5X3", nbStates, "optimal")
  b=loadFile("Data5X4", nbStates, "optimal")
  c=loadFile("Dir8", nbStates, "optimal")
  d=loadFile("Dir16", nbStates, "optimal")
  
  e=loadFile("Data5X3", nbStates, "uniform")
  f=loadFile("Data5X4", nbStates, "uniform")
  g=loadFile("Dir8", nbStates, "uniform")
  h=loadFile("Dir16", nbStates, "uniform")

  jpeg(paste("graph/optimal_vs_uniform_", nbStates, ".jpg", sep=''))
  matplot(0:3, matrix(c(a$test$globalReco, b$test$globalReco, c$test$globalReco, d$test$globalReco, e$test$globalReco, f$test$globalReco, g$test$globalReco, h$test$globalReco), nrow=4), type = "l", col=c("red", "blue"), xlab="Data type", ylab="Global Recognition", xlim=c(0,3), ylim=c(0.70, 1), xaxt='n')
  axis(side=1, at=c(0,1,2,3), labels=c("Data5X3", "Data5X4", "Dir8", "Dir16"))
  
  dev.off()  
}

graphDigit <- function(nbStates, matrice)
{
  a=loadFile("Data5X3", nbStates, matrice)
  b=loadFile("Data5X4", nbStates, matrice)
  c=loadFile("Dir8", nbStates, matrice)
  d=loadFile("Dir16", nbStates, matrice)
  
  jpeg(paste("graph/digit_", nbStates, "_", matrice, ".jpg", sep=''))
  matplot(0:9, matrix(c(a$test$scores, b$test$scores, c$test$scores, d$test$scores), nrow=10), type="l", col=c("red", "green", "blue", "black"), ylim=c(0.4, 1), xaxt='n', xlab="Digits", ylab="Global Recognition")
  axis(side=1, at=0:9, labels=0:9)
  dev.off()  
}

graphNbStates <- function(type, matrice)
{
  a=loadFile(type, 3, matrice)
  b=loadFile(type, 6, matrice)
  c=loadFile(type, 9, matrice)
  
  jpeg(paste("graph/nbStates_", type, "_", matrice, ".jpg", sep=''))
  matplot(c(3,6,9), matrix(c(a$test$globalReco, b$test$globalReco, c$test$globalReco, a$train$globalReco, b$train$globalReco, c$train$globalReco), nrow=3), type="l", col=c("red", "blue"), xlab="Nomber of states", ylab="Global Recognition", xaxt='n', ylim=c(0.8,0.9))
  axis(side=1, at=c(3,6,9), labels=c(3,6,9))
  
  dev.off()
}

graphNbLongStates <- function(type, matrice)
{
  a=loadFile(type, 3, matrice)
  b=loadFile(type, 6, matrice)
  c=loadFile(type, 7, matrice)
  d=loadFile(type, 8, matrice)
  e=loadFile(type, 9, matrice)
  f=loadFile(type, 10, matrice)
  g=loadFile(type, 15, matrice)
  h=loadFile(type, 18, matrice)
  i=loadFile(type, 21, matrice)
  j=loadFile(type, 24, matrice)
  
  jpeg(paste("graph/nbLongStates_", type, "_", matrice, ".jpg", sep=''))
  matplot(c(3,6,7,8,9,10,15,18,21,24), matrix(c(a$test$globalReco, b$test$globalReco, c$test$globalReco, d$test$globalReco, e$test$globalReco, f$test$globalReco, g$test$globalReco, h$test$globalReco, i$test$globalReco, j$test$globalReco, 
                            a$train$globalReco, b$train$globalReco, c$train$globalReco, d$train$globalReco, e$train$globalReco, f$train$globalReco, g$train$globalReco, h$train$globalReco, i$train$globalReco, j$train$globalReco
                                ), nrow=10), type="l", col=c("red", "blue"), xlab="Nomber of states", ylab="Global Recognition", xaxt='n', ylim=c(0.82,0.9))
  axis(side=1, at=c(3,6,7,8,9,10,15,18,21,24), labels=c(3,6,7,8,9,10,15,18,21,24))
  
  dev.off()
}

graphOptimalVsUniform(3)
graphOptimalVsUniform(6)
graphOptimalVsUniform(9)

graphDigit(3, "optimal")
graphDigit(6, "optimal")
graphDigit(9, "optimal")

graphNbStates("Data5X3", "optimal")
graphNbStates("Data5X4", "optimal")
graphNbStates("Dir8", "optimal")
graphNbStates("Dir16", "optimal")

graphNbLongStates("Data5X4", "optimal")
