nbc <- function(dataTable, y){
  #Provide check
  #make sure datatable is a df or matrix
  #make sure that y is the same length as datatable columns
  
  
  #calculating prior probabilities
  prior <- c()
  sumOfVariables <- c()
  total <- nrow(dataTable)
  
  numOfHam <-sum(y)
  numOfSpam <- total - numOfHam
  
  prior[1] <- numOfSpam/total
  prior[2] <- numOfHam/total
  
  #Class conditional probabilities
  classConditionalsSPAM <- c()
  classConditionalsHAM <- c()
  
  for(i in 1:nrow(dataTable)){
    variableAndLabelTotal <- 0
    for(j in total) {
      if(y[i] == 1 && dataTable[j,i] == 1) {
        variableAndLabelTotal++
          append(classConditionalsSPAM, variableAndLabelTotal/total)
        append(classConditionalsHAM, (total - variableAndLabelTotal)/total)
      }
    }
  }
  
  #Posterior probabilities
  posteriorSPAM <- c()
  posteriorHAM <- c()
  
  for(i in length(y)){
    for(j in ncol(dataTable)){
      observationConditionals <- c()
      observationConditionalsHAM <- c()
      if(dataTable[i,j] == 1){
        append(observationConditionals, classConditionalsSPAM[j])
      }
      else
        append(observationConditionalsHAM, classConditionalsHAM[j])
    }
    for(k in length(observationConditionals)){
      postProbSPAM <- postProbSPAM*observationConditionals[k]
    }
    for(k in length(observationConditionals)){
      postProbHAM <- postProbHAM*observationConditionalsHAM[k]
    }
    postProbSPAM <- (postProbSPAM*prior[1])
    posteriorSPAM[i] <- postProbSPAM 
    postProbHAM <- (postProbHAM*prior[2])
    posteriorHAM[i] <-postProbHAM
  }
  
  
  #Classify
  # 0 denoting SPAM, 1 being not SPAM
  classification <- c()
  for(i in 1:length(y)){
    if(posteriorSPAM[i] > 0.5) {
      classification <- c(classification, 0)
    }
    else {
      classification <- c(classification, 1)
    }
  }
  
  #Misclassification
  totalCorrect <- 0
  for(i in length(classification)){
    if(classification[i] == y[i])
      totalCorrect++
      misclassified <- 1-totalCorrect/total
  }
  
  #add to list of outputs
  return(list(prior, classConditionalsSPAM, classConditionalsHAM, posteriorSPAM, classification, misclassified))
}
