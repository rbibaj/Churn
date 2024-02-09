########

GetCustomerChurnProb <- function(data, CustId){
  if(any(data[,CustomerId]==CustId)){
    churn_prob_model <- glm(data$Exited ~ data$CreditScore + data$Gender + data$Age + data$Age + data$Tenure + data$Balance + data$NumOfProducts + data$HasCrCard + data$IsActiveMember + data$EstimatedSalary, family="binomial")
    churn_pred <- predict(churn_prob_model, data, type="response")
    data <- cbind(data, churn_pred)
    return(data[CustomerId==CustId,churn_pred])
  } else {
    stop("CustomerID does not exist")
  }
}

#GetCustomerChurnProb(full_data, 15662641)
#GetCustomerChurnProb(full_data, 11111111111)