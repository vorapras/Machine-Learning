###//////////////////////////////////////////////////////////////////////////###
# Generate 50 Simulated Datasets
###//////////////////////////////////////////////////////////////////////////###

#Import library first
library(mvtnorm)

# Define covariance matrix based on the assignment
numpar = 8
covar = matrix(0,ncol = numpar,nrow = numpar)
# Let sigma[i] = i for i =1,2,...,8  and include correlation
for(j in 1:numpar){
  for(i in 1:numpar){
    if(i==j){
      covar[i,j] = 1
    }
    else{
      covar[i,j] = 0.5^abs(i-j)*1
    }
  }
}

# Preallocating space for 50 simulated data sets
ndataset = 50
numtrain = 20
numvalid = 20
numtest = 200
numobs = numtrain+numvalid+numtest
# Response Variable
Ylist = list(ndataset)
Ylist_train = list(ndataset)
Ylist_validate = list(ndataset)
Ylist_post_train = list(ndataset)
Ylist_test  = list(ndataset)
# 8 predictors
Xlist = list(ndataset)
Xlist_train = list(ndataset)
Xlist_validate = list(ndataset)
Xlist_post_train = list(ndataset)
Xlist_test  = list(ndataset)

# Define sigma = 3 beta = (3, 1.5, 0, 0, 2, 0, 0, 0)T and err = N(0,1)
sigma = 3
beta_true = matrix(c(3,1.5,0,0,2,0,0,0),nrow = numpar,ncol = 1)

# Generate 50 data sets
for(i in 1:ndataset){
  # Randomize each time
  set.seed(i)
  # Define epsilon
  eps = as.matrix(rnorm(numobs, mean = 0,sd = 1))
  # Create simulated data with mean = 0 
  Xlist[[i]] = rmvnorm(n = numobs,mean = rep(0,numpar),sigma = covar)
  Ylist[[i]] = Xlist[[i]]%*%beta_true+sigma*eps
  # Seperated Data
  Data_Y = Ylist[[i]]
  Data_X = Xlist[[i]]
  # Randomly selected data
  train = sample(numobs,numtrain)
  validate = sample((1:numobs)[-train],numvalid)
  post_train = c(train,validate)
  test = (1:numobs)[-post_train]
  # Store Y
  Ylist_train[[i]] = Data_Y[train,] 
  Ylist_validate[[i]] = Data_Y[validate,] 
  Ylist_post_train[[i]] = Data_Y[post_train,] 
  Ylist_test[[i]]  = Data_Y[test,]  
  # Store X
  Xlist_train[[i]] = Data_X[train,] 
  Xlist_validate[[i]] = Data_X[validate,]
  Xlist_post_train[[i]] = Data_X[post_train,] 
  Xlist_test[[i]]  = Data_X[test,]  
}

###//////////////////////////////////////////////////////////////////////////###
# Build Function From Coordinate Descent Algorithm For Solving Lasso Problem
###//////////////////////////////////////////////////////////////////////////###

# Coordinate Descent Algorithm for Lasso
lasso_reg = function(x,y,lambda){
  
  # Initialize all parameters
  p = ncol(x)
  n = nrow(x)
  threshold = 10^(-4)
  iteration=0
  
  # Preallocating space for updating coefficient
  beta_tmp = rep(sqrt(threshold),p) 
  beta = rep(0,p)
  
  # Check if it converges or not??
  while(norm(beta_tmp-beta,type="2")>=threshold){
    beta_tmp = beta
    # Cycle through all parameters
    for(j in 1:p){
      resid = y-x[,-j]%*%beta[-j]
      z  = mean(x[,j]*resid)
      beta[j] = sign(z)*max((abs(z)-lambda),0)
    }
    # Count iteration number to be converge
    iteration=iteration+1
    if(iteration > 100) {
      beta = rep(-1,p)  # Constraint not to be diverge
    }
  }
  return_object = list("coef"= beta, "iter"=iteration)
  return(return_object)
}


# Coordinate Descent Algorithm for Elastic Net Penalty
elastic_net = function(x,y,lambda1,lambda2){
  # Initialize all parameters
  p = ncol(x)
  n = nrow(x)
  threshold = 10^(-4)
  iteration=0
  
  # Preallocating space for updating coefficient
  beta_tmp = rep(sqrt(threshold),p) 
  beta = rep(0,p)
  
  # Check if it converges or not??
  while(norm(beta_tmp-beta,type="2")>=threshold){
    beta_tmp = beta
    # Cycle through all parameters
    for(j in 1:p){
      resid = y-x[,-j]%*%beta[-j]
      z  = mean(x[,j]*resid)
      beta[j] = (1/(1+2*lambda1))*sign(z)*max((abs(z)-lambda2),0)
    }
    # Count iteration number to be converge
    iteration=iteration+1
    if(iteration > 100) {
      beta = rep(-1,p)  # Constraint not to be diverge
    }
  }
  return_object = list("coef"= beta, "iter"=iteration)
  return(return_object)
}


###//////////////////////////////////////////////////////////////////////////###
# How to select the regularization parameters for the lasso problem 
###//////////////////////////////////////////////////////////////////////////###

# Training data set with different lambda value
# Initialize grid search and preallocating space for storing parameters
grid = seq(0, 4,length = 50)
MSE_lasso  = rep(0,length(grid))
numconv = 0
# Loop for lambda grid search
for(i in 1:length(grid)){
  # Fitting model from training data set
  avg_mse = matrix(0, nrow = 1, ncol = ndataset)
  lambda = grid[i]
  # Compare MSE
  for(j in 1:ndataset){
    # Setting input parameters
    x = Xlist_train[[j]]
    y = Ylist_train[[j]]
    y = as.matrix(y)
    beta = lasso_reg(x,y,lambda)$coef
    numconv = numconv+lasso_reg(x,y,lambda)$iter
    # Use coefficients from training data to predict Y in validation data set
    predictY = Xlist_validate[[j]]%*%beta
    # Caluclate MSE for 50 simulated data set in each selected lamda value
    avg_mse[1,j] = mean((predictY-Ylist_validate[[j]])^2)
  }
  MSE_lasso[i] = mean(avg_mse)
}
# Average number of convergence
avg_conv_lasso = numconv/(length(grid)*ndataset)
# Plot MSE VS Lambda value from cross validation 
plot(grid, MSE_lasso, type="b", xlab ="lambda", ylab = "MSE",main = "Lasso")
# Validation set approach to select best lambda from lowest MSE
lambda_lasso = grid[which.min(MSE_lasso)]

# After selecting the best lambda, test MSE in testing data set
lasso_coef = matrix(0,nrow = numpar,ncol=ndataset)
avg_mse = matrix(0, nrow = 1, ncol = ndataset)
for(j in 1:ndataset){
  # Setting input parameters
  x = Xlist_post_train[[j]]
  y = Ylist_post_train[[j]]
  beta = lasso_reg(x,y,lambda_lasso)$coef
  lasso_coef[,j] = beta
  # Use coefficients from training data to predict Y in validation data set
  X_test = Xlist_test[[j]]
  Y_test = Ylist_test[[j]]
  predictY = X_test%*%beta
  # Caluclate MSE for 50 simulated data set in each selected lamda value
  avg_mse[1,j] = mean((predictY-Y_test)^2)
}
# Calculate average mean square error from 50 simulated data set
Lasso_Coef = lasso_coef
# Average number of variable selection and bias from the true beta
numvar = 0
biasdiff = 0
for(i in 1:50){
  biasdiff = biasdiff+sum(sqrt((Lasso_Coef[,i]-beta_true)^2))
  numvar = numvar+length(which(Lasso_Coef[,i]!=0))
}
avg_numvar_lasso = numvar/50
avg_biasdiff_lasso = biasdiff/50
# Prediction accuracy
Test_MSE_Lasso = cbind(lambda_lasso,mean(avg_mse))


###//////////////////////////////////////////////////////////////////////////###
# How to select the regularization parameters for the elastic net problem 
###//////////////////////////////////////////////////////////////////////////###

# Training data set with different lambda value
# Preallocating space for storing lambda1 and lambda2
MSE_Data = matrix(0,ncol = 3,nrow = length(grid)^2)
index = 1
numconv = 0
# Double Loop for lambda grid search
for(i in 1:length(grid)){
  for(j in 1:length(grid)){
    # Fitting model from training data set
    avg_mse = matrix(0,ncol = ndataset, nrow = 1)
    lambda1 = grid[i]
    lambda2 = grid[j]
    # Compare MSE
    for(k in 1:ndataset){
      # Setting input parameters
      x = Xlist_train[[k]]
      y = Ylist_train[[k]]
      y = as.matrix(y)
      beta_elas = elastic_net(x,y,lambda1,lambda2)$coef
      numconv = numconv+lasso_reg(x,y,lambda)$iter
      # Use coefficients from training data to predict Y in validation data set
      predictY = Xlist_validate[[k]]%*%beta_elas
      # Caluclate MSE for 50 simulated data set in each selected lamda1 and lambda2 value
      avg_mse[1,k] = mean((predictY-Ylist_validate[[k]])^2)
    }
    #Store each value of lambda1 and lambda2 value
    MSE_Data[index,]  = cbind(lambda1,lambda2, mean(avg_mse))
    index = index+1
  }
}
# Average number of convergence
avg_conv_elastic = numconv/(length(grid)^2*ndataset)
# 3D Plot of MSE
library(scatterplot3d)
scatterplot3d(x = MSE_Data[,1], y = MSE_Data[,2], z = MSE_Data[,3],  main="Elastic Net", xlab="Lambda1", ylab="Lambda2", zlab="MSE")
# Validation set approach to select best lambda from lowest MSE
# Find the lambda1 and lambda2 which have the lower MSE than the Lasso
lambda_elasnet = MSE_Data[which(MSE_Data[,3]< MSE_lasso[which.min(MSE_lasso)]),]


# After selecting the best lambda, test MSE in testing data set
elasnet_coef = matrix(0,ncol = ndataset,nrow = numpar)
MSE_elasnet  = matrix(0,ncol = 3, nrow = nrow(lambda_elasnet))
Elasnet_Coef = list(nrow(lambda_elasnet))

for(i in 1: nrow(lambda_elasnet)){
  for(j in 1:ndataset){
    # Setting input parameters
    x = Xlist_post_train[[j]]
    y = Ylist_post_train[[j]]
    beta = elastic_net(x,y,lambda_elasnet[i,1],lambda_elasnet[i,2])$coef
    elasnet_coef[,j] = beta
    # Use coefficients from training data to predict Y in validation data set
    X_test = Xlist_test[[j]]
    Y_test = Ylist_test[[j]]
    predictY = X_test%*%beta
    # Caluclate MSE for 50 simulated data set in each selected lamda value
    avg_mse[1,j] = mean((predictY-Y_test)^2)
  }
  # Calculate average mean square error from 50 simulated data set
  Elasnet_Coef[[i]] = elasnet_coef
  MSE_elasnet[i,] = cbind(lambda_elasnet[i,1],lambda_elasnet[i,2],mean(avg_mse))
}
# Rank MSE value
Rank = sort(MSE_elasnet[,3],decreasing = FALSE)

# First Rank
Test_MSE_Elasnet_1 = MSE_elasnet[which(MSE_elasnet[,3]== Rank[1]),]
Elas_Coef_1 = Elasnet_Coef[[which(MSE_elasnet[,3]== Rank[1])]]
# Average number of variable selection and bias from the true beta
numvar = 0
biasdiff = 0
for(i in 1:50){
  biasdiff = biasdiff+sum(sqrt((Elas_Coef_1[,i]-beta_true)^2))
  numvar = numvar+length(which(Elas_Coef_1[,i]!=0))
}
avg_numvar_elas_1 = numvar/50
avg_biasdiff_elas_1 = biasdiff/50

# Second Rank
Test_MSE_Elasnet_2 = MSE_elasnet[which(MSE_elasnet[,3]== Rank[2]),]
Elas_Coef_2 = Elasnet_Coef[[which(MSE_elasnet[,3]== Rank[2])]]
# Average number of variable selection and bias from the true beta
numvar = 0
biasdiff = 0
for(i in 1:50){
  biasdiff = biasdiff+sum(sqrt((Elas_Coef_2[,i]-beta_true)^2))
  numvar = numvar+length(which(Elas_Coef_2[,i]!=0))
}
avg_numvar_elas_2 = numvar/50
avg_biasdiff_elas_2 = biasdiff/50

# Third Rank
Test_MSE_Elasnet_3 = MSE_elasnet[which(MSE_elasnet[,3]== Rank[3]),]
Elas_Coef_3 = Elasnet_Coef[[which(MSE_elasnet[,3]== Rank[3])]]
# Average number of variable selection and bias from the true beta
numvar = 0
biasdiff = 0
for(i in 1:50){
  biasdiff = biasdiff+sum(sqrt((Elas_Coef_3[,i]-beta_true)^2))
  numvar = numvar+length(which(Elas_Coef_3[,i]!=0))
}
avg_numvar_elas_3 = numvar/50
avg_biasdiff_elas_3 = biasdiff/50

# Fourth Rank
Test_MSE_Elasnet_4 = MSE_elasnet[which(MSE_elasnet[,3]== Rank[4]),]
Elas_Coef_4 = Elasnet_Coef[[which(MSE_elasnet[,3]== Rank[4])]]
# Average number of variable selection and bias from the true beta
numvar = 0
biasdiff = 0
for(i in 1:50){
  biasdiff = biasdiff+sum(sqrt((Elas_Coef_4[,i]-beta_true)^2))
  numvar = numvar+length(which(Elas_Coef_4[,i]!=0))
}
avg_numvar_elas_4 = numvar/50
avg_biasdiff_elas_4 = biasdiff/50

# Fifth Rank
Test_MSE_Elasnet_5 = MSE_elasnet[which(MSE_elasnet[,3]== Rank[5]),]
Elas_Coef_5 = Elasnet_Coef[[which(MSE_elasnet[,3]== Rank[5])]]
# Average number of variable selection and bias from the true beta
numvar = 0
biasdiff = 0
for(i in 1:50){
  biasdiff = biasdiff+sum(sqrt((Elas_Coef_5[,i]-beta_true)^2))
  numvar = numvar+length(which(Elas_Coef_5[,i]!=0))
}
avg_numvar_elas_5 = numvar/50
avg_biasdiff_elas_5 = biasdiff/50

