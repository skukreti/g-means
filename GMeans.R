# G-means: Hamerly, Elkan: Learning the k in k means
# Author: Sarthak Kukreti
# Assumptions: Needs "nortest" library to run the anderson darling test
library(nortest)
library(car)

# G Means: takes as input (2D) data set and outputs appropriate k means model

gmeans <- function(dataset, alpha = 0.0001) {
  # 1. Calculate mean (Xbar, Ybar, ...) (cannot assume anything about the data set; also remove NAs/Missing data)
  #c_0 <- colMeans(dataset, na.rm = TRUE)
  dataset<-as.matrix(dataset)
  
  c_0 <- apply(dataset,2,mean)
  # 2. Create empty center list
  c_list <- matrix(, nrow = 0, ncol = dim(dataset)[2])
  
  # Canonical way of appending to centers list
  c_list <- rbind(c_list, c_0)
  
  flag = 1
  iterations = 0
  while(flag == 1)
  {
    flag = 0
    # Perform kmeans get C_list
    if(dim(c_list)[1] > 1)
      k<-kmeans(dataset, centers = c_list)
    else
      k<-kmeans(dataset, centers = 1)
    c_list_new <- matrix(, nrow = 0, ncol = dim(dataset)[2]) # willl append to this list at every point
    c_list<-k$centers
    cluster_vector<-k$cluster
    for( i in seq(1,dim(c_list)[1]))
    {
      # First get data: Indicator variable
      I <- sapply(cluster_vector, function(x) x == i)
      
      sub_cluster <- as.matrix(dataset[I,])
      if(dim(sub_cluster)[1] <= 10) { # One of the conditions of A-D Test
        if(dim(sub_cluster)[1] > 0)
          c_list_new<-rbind(c_list_new, c_list[i,])
        next
      }
      
      if(dim(dataset)[2] > 1) {
        sub_pr<-prcomp(sub_cluster, center = TRUE, scale=TRUE)
      
        # Take principal component and sdev, multiply, add subtract
        prin_comp<-sub_pr$rotation[,1] #/(sub_pr$rotation[,1] %*% t(sub_pr$rotation[,1])) # Normalized principle component
        #prin_comp<- prin_comp/as.integer(sqrt(sum(prin_comp^2))) # Already normalized 
        lambda<-sub_pr$sdev[1]
      
        sub_c_list <- matrix(, nrow = 0, ncol = dim(dataset)[2])
        sub_c_list <- rbind(sub_c_list, c_list[i,] + lambda * sqrt(2/pi) * prin_comp)
        sub_c_list <- rbind(sub_c_list, c_list[i,] - lambda * sqrt(2/pi) * prin_comp)
      
        sub_k<- kmeans(sub_cluster, centers = sub_c_list)
      
        sub_centers <- sub_k$centers
        sub_princomp <- sub_centers[2,] - sub_centers[1,]
        sub_princomp<- sub_princomp/as.integer(sqrt(sum(sub_princomp^2)))
      
        # Project data into sub_princomp and use AD test
        projected_data <- as.matrix(sub_cluster) %*% as.matrix(sub_princomp) # use principle component of new centers
      
        
      }
      else
      {
        projected_data<-sub_cluster
        sub_c_list <- matrix(, nrow = 0, ncol = dim(dataset)[2])
        lambda <- sd(sub_cluster)
        sub_c_list <- rbind(sub_c_list, c_list[i,] + lambda * sqrt(2/pi) )
        sub_c_list <- rbind(sub_c_list, c_list[i,] - lambda * sqrt(2/pi) )
        sub_k<- kmeans(sub_cluster, centers = sub_c_list)
        
        sub_centers <- sub_k$centers
      }
      
      # Scale X so that the mean and variance is 0 and 1 respenctively
      projected_data <- projected_data - mean(projected_data)
      projected_data <- projected_data/as.double(sqrt(var(projected_data)))
      
      test_result <- ad.test(projected_data)
      
      # Depending on test result append to new list
      if(test_result$p.value > alpha)
        c_list_new<-rbind(c_list_new, c_list[i,])
      else {
        c_list_new<-rbind(c_list_new, sub_centers[1,])
        c_list_new<-rbind(c_list_new, sub_centers[2,])
        #flag<-1
      }
    }
    # If centers list has changed, retry with new centers
    if(dim(c_list_new)[1] > dim(c_list)[1])
      flag <- 1
    c_list <- c_list_new
    iterations = iterations + 1
    #print(iterations)
  }
  # Return kmeans equivalent of the centers
  kmeans(dataset, centers = c_list)
  # or instead return list of centers
  # c_list
}

# Visualizes the gmeans clusters and ellipsoids (95%)
gmeans_visualize <- function(d, gmeans_model, alpha = 0.0001, sig = 0.95) {
  d<-as.matrix(d)
  if(dim(d)[2] >= 2) {
    dataEllipse(d[,1], d[,2], xlab="Dimension 1", ylab="Dimension 2", main=paste("Scatter and ellipsoid plot for alpha = ", alpha), center.pch = 19,pch = rep(1,dim(gmeans_model$centers)[1]), groups=as.factor(gmeans_model$cluster), group.labels = levels(as.factor(gmeans_model$cluster)), levels=sig, col = sample(rainbow(dim(gmeans_model$centers)[1]), replace = FALSE))
    
  } else {
    dataEllipse(d[,1], d[,1], xlab="Dimension 1", ylab="Dimension 1", main=paste("Scatter and ellipsoid plot for alpha = ", alpha), center.pch = 19,pch = rep(1,dim(gmeans_model$centers)[1]), groups=as.factor(gmeans_model$cluster), group.labels = levels(as.factor(gmeans_model$cluster)), levels=sig, col = sample(rainbow(dim(gmeans_model$centers)[1]), replace = FALSE))
  }
}
  