source('functions.R') # Read the Functions

# Generate a toy data with 100 points
# Number of relevant features = 2.
# Number of irrelevant features = 30.

X1=matrix(rnorm(100*2),100,2)
X2=matrix(rnorm(100*2,5),100,2)
X=rbind(X1,X2)
for(i in 1:30){
  X=cbind(X,rnorm(200))
}

# Normalise the data

p=dim(X)[2]
for(i in 1:p){
  X[,i]=(X[,i]-mean(X[,i]))/sd(X[,i])
}
plot(X) # Plot

# Run the WBMS function

l=WBMS(X,0.1,lambda = 10)

# plot the centroids
points(l[[1]],col=2,pch=19)

# Print the Feature Weights
l[[2]]

# Find the clusters 
label=U2clus(l[[1]])

# Plot color coded with the cluster labels

plot(X,col=label)
