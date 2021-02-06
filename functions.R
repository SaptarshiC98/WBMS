library(igraph)
K=function(x,y,w,h){
  return(exp(-sum((x-y)^2*w)/h))
}

WBMS=function(X,h,lambda=1,tmax=30){
  n=dim(X)[1]
  p=dim(X)[2]
  K_matrix=matrix(0,n,n)
  w=rep(1/p,p)
  D=numeric(p)
  X1=X
  X2=X
  for(t in 1:20){
    for(i in 1:n){
      for(j in i:n){
        K_matrix[i,j]=K(X2[i,],X2[j,],w,h)
      }
    }
    for(i in 1:n){
      for(j in i:n){
        K_matrix[j,i]=K_matrix[i,j]
      }
    }
    for(i in 1:n){
      I=(1:n)[-i]
      s=sum(K_matrix[I,i])
      for(l in 1:p){
        X1[i,l]=sum(X2[I,l]*K_matrix[I,i])  
      }
      X1[i,]=X1[i,]/s
    }
    D=colSums((X-X1)^2)
    w=exp(-D/lambda)
    w=w/sum(w)
    X2=X1
    cat(t)
    cat('\n')
  }
  X1=X
  X2=X
  for(t in 1:tmax){
    for(i in 1:n){
      for(j in i:n){
        K_matrix[i,j]=K(X2[i,],X2[j,],w,h)
      }
    }
    for(i in 1:n){
      for(j in i:n){
        K_matrix[j,i]=K_matrix[i,j]
      }
    }
    for(i in 1:n){
      I=(1:n)[-i]
      s=sum(K_matrix[I,i])
      for(l in 1:p){
        X1[i,l]=sum(X2[I,l]*K_matrix[I,i])  
      }
      X1[i,]=X1[i,]/s
    }
    D=colSums((X-X1)^2)
    w=exp(-D/lambda)
    w=w/sum(w)
    X2=X1
    if(t%%50==0){
      cat(t)
      cat('\n')
    }
  }
  return(list(X2,w))
}
U2clus=function(U,epsa=10^(-5)){
  n=dim(U)[1]
  A=matrix(0,n,n)
  for(i in 1:n){
    for(j in 1:n){
      if(sqrt(sum((U[i,]-U[j,])^2))<epsa){
        A[i,j]=1
      }
    }
  }
  g=graph_from_adjacency_matrix(A,'undirected')
  clu <- components(g)
  return(clu$membership)
}
