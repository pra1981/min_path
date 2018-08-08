#dijkstra算法
##输入数据集
dijkstra <- function(data_set,i){
  n <- nrow(data_set)
  dm <- as.matrix(dist(data_set,diag = TRUE))
  juli <- NULL
  path <- as.list(rep(i,n))
  visit <- rep(0,n)
  visit[i] <- 1
  juli = dm[i,]
  juli[i] <- 0
  for(j in 1:n){
    min_cost <- 1/0
    for(t in 1:n){
      if(visit[t]==0&juli[t]<min_cost){
        min_cost <-  juli[t]
        min_cost_index  <-  t
      }
    }
    visit[min_cost_index] <- 1
    for(k in 1:n){
      if (dm[min_cost_index,k]!=1/0&dm[min_cost_index,k] + min_cost < juli[k]){
        juli[k] = dm[min_cost_index,k] + min_cost
        path[[k]] <- c(path[[min_cost_index]],min_cost_index)
      }
    }
  }
  for(l in 1:n){
    path[[l]] <- c(path[[l]],l)
  }
  return(list(juli,path))
}
##输入邻接矩阵
dijkstra <- function(dm,i){
  n <- nrow(dm)
  juli <- NULL
  path <- as.list(rep(i,n))
  visit <- rep(0,n)
  visit[i] <- 1
  juli = dm[i,]
  juli[i] <- 0
  for(j in 1:n){
    min_cost <- 1/0
    for(t in 1:n){
      if(visit[t]==0&juli[t]<min_cost){
        min_cost <-  juli[t]
        min_cost_index  <-  t
      }
    }
    visit[min_cost_index] <- 1
    for(k in 1:n){
      if (dm[min_cost_index,k]!=1/0&dm[min_cost_index,k] + min_cost < juli[k]){
        juli[k] = dm[min_cost_index,k] + min_cost
        path[[k]] <- c(path[[min_cost_index]],min_cost_index)
      }
    }
  }
  for(l in 1:n){
    path[[l]] <- c(path[[l]],l)
  }
  return(list(juli,path))
}