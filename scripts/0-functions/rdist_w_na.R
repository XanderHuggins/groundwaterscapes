rdist.w.na <- function(X,Y)
{
  if (!is.matrix(X)) 
    X = as.matrix(X)
  if (!is.matrix(Y)) 
    Y = as.matrix(Y)
  distances <- matrix(pdist(X,Y)@dist, ncol=nrow(X), byrow = TRUE)
  #count NAs
  na.count <- sapply(1:nrow(X),function(i){rowSums(is.na(Y) | is.na(X[i,]))})
  #scaling to number of cols
  distances * sqrt(ncol(X)/(ncol(X) - na.count))
}

# with thanks here to:
# https://stackoverflow.com/questions/22785475/does-a-function-like-dist-rdist-exist-which-handles-nas