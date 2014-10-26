CreateUniformCorrelationMatrix <-
function(Dimension, OffDiagVal){
### Create a symmetric matrix with ones on the diagonal
### and a presribed value on the off - diagonal entries
  matrix(OffDiagVal, Dimension, Dimension) + diag(1 - OffDiagVal, Dimension)
}
