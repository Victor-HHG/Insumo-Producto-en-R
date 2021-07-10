# Modelo Insumo-Producto en R

# install.packages("readxl")
library("readxl")


datosExcel <- read_excel("Datos\\MIP pxp Domestica 3 sectores.xlsx", sheet = "2018")

n <- 3

Z <- as.matrix( datosExcel[ 1:n , (1+1):(1+n) ] )
F_matrix <- as.matrix( datosExcel[ 1:n , (1+1+n):ncol(datosExcel) ] )

f <- 1:n
for(i in 1:n){
  f[i] = sum(F_matrix[i,])
}

x <- 1:n
for(i in 1:n){
  x[i] = sum(Z[i,])+f[i]
}

A <- Z %*% solve(diag(x))

L <- solve(diag(n)-A)

x - L%*%f
