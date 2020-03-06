library('rsvd')
#Define matrix
A <- matrix(c(1,0,1,0,1,1,1,1,1,0,0,1,1,0,1,0,0,1,1,0,2,1,1,1,0,0,0,1,1,1,1,0,1),
            nrow = 11, 
            dimnames = list("terms" = c("t1", "t2", "t3", "t4", "t5", "t6", "t7", "t8", "t9", "t10", "t11"),
                            "docs" = c("d1", "d2", "d3")) )

#Decompose A Single Value Decomposition
decomposeA <- svd(A)


#Define query
Q = matrix(c(0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 1), nrow =11,
           dimnames = list("terms" = c("t1", "t2", "t3", "t4", "t5", "t6", "t7", "t8", "t9", "t10", "t11"),
                                       "query" = c("q1")) )

#Perform Calculations
inner.product <- t(Q) %*% A
inner.productDecompose <- t(Q) %*%  decomposeA$u %*% diag(decomposeA$d) %*% t(decomposeA$v)  # CORRECT


#rank 2
decomposeA2.u <- decomposeA$u[,c(1,2)]
#new coordinnates of each doc
decomposeA2.v <- decomposeA$v[,c(1,2)]
decomposeA2.d <- matrix(0, 2, 2)
decomposeA2.d[1,1] <- decomposeA$d[1]
decomposeA2.d[2,2] <- decomposeA$d[2]
A2 <- decomposeA2.u %*% decomposeA2.d %*% t(decomposeA2.v)

#Coordinates query
q2.aux <- t(Q) %*% (decomposeA2.u)

#1/d and guarantee that inf is 0
dInverse <- 1/decomposeA2.d
dInverse[which(!is.finite(dInverse))] <- 0

q2 <- q2.aux %*% dInverse

#alinea d
docs.coord <- matrix(decomposeA2.v, nrow = 3, dimnames = list("docs" = c("d1", "d2", "d3"),"coordinates" = c("x", "y")) )
query.coord <- matrix(q2, nrow = 1, dimnames = list("query" = c("q1"),"coordinates" = c("x", "y")) )

plotM <- matrix(0,nrow = 4, ncol = 2)
plotM[1,] <- decomposeA2.v[1,]
plotM[2,] <- decomposeA2.v[2,]
plotM[3,] <- decomposeA2.v[3,]
#plotM[1,] <- t(decomposeA2.v)[1,]
#plotM[2,] <- t(decomposeA2.v)[2,]
#plotM[3,] <- t(decomposeA2.v)[3,]
plotM[4,] <- q2

plot(x = plotM[,1], y = plotM[,2], 
     col = c("#E7B800", "#E7B800", "#E7B800","#FC4E07"), 
     xlim = c(-1,0.5), ylim = c(-1,1), 
     xlab = "x", ylab = "y",
     pch = c(17, 17, 17,18))
legend("topright", legend = c("doc1", "doc2", "doc3","query1"),
       col =  c("#E7B800", "#E7B800", "#E7B800", "#FC4E07"),
       pch = c(17, 17, 17,18) )

# alinea e
t(Q) %*% A2





