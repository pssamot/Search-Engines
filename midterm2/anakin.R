#Define matrix
indexWords <- c("windows", "azure", 
                "cloud", "microsoft", 
                "chairman", "bill", 
                "gates", "retirement", 
                "morning", "success", 
                "attributed", "successful",
                "sunrise", "garden", 
                "wooden", "frames",
                "trees","flowers")

A <- matrix(c(1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
              0,0,0,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,
              1,0,0,0,0,0,1,0,0,1,1,0,0,0,0,0,0,0,
              0,1,1,1,0,0,1,0,0,0,0,1,0,0,0,0,0,0,
              0,0,1,0,0,0,0,0,1,0,0,0,1,1,0,0,0,0,
              1,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,
              1,0,0,0,0,0,1,0,0,0,0,0,0,0,2,0,0,0,
              1,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,1,1),
            nrow = 18, 
            dimnames = list(
                            "terms" = indexWords,
                            "docs" = c("d1","d2","d3","d4","d5","d6","d7","d8")
                            ) )

normalize <- function(x) {x / sqrt(sum(x^2))}
A.normalize <- A

for(i in 1:dim(A)[2]) {
  A.normalize[,i] <- normalize(A[,i])
}


A.normalize.svd <- svd(A.normalize)

#rank 2
A.decomposed.u <- A.normalize.svd$u[,c(1,2)]
#new coordinnates of each doc
A.decomposed.v <- A.normalize.svd$v[,c(1,2)]
A.decomposed.d <- matrix(0, 2, 2)
A.decomposed.d[1,1] <- A.normalize.svd$d[1]
A.decomposed.d[2,2] <- A.normalize.svd$d[2]

#1/d and guarantee that inf is 0
A.decomposed.d.inverse <- 1/A.decomposed.d
A.decomposed.d.inverse[which(!is.finite(A.decomposed.d.inverse))] <- 0

docs.coord <- matrix(A.decomposed.v, nrow = 8, dimnames = list("docs" = c("d1", "d2", "d3","d4", "d5", "d6", "d7", "d8"),"coordinates" = c("x", "y")) )

plot(x = A.decomposed.v[,1], y = A.decomposed.v[,2], 
     col = c("#E7B800", "#39FF33", "#F445FF","#FC4E07","#C433FF", "#4F0C0C", "#3386FF","#469722"), 
     xlim = c(-0.75,0), ylim = c(-0.75,0.75), 
     xlab = "x", ylab = "y",
     pch = 1:8)
legend("topright", legend = c("doc1", "doc2", "doc3","doc4", "doc5","doc6", "doc7","doc8"),
       col =  c("#E7B800", "#39FF33", "#F445FF","#FC4E07","#C433FF", "#4F0C0C", "#3386FF","#469722"),
       pch = 1:8 )


#Query
#Microsoft windows
#windows garden
Q = matrix(c(1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0,0, 0, 0, 0, 0, 0, 0,
             1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0, 0,1, 0, 0, 0, 0), nrow=18,
           dimnames = list("terms" = indexWords,
                           "query" = c("q1","q2")) )



q1.aux <- t(Q[,1]) %*% (A.decomposed.u)
q1 <- q1.aux %*% dInverse

q2.aux <- t(Q[,2]) %*% (A.decomposed.u)
q2 <- q2.aux %*% dInverse

plotM <- matrix(0,nrow = 10, ncol = 2)

plotM[1,] <- A.decomposed.v[1,]
plotM[2,] <- A.decomposed.v[2,]
plotM[3,] <- A.decomposed.v[3,]
plotM[4,] <- A.decomposed.v[4,]
plotM[5,] <- A.decomposed.v[5,]
plotM[6,] <- A.decomposed.v[6,]
plotM[7,] <- A.decomposed.v[7,]
plotM[8,] <- A.decomposed.v[8,]
plotM[9,] <- q1
plotM[10,] <- q2

plot(x = plotM[,1], y = plotM[,2], 
     col = c("#E7B800","#E7B800","#E7B800","#E7B800","#E7B800","#E7B800","#E7B800","#E7B800","#00AFBB","#00AFBB"), 
     xlim = c(-0.75,0), ylim = c(-0.75,0.75), 
     xlab = "x", ylab = "y",
     pch = c(1,2,3,4,5,6,7,8,16,17))
legend("topleft", legend = c("doc1", "doc2", "doc3","doc4", "doc5","doc6", "doc7","doc8"),
       col =  c("#E7B800"),
       pch = 1:8 )

legend("topright", legend = c("Microsoft windows","windows garden" ),
       col =  c("#00AFBB"),
       pch = c(16, 17) )

combo.noDecompose = matrix(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0, 0, 0, 0, 0), nrow =2,
               dimnames = list("query" = c("q1","q2"),
                               "docs" = c("d1","d2","d3","d4","d5","d6","d7","d8")
                               ) )
combo.decompose = matrix(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0, 0, 0, 0, 0), nrow =2,
                           dimnames = list("query" = c("q1","q2"),
                                           "docs" = c("d1","d2","d3","d4","d5","d6","d7","d8")
                           ) )
for(i in 1:dim(A)[2]) {
  combo.noDecompose[1,i] <- t(Q[,1]) %*% A.normalize[,i]
  combo.noDecompose[2,i] <- t(Q[,2]) %*% A.normalize[,i]
  combo.decompose[1,i] <- q1 %*% A.decomposed.v[i,]
  combo.decompose[2,i] <- q2 %*% A.decomposed.v[i,]
}






################################################################
#     
#                     QUESTION 2
#
################################################################

avgSimilarity <- 
computeSimilarity <- function(termNotIncluded, begin=1, finish=8) {
  N <- dim(A)[2]
  aux <- 1/(N*(N-1))
  sum <- 0
  for(i in begin:finish) {
    for(k in begin:finish) {
      if(i != k){
        iDoc <- A.normalize[,i]
        kDoc <- A.normalize[,k]
        sum <- sum + t(iDoc[indexWords[which(indexWords != termNotIncluded)]])  %*% kDoc[indexWords[which(indexWords != termNotIncluded)]]
      }
        
    }
  }
  aux * sum
}

allTermsSimilarity <- computeSimilarity("")

dvValues <- integer(length(indexWords))
names(dvValues) <- indexWords

for(term in indexWords) {
  dvValues[term] <- allTermsSimilarity - computeSimilarity( termNotIncluded = term)
}
















