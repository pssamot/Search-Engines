#question 1
initial <- 1/4

a <- initial
b <- initial
c <- initial
d <- initial

o1 <- initial
o2 <- initial
o3 <- initial
o4 <- initial


a <- 1/2
b <- 1/8
c <- 1/4
d <- 1/8

o1 <- 3/8
o2 <- 3/8
o3 <- 1/8
o4 <- 1/8

a <- o1 + o2 + o3 + o4
b <- o2
c <- o1 + o2
d <- o2

o1 <- a + c + d
o2 <- a + b + c 
o3 <- a
o4 <- a

l1_a <- a + b + c +d
l1_1 <- o1 + o2+ o3+ o4

fractions(a)
a <- a / l1_a
fractions(a)

fractions(b)
b <- b / l1_a
fractions(b)

fractions(c)
c <- c / l1_a
fractions(c)

fractions(d)
d <- d / l1_a
fractions(d)


fractions(o1)
o1 <- o1 / l1_1
fractions(o1)

fractions(o2)
o2 <- o2 / l1_1
fractions(o2)

fractions(o3)
o3 <- o3 / l1_1
fractions(o3)


fractions(o4)
o4 <- o4 / l1_1
fractions(o4)

#question 2


A <- matrix(c(0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0, 
              0.4, 0.8, 0.1, 0.3, 0.9, 0.2, 0.4, 0.2, 0.1, 0.7, 0.6, 
              1, 1, 1, 1, 1, -1, -1, -1, -1, -1, -1),
            nrow = 11, 
            dimnames = list(
              "terms" = seq(1,11,1),
              "docs" = c("X1","X2","Y")
            ) )

A <- t(A)

A.svd <- svd(A)

#rank 2
A.decomposed.u <- A.svd$u[,c(1,2)]
#new coordinnates of each doc
A.decomposed.v <- A.svd$v[,c(1,2)]
A.decomposed.d <- matrix(0, 2, 2)
A.decomposed.d[1,1] <- A.svd$d[1]
A.decomposed.d[2,2] <- A.svd$d[2]


plot(x = A[,1], y = A[,2], 
     col = c("#26004c", "#26004c", "#26004c", "#26004c", "#26004c","#f08853","#f08853","#f08853","#f08853","#f08853","#f08853"), 
     pch = c(17,17,17,17,17,19,19,19,19,19,19))

legend("topright", legend = c("Y=1","Y=-1" ),
       col =  c("#26004c", "#f08853"),
       pch = c(17, 19) )



for(i in 1:dim(A)[1]) {
  if(i == 1 || i == 2 || i == 3 || i == 4 ||i == 5 || i ==7   ){
    text(A[i,1], A[i,2] + 0.02, paste("p", i,sep = ""))
  }

}

x  <- seq(0, 1, 0.1)
y1 <- 2*x - 0.8
lines(x,y1,col="green")
y2 <- 2*x - 0.3
lines(x,y2,col="green")
y3 <- 2*x - 0.55
lines(x,y3,col="blue")





