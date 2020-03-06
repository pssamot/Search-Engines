sim_u2_u4 <- 0.2*0.9 + 0.3*0.4+0.4*0.5
sim_u3_u4 <- 0.3*0.9
sim_u5_u4 <- 0.7*0.9 + 0.5*0.5+0.6*0.4


user_user <- (0.3*sim_u2_u4 + 0.4*sim_u3_u4 + 0.7*sim_u5_u4)/(sim_u2_u4+sim_u3_u4+sim_u5_u4)

avgu2 <- (0.2+0.3+0.3+0.1+0.4)/5
avgu3 <- (0.4+0.3+0.2+0.4)/4
avgu4 <- (0.9+0.4+0.5)/3
avgu5 <- (0.4+0.7+0.6+0.7+0.5)/5

delta_u2_i7 <- 0.3 - avgu2
delta_u3_i7 <- 0.4 - avgu3
delta_u5_i7 <- 0.7 - avgu5

lenient <- avgu4 + (sim_u2_u4*delta_u2_i7 +sim_u3_u4*delta_u3_i7 + sim_u5_u4*delta_u5_i7 ) / (sim_u2_u4+sim_u3_u4+sim_u5_u4)


x1 <- c(2,1,4,3,5,4.5)
x2 <- c(1,3,2,4,5,6) 
plot(x = x1, y = x2, 
       col = c("#26004c", "#f08853", "#26004c", "#f08853", "#26004c","#f08853"), 
       pch = c(17,19,17,19,17,19))
 
legend("topleft", legend = c("click","no click" ),
      col =  c("#26004c", "#f08853"),
      pch = c(17, 19) )

for(i in 1:6) {
  if(i!=6){
    text(x1[i], x2[i] + 0.2, paste("doc", i,sep = ""))
  }else{
    text(x1[i], x2[i]-  0.2, paste("doc", i,sep = ""))
  }
}
    
  
}