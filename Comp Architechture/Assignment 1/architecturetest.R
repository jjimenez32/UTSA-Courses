library(ggplot2)


p <- 256
s <- 0.01
Speedup <- 0
S <- 0
for(i in 1:25){
 
  
  # answer <- 1/((1-s)+ (s/p))
  answer <- (1/(s + (1-s)/p))
  
  
  
  Speedup <- append(Speedup, answer)
  S <- append(S, s)
  s <- s + 0.01
}




total <- data.frame(Speedup, S)
total <- total[-1,]
# qplot(x = total$S, y = total$Speedup, xlab = "S", ylab = "Speedup")

write.csv(total, file = "Question1.csv")

ggplot(total, aes(x = S)) +
  geom_line(aes(y = Speedup), colour = "blue")
