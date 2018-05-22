library(ggplot2)


Speedup1 <- 0
Speedup2 <- 0
Speedup3 <- 0 
sanswer1 <- 0
sanswer2 <- 0
sanswer3 <- 0
pvalue <- 0

p <- 2
s1 <- 0.01
s2 <- 0.1
s3 <- 0.25
for(i in 1:63){
  # answer1 <- 1/((1-s1)+ (s1/p))
  # answer2 <- 1/((1-s2) + (s2/p))
  # answer3 <- 1/((1-s3) + (s3/p))
  answer1 <- 1/(s1+ ((1-s1)/p))
  answer2 <- 1/(s2+ ((1-s2)/p))
  answer3 <- 1/(s3+ ((1-s3)/p))
  p = p + 1

  
  Speedup1 <- append(Speedup1, answer1)
  Speedup2 <- append(Speedup2 , answer2)
  Speedup3 <- append(Speedup3, answer3)
  sanswer1 <- append(sanswer1, s1)
  sanswer2 <- append(sanswer2, s2)
  sanswer3 <- append(sanswer3, s3)
  pvalue <- append(pvalue, p)
  
  # s <- s + 0.01
}

total <- data.frame(pvalue, Speedup1,Speedup2,Speedup3)
total <- total[-1,]

write.csv(total, file = "question2v1.csv")

ggplot(total, aes( pvalue))+
  geom_line(aes(y = Speedup1 ),colour= "red")+
  geom_line(aes(y = Speedup2 ),colour = "blue") +
  geom_line(aes(y = Speedup3 ),colour = "green")