library(ggplot2)

ts = 250
tp = 750
P = 2
T1 = 1000
EfficiencyList = 0
TPList = 0
SpeedupList = 0
for(i in 1:4){
  if (i == 1){
    P = 2
  }else if( i == 2){
    P = 10
  }else if( i == 3){
    P = 50
  }else if( i == 4){
    P = 100
  }
  TP = ts + (tp/P)  
  Speedup = T1/TP
  Efficiency = 1/P * Speedup

  TPList = append( TPList , TP)
  SpeedupList = append(SpeedupList, Speedup)
  EfficiencyList = append(EfficiencyList, Efficiency)
}

Pvalues <- c(0,2,10,50,100)
frame <- data.frame(Pvalues, TPList, SpeedupList, EfficiencyList)
frame <- frame[-1,]

write.csv(frame, file = "question3.csv")


# qplot (x = frame$Pvalues, y = frame$TPList, xlab = "P", ylab = TP)

# ggplot(frame, aes(x = Pvalues))+
#   geom_line(aes(y =  EfficiencyList ),colour= "blue")+
#   geom_line(aes(y = SpeedupList,), colour = "red")+
#   xlab("")+
#   ylab("")+
#   geom_boxplot()
# 
# ggplot(frame, aes(x = Pvalues))+
#   geom_line(aes(y =  SpeedupList ),colour= "blue")+
#   xlab("P")+
#   ylab("Speedup")
# 
# ggplot(frame, aes(x = Pvalues)) +
#   geom_line(aes(y =  EfficiencyList ),colour= "blue") +
#   xlab("P") + 
#   ylab("Efficiency")
