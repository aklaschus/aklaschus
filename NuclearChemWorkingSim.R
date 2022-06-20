#simulate nuclear decay with a starting pool of 500 dice
#roll all 500 at once. Remove all 3s or 9s. Re-roll remaining dice
#the pattern will imitate the rate of nuclear decay (illustrating half-life)

#structure of a repeat:
#repeat{
# if(condition){
# break
# }
#}


library(tidyr)
library(ggplot2)


dice = 500 #number of dice to start (total number of atoms to decay)
count = 0 #counter that keeps track of number of loops
size = 500 #size of beginning dice pool, SHOULD change with each loop

table <- c() #empty table to collect rows of observations

repeat{ #begin outer loop
  size = dice
  count = count + 1
  xDice = 0
  tDice = 0
  nDice = 0
  totalRemoved = 0
  
  repeat { #begin inner loop
    {
      size = size - 1
      x <- sample(c(1:20),1)
      {
        if(x == 3) {
          tDice <- tDice + 1
        } # if 1
        else if (x == 9) {
          nDice <- nDice + 1
        } #if 2
        else {
          xDice <- xDice + 1
        } #if 3
      }# all if statements
    } #total dice roll loop (inner)
    
      if(size == 0) {
        break
    } #break cue
  } #end of loop

  dice = dice - nDice - tDice
  totalRemoved = tDice + nDice
  table <- rbind(table, c(count, nDice, tDice, totalRemoved, dice))
  if (dice <= 5){
    break
  }
}

colnames(table) <- c("Round", "3Dice", "9Dice", "TotalRemoved", "DiceLeft")
print(table)

#now for plotting the decay rate
library(ggthemes)
table <- data.frame(rbind(table))

ggplot(table) + 
  geom_line(mapping = aes(x = Round, y = DiceLeft), color = "purple", size = 2) + 
  labs(title = "Rate of Decay",
       subtitle = "Simulated nuclear decay using dice",
       x = "Time",
       y = "Undecayed Nuclei") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text())


