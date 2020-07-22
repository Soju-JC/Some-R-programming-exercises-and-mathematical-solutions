#03 May 2020| Soju.JC
#Exercice about nesting(loop inside a loop)
#############################################################################
# A room contains 100 toggle switches, originally all turned off. 100 people
# enter the room in turn. The first toggles every switch, the second toggles
# every second switch, the third every third switch, and so on, to the last
# person who toggles the last switch only.
# At the end of this process, which switches are turned on?
#############################################################################

switches <- rep(0,100)
count <- 0
for(i in 1:100){
  for(k in 1:(100 - count)){
    switches[k + count] <- ifelse((i %% 2) == 0, 0, 1)
  }
  count <- count + 1
}
cat("These are the switches turned on:", "\n", which(switches == 1))



