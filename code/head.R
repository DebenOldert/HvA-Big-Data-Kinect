print("START ANALYZING HEAD")
color <- c("red", "blue", "blue", "green")

#Plot graph
plot(DATA$Head.y,
     type = "l",
     ylab = "Hoogte hoofd"
)

for(i in 1:nrow(DATA)){
  points(i, DATA[i,]$Head.y, col = color[DATA[i,]$state])
}
remove(i)

#Check if patient is walking fast enough
differ_ <- 0.5

# Algorithm to calculate walk phase
f <- function(x) (sin((x - 5) / 2.7) / 30) + patient$WALKBASE

# Draw Optimum walk
lines(patient$WALKING, f(patient$WALKING), col = "orange", type = "l")

# Check if each point is close to optimum track
WALK <- data.frame(index=patient$WALKING)
WALK$probability <- 0

for(i in patient$WALKING){
   prob_ <- 1 - abs((abs(DATA[i,]$Head.y - f(i))) / differ_)
   WALK[WALK$index==i,]$probability <- if(prob_ > 1) 1 else if(prob_ < 0) 0 else prob_
}
remove(i)
remove(prob_)
# Print out possible fall chance
print(paste("FALL CHANCE BASED ON STEPS: ", as.character(specify_decimal(100-(mean(WALK$probability)*100), 2)), "%", sep = ""))

print("DONE ANALYZING HEAD")
