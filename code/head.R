print("START ANALYZING HEAD")
#color <- rainbow(length(STATE))
color <- c("red", "blue", "blue", "green")

#Plot graph
plot(DATA$Head.y,
     type = "l",
     ylab = "Hoogte hoofd"
)

for(i in 1:nrow(DATA)){
  points(i, DATA[i,]$Head.y, col = color[DATA[i,]$state])
}


#Check if patient is walking fast enough
differ_ <- 0.1

# Algorithm to calculate walk phase
f <- function(x) (sin((x - 5) / 2.7) / 30) + WALKBASE

# Draw Optimum walk
lines(WALKING, f(WALKING), col = "red", type = "l")

# Check if each point is close to optimum track
WALK <- data.frame(index=WALKING)
WALK$probability <- 0

for(i in WALKING){
   prob_ <- 1 - abs((abs(DATA[i,]$Head.y - f(i))) / differ_)
   #print(DATA[i,]$Head.y)
   #print(f(i))
   #print(prob_)
   #print("======")
   WALK[WALK$index==i,]$probability <- if(prob_ > 1) 1 else if(prob_ < 0) 0 else prob_
}

# points(WALK[WALK$probability == min(WALK$probability),]$index, 0.3)
# points(WALK[WALK$probability == max(WALK$probability),]$index, 0.3)

# Print out possible fall chance
print(paste("FALL CHANCE BASED ON STEPS: ", as.character(specify_decimal(100-(mean(WALK$probability)*100), 2)), "%", sep = ""))

print("DONE ANALYZING HEAD")
# http://stats.stackexchange.com/questions/30975/how-to-add-non-linear-trend-line-to-a-scatter-plot-in-r
# http://www.mathsisfun.com/geometry/parabola.html
