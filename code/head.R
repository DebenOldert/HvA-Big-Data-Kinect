#color <- rainbow(length(STATE))
color <- c("red", "blue", "blue", "green")

#Plot graph
plot(DATA$Head.y,
     type = "l",
     ylab = "Hoogte hoofd"
)

for(i in 1:nrow(DATA)){
  print(i)
  points(i, DATA[i,]$Head.y, col = color[DATA[i,]$state])
}


#Check if patient is walking straight enough
differ_ <- 0.4

WALKING <- strtoi(rownames(DATA[DATA$state==1,]))

start <- min(WALKING)
stop <- max(WALKING)


segments(start, mean(c(DATA[start,]$Head.y, DATA[stop,]$Head.y)), stop, col = "black")

base <- mean(c(DATA[start,]$Head.y, DATA[stop,]$Head.y))
size_ <- 7
for(i in seq(start, stop, size_)){

  step <- mean(DATA[i:(i+size_),]$Head.y) - base

  for(j in 0:(size_ -1)){
    if(DATA[i+j,]$state != 1) next
    points(i+j, DATA[i+j,]$Head.y - step, p = "*")
  }

}
