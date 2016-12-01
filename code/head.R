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


#Check if patient is walking straight enough
differ_ <- 0.4

start <- min(WALKING)
stop <- max(WALKING)


segments(start, WALKBASE, stop, col = "black")

#base <- mean(c(DATA[start,]$Head.y, DATA[stop,]$Head.y))
#size_ <- 5
# for(i in seq(start, stop, size_)){
#
#   step <- mean(DATA[i:(i+size_),]$Head.y) - base
#
#   for(j in 0:(size_ -1)){
#     if(DATA[i+j,]$state != 1) next
#     points(i+j, DATA[i+j,]$Head.y - step, p = "*")
#   }
#
# }

# http://stats.stackexchange.com/questions/30975/how-to-add-non-linear-trend-line-to-a-scatter-plot-in-r
# http://www.mathsisfun.com/geometry/parabola.html
