# For loop to add 1.1m to all the data of AnkleLeft.y to determine real height.
#for (j in 1:(nrow(DATA))) {
#  DATA$AnkleLeft.y + 1.1
#}

lowestLeft <- min(DATA$AnkleLeft.y)
lowestRight <- min(DATA$AnkleRight.y)

highestLeft <- max(DATA$AnkleLeft.y)
highestRight <- max(DATA$AnkleRight.y)

VerschilLeft <- (lowestLeft - highestLeft)
VerschilRight <- (lowestRight - highestRight)

plot(DATA$AnkleLeft.y,
     type = "l",
     ylab = "Hoogte...",
     col = ifelse(DATA$AnkleLeft.y > -1.1, "green", "red"),
     ylim = c(-1.15, -0.9)
)
par(new=TRUE)
plot(DATA$AnkleRight.y,
     type = "p",
     ylab = "Hoogte",
     col = ifelse(DATA$AnkleRight.y > -1.1, "orange", "purple"),
     ylim = c(-1.15, 0)
)

#Calculates the mean height of AnkleLeft.y
meanLine <- mean(DATA[patient$WALKING,]$AnkleLeft.y)

#Creates vector which contains the values above the mean height (1e quarter)
Q1 <- c()
for(i in patient$WALKING){
  if (DATA[i,]$AnkleLeft.y > meanLine){
    Q1 <- c(Q1, DATA[i,]$AnkleLeft.y)
  }
}

#Gives the mean value of the first quarter
meanQ1 <- mean(Q1)

#Creates vector which containts the values under the mean height value (3e quarter)
Q2 <- c()
for(i in patient$WALKING){
  if(DATA[i,]$AnkleLeft.y < meanLine){
    Q2 <- c(Q2, DATA[i,]$AnkleLeft.y)
  }
}

#Gives the mean value of the third quarter
meanQ2 <- mean(Q2)

meanQ1
meanQ2
DifferenceAnkleLeft <- meanQ2 - meanQ1
remove(i)


#Creates Smoothline of the left ankle
SmoothAnkleLeft <- smooth.spline(DATA[patient$WALKING,]$AnkleLeft.y, spar=0.35)
lines(SmoothAnkleLeft)
