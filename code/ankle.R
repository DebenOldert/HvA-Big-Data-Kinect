# For loop to add 1.1m to all the data of AnkleLeft.y to determine real height.
#for (j in 1:(nrow(DATA))) {
#  DATA$AnkleLeft.y + 1.1
#}

plot(DATA[patient$WALKING,]$AnkleLeft.y,
     type = "l",
     ylab = "Hoogte",
     col = ifelse(DATA$AnkleLeft.y > -1.1, "green", "red"),
     ylim = c(-1.15, -0.9)
)
par(new=TRUE)
plot(DATA[patient$WALKING,]$AnkleRight.y,
     type = "l",
     ylab = "Hoogte",
     col = ifelse(DATA$AnkleRight.y > -1.1, "orange", "purple"),
     ylim = c(-1.15, -0.9)
)

#Calculates the mean height of AnkleLeft.y
meanLine <- mean(DATA[patient$WALKING,]$AnkleLeft.y)

##-LEFT-##
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
Q3 <- c()
for(i in patient$WALKING){
  if(DATA[i,]$AnkleLeft.y < meanLine){
    Q3 <- c(Q3, DATA[i,]$AnkleLeft.y)
  }
}

#Gives the mean value of the third quarter
meanQ3 <- mean(Q3)

DiffAnkleLeft <- abs(meanQ3 - meanQ1)


##-RIGHT-##
#Creates vector which contains the values above the mean height (1e quarter)
Q1 <- C()
for(i in patient$WALKING){
  if(DATA[i,]$AnkleRight.y > meanLine){
    Q1 <- c(Q1, DATA[i,]$AnkleRight.y)
  }
}

#Gives the mean value of the first quarter
meanQ1 <- mean(Q1)

#Creates vector which contains the values above the mean height (3e quarter)
Q3 <- c()
for(i in patient$WALKING){
  if(DATA[i,]$AnkleRight.y < meanLine){
    Q3 <- c(Q3, DATA[i,]$AnkleRight.y)
  }
}

#Gives the mean value of the third quarter, And the difference
meanQ3 <- mean(Q3)

DiffAnkleRight <- abs(meanQ3 - meanQ1)

DiffAnkleLeft
DiffAnkleRight

if(DiffAnkleLeft < 0.05 || DiffAnkleRight < 0.05){
  print("The patient barely lift his foot up, there is a potentional falling risk")
}
else {
  print("The patient walks just fine according his ankles")
  }

#Creates Smoothline of the left ankle
SmoothAnkleLeft <- smooth.spline(DATA[patient$WALKING,]$AnkleLeft.y, spar=0.35)
lines(SmoothAnkleLeft, col = "green")

SmoothAnkleRight <- smooth.spline(DATA[patient$WALKING,]$AnkleRight.y, spar=0.35)
lines(SmoothAnkleRight, col = "purple")
