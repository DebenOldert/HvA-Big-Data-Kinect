library(dplyr)

path = file(file.choose())
file = read.csv(path, stringsAsFactors = FALSE, header = T, sep = ",")

axis = data.frame(
  axis = character(),
  stringsAsFactors = FALSE
)

xAxis = "X"
yAxis = "Y"
zAxis = "Z"

x = 1
y = 2
z = 3

for (i in 1:(nrow(file))) {
  if (i / z == 1) {
    z = i + 3
    axis[i,1] = zAxis
  }else if(i / y == 1){
    y = i + 3
    axis[i,1] = yAxis
  }else if(i / x == 1){
    x = i + 3
    axis[i,1] = xAxis
  }
}

fileWithAxis = cbind(file, axis)
angleBody = select(fileWithAxis,Time, axis,SpineBase, Head)
rm(xAxis,yAxis,zAxis, x, y , z, file,axis, fileWithAxis)

valueBetweenTwoValue = data.frame(
  Time = integer(),
  Difference = double(),
  stringsAsFactors = FALSE
)

for (i in 1:nrow(angleBody)) {
  valueBetweenTwoValue[i,1] = angleBody[i,1]
  valueBetweenTwoValue[i,2] = angleBody[i,3] - angleBody[i,4]
}

valueBetweenTwoValue = abs(valueBetweenTwoValue)

calculateAngle = data.frame(
  Time = integer(),
  Angle = double(),
  stringsAsFactors = FALSE
)

doCalculateAngle = function(x){
  O = x[1]
  A = x[3]
  S = sqrt((O^2)+(A^2))
  newA = x[2]
  radian = atan(S/newA)
  degrees = radian*(180/pi)
  return(degrees)
}

initialTime = valueBetweenTwoValue[1,1]

count = 1;
timeDivision = 10^7

for(i in seq(from=1, to=nrow(valueBetweenTwoValue), by=3)){
  calculateAngle[count,1] = (valueBetweenTwoValue[i,1] - initialTime)/timeDivision
  vector = valueBetweenTwoValue$Difference[i:(i+2)]
  calculateAngle[count,2] = doCalculateAngle(vector)
  count = count + 1
}

plot(calculateAngle$Time,calculateAngle$Angle, xlab = "Second", ylab = "Degrees", main = "Angele of body", type = "l")

View(calculateAngle)

healthyPercentage = 20

if(mean(calculateAngle[, 2]) > healthyPercentage){
  print("High risk")
}else{
  print("Low risk")
}
