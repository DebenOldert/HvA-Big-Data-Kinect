library(dplyr)

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
     ylim = c(-1.15, -0.80)
)
#par(new=TRUE)
#plot(DATA$AnkleRight.y,
#     type = "l",
#     ylab = "Hoogte",
#     col = ifelse(DATA$AnkleRight.y > -1.1, "orange", "purple"),
#     ylim = c(-1.20, -0.8)
#)
