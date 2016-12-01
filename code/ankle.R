library(dplyr)


WD <- getwd()


CSV <- file.choose()

CSV <- read.csv(CSV)

# CONVERT
converted_ <- NULL

for(i in 1:(nrow(CSV) / 3)) {
  x <- NULL
  y <- NULL
  z <- NULL
  
  if(i > 1) { i <- i * 3 - 2 }
  
  x <- CSV[i,]
  y <- CSV[i+1,]
  z <- CSV[i+2,]
  #print(i)
  cnv_ <- merge(x, y, all = TRUE, suffixes = c(".x", ".y"), by = "Time")
  cnv_ <- merge(cnv_, z, all = TRUE, suffixes = c("", ".z"), by = "Time")
  
  if(i == 1) {
    #print("CREATE")
    converted_ <- cnv_
  }
  else {
    #print("BIND")
    converted_ <- bind_rows(converted_, cnv_)
  }
  
}

# For loop to add 1.1m to all the data of AnkleLeft.y to determine real height.
#for (j in 1:(nrow(converted_))) {
#  converted_$AnkleLeft.y + 1.1
#}

lowestLeft <- min(converted_$AnkleLeft.y)
lowestRight <- min(converted_$AnkleRight.y)

highestLeft <- max(converted_$AnkleLeft.y)
highestRight <- max(converted_$AnkleRight.y)

VerschilLeft <- (lowestLeft - highestLeft)
VerschilRight <- (lowestRight - highestRight)



plot(converted_$AnkleLeft.y,
     type = "l",
     ylab = "Hoogte...",
     col = ifelse(converted_$AnkleLeft.y > -1.1, "green", "red"),
     ylim = c(-1.20, -0.8)
)
par(new=TRUE)
plot(converted_$AnkleRight.y,
     type = "l",
     ylab = "Hoogte",
     col = ifelse(converted_$AnkleRight.y > -1.1, "orange", "purple"),
     ylim = c(-1.20, -0.8)
)
par(new=TRUE)
plot(converted_$Head.y,
     type = "l",
     ylab = "Hoogte in M",
     col = "blue",
     ylim = c(-1.20, 0.70)
)



