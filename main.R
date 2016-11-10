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

plot(converted_$Head.y,
     type = "l",
     ylab = "Hoogte hoofd"
     )







