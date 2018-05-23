year <- 18
for(i in 1:5){
  fileName <- paste("Lebron20", as.character(year), ".txt",sep='')
  Lebron2018 <- read.csv(fileName)
  #Lebron2018Home <- subset(Lebron2018, subset=X != '@', select='PT')
  Lebron2018Home <- Lebron2018$PTS[which(Lebron2018$X != "@")]
  Lebron2018Away <- Lebron2018$PTS[which(Lebron2018$X == "@")]

  totalGames <- length(Lebron2018$PTS)
  homeGames <- length(Lebron2018Home)
  diff <- mean(Lebron2018Home) - mean(Lebron2018Away)
  diffs <- c()
  for(i in 1:10000){
    indeces <- sample(1:totalGames, homeGames)
    home <- Lebron2018$PTS[indeces]
    away <- Lebron2018$PTS[-indeces]
    diffs[i] <- mean(home) - mean(away)
  }
  hist(diffs)
  
  PValAbove <- length(which(diffs > diff)) / length(diffs)
  PValBelow <- length(which(diffs < diff)) / length(diffs)
  print(min(PValAbove, PValBelow))
  year <- year - 1
}