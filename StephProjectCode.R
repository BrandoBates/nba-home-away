yearint<-14

for(i in 1:5){
  yearstr<-as.character(yearint)
  fileName <- paste("C:/Users/Brandon/Desktop/Statistics/Project/Steph20", yearstr, ".csv",sep='')
  Steph2014<-read.csv(fileName)
  Steph2014PTS<-na.omit(Steph2014$PTS)
  
  Steph2014Home <- Steph2014$PTS[which(Steph2014$Where != "@")]
  Steph2014Home<-na.omit(Steph2014Home)
  
  Steph2014Away <- Steph2014$PTS[which(Steph2014$Where == "@")]
  Steph2014Away<-na.omit(Steph2014Away)
  
  TotalGames<-length(Steph2014PTS)
  HomeGames<-length(Steph2014Home)
  AWayGames<-length(Steph2014Away)
  
  MeanDiff<-mean(Steph2014Home) - mean(Steph2014Away)
  diffs <- c()
  for(i in 1:10000){
    index <- sample(1:TotalGames, HomeGames)
    home <- Steph2014PTS[index]
    away <- Steph2014PTS[-index]
    diffs[i] <- mean(home) - mean(away)
  }
  hist(diffs)
  abline(v = MeanDiff, col = "blue", lty=5)
  
  PValAbove <- length(which(diffs > MeanDiff)) / length(diffs)
  PValBelow <- length(which(diffs < MeanDiff)) / length(diffs)
  print(PValAbove)
  
  z <- MeanDiff / sqrt((sd(Steph2014Home)^2 / HomeGames) + (sd(Steph2014Away)^2 / AWayGames))
  theoryP <- 1-pnorm(z)
  
  yearint<-yearint + 1
  
  print(paste0("Mean difference: ", MeanDiff))
  print(paste0("Home mean: ", mean(Steph2014Home)))
  print(paste0("Home sd: ", sd(Steph2014Home)))
  print(paste0("Home n: ", length(Steph2014Home)))
  print(paste0("Away mean: ", mean(Steph2014Away)))
  print(paste0("Away sd: ", sd(Steph2014Away)))
  print(paste0("Away n: ", length(Steph2014Away)))
  print(quantile(diffs, c(0.025, 0.975)))
  
  print(paste0("Theoretical p: ", theoryP))
  qqnorm(diffs)
  
}