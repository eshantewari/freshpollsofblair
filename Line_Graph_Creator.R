setwd("C:\\Users\\emmaj\\Documents\\649\\State_Change") 

#The names of all of the files
sheet_names <- c("New_Hampshire","Pennsylvania","Ohio","Michigan","Wisconsin","Minnesota","Iowa","Colorado","Nevada","Arizona","Virginia","North_Carolina","Florida")

#sheet_names <- c("Florida")

#Percents
for(name in sheet_names) {
  png(paste("_",name,"Odds.png",sep=""), width=700, height=540, res=120)
  #Get the input
  state_data <- read.csv(paste(name,"_Percent.csv",sep=""))
  
  #retrive the row names
  clinton_percents <- as.vector(t(state_data)[-1,1])
  trump_percents <- as.vector(t(state_data)[-1,2])
  johnson_percents <- as.vector(t(state_data)[-1,3])
  
  plot(clinton_percents, type="l", col="blue", ylim=c(0.0, 1.0), xlim=c(1,6),xaxt="n", yaxt="n", 
       xlab="Date", ylab="Probability", main="How the odds have changed")
  axis(1,at=c(1,2,3,4,5,6),labels=c("Oct 10", "Oct 13", "Oct 17","Oct 19","Oct 21","    "))
  axis(2,at=c(0,.25,.5,.75,1),labels=c("0%","25%","50%","75%","100%"),las=2)
  lines(trump_percents, type="l", col="red")
  lines(johnson_percents, type="l", col="purple")
  
  clintonProb = as.double(tail(clinton_percents, n=1))
  trumpProb = as.double(tail(trump_percents, n=1))
  clintonGraph <- clintonProb
  trumpGraph <- trumpProb
  if(clintonProb - trumpProb < .3){
    if(clintonProb > trumpProb){
      clintonGraph <- .75
      trumpGraph <- .25
    }else{
      clintonGraph <- .29
      trumpGraph <- .64
    }
  }

  text(c(5.5,5.5), c(clintonGraph-.07, trumpGraph+.07),
       c(paste("Clinton\n ",clintonProb*100,"%",sep=""), paste("Trump\n ",trumpProb*100,"%",sep="")),
       col=c(rgb(0,0,.6), rgb(.6,0,0),rgb(.4,.02,.6)))

  dev.off()
}

#Means
for(name in sheet_names) {
  png(paste("_",name,"Means.png",sep=""), width=700, height=540, res=120)
  #Get the input
  #print(name)
  state_means <- read.csv(paste(name,"_Mean.csv",sep=""))
  #retrive the row names
  clinton_means <- as.vector(t(state_means)[-1,1])
  trump_means <- as.vector(t(state_means)[-1,2])
  johnson_means <- as.vector(t(state_means)[-1,3])

  plot(clinton_means, type="l", col="blue", ylim=c(0.0, 1.0), xlim=c(1,6), xaxt="n", yaxt="n", 
       xlab="Date", ylab="Mean", main="How the vote shares have changed")
  axis(1,at=c(1,2,3,4,5,6),labels=c("Oct 10", "Oct 13", "Oct 17","Oct 19", "Oct 21", "   "))
  axis(2,at=c(0,.25,.5,.75,1),labels=c("0%","25%","50%","75%","100%"),las=2)
  lines(trump_means, type="l", col="red")
  lines(johnson_means, type="l", col="purple")

  clintonMean = round(as.double(tail(clinton_means, n=1)), digits=4)
  trumpMean = round(as.double(tail(trump_means, n=1)), digits=4)
  clintonGraph <- clintonMean
  trumpGraph <- trumpMean
  if(clintonMean - trumpMean < .3){
    if(clintonMean > trumpMean){
      clintonGraph <- .75
      trumpGraph <- .25
    }else{
      clintonGraph <- .29
      trumpGraph <- .64
    }
  }
  
  text(c(5.5,5.5), c(clintonGraph-.07, trumpGraph+.07),
       c(paste("Clinton\n ",clintonMean*100,"%",sep=""), paste("Trump\n ",trumpMean*100,"%",sep="")),
       col=c(rgb(0,0,.6), rgb(.6,0,0),rgb(.4,.02,.6)))

  dev.off()
}