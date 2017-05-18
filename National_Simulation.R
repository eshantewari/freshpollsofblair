rm(list=ls())
setwd("~/Desktop/PoliStat") 
writeFiles <- FALSE

#Aggregates all the polls of the states
#Inputs: File name corresponding to the state file
#Outputs: Vector with the following components - Number of Electoral Votes, Clinton Mean, Trump Mean, Johnson Mean, Candidate Variance, Undecided Size, Undecided Mean, Undecided Variance
state_poll_aggregation <- function(file){
  
  setwd("C:\\Users\\emmaj\\Documents\\649")
  #setwd("~/Desktop/PoliStat") 
  sample_data <- read.csv(file)
  #names(sample_data) <- c("State", "Evotes", "Time", "Sample_Size", "Clinton", "Trump", "Johnson")
  
  
  #Input from the File
  votes <- c(sample_data$"Evotes")
  dates <- c(sample_data$"Time")
  sample_sizes <- c(sample_data$"Sample_Size")
  clinton_results <- c(sample_data$"Clinton")
  trump_results <- c(sample_data$"Trump")
  johnson_results <- c(sample_data$"Johnson")
  
  #Calculate the Variances (All Candidate Distributions will have the same variance?)
  variances <- 1/(4*sample_sizes)
  #print(votes)
  
  
  #Time Weight Constant
  c <- log(100)/47
  
  #Calculate and Normalize the Weights
  
  weights <- exp(-c*dates + 1/log(sample_sizes))*1/(1+exp(((1-clinton_results-trump_results-johnson_results)-0.175)/0.035))
  weights <- weights/sum(weights)
  
  #Construct the "covariance" matrix
  cov_matrix <- matrix(nrow = length(dates), ncol = length(dates))
  
  for(i in 1:dim(cov_matrix)[1]){
    cov_matrix[i,] <- sqrt(variances[i])*sqrt(variances)
  }
  
  #Construct the "Lambian interval"
  min_variance <- sum(weights^2*variances)
  max_variance <- (t(weights) %*% cov_matrix %*% weights)[1,1]
  #print(min_variance)
  
  #Compute the middle of the Lambian interval (which we are currently not using)
  average_variance <- (min_variance+max_variance)/2
  
  #Determine candidate means through weighted averages
  clinton_mean <- sum(weights*clinton_results)
  trump_mean <- sum(weights*trump_results)
  johnson_mean <- sum(weights*johnson_results)
  
  #print(clinton_mean-trump_mean)
  
  #Determine the undecided size and variance
  undecided_size <- 1-clinton_mean-trump_mean-johnson_mean
  undecided_variance <- 3*max_variance
  if(undecided_size < 0){
    undecided_size = 0
    undecided_variance = 0
  }
  
  #Determine the mean of the undecideds (the proportion of undecideds who will flock towards Clinton) by regressing on the latest 10 polls
  r = -cor(tail(dates, 5),tail(clinton_results-trump_results, 5))*1
  undecided_mean <- .5*(1+r^11)
  #print(r)
  
  #Output Vector
  outputs <- c(votes[1], clinton_mean, trump_mean, johnson_mean, max_variance, undecided_size, undecided_mean, undecided_variance)
  return(outputs)
  
}

#The names of all of the files
sheet_names <- c("New_Hampshire.csv","Pennsylvania.csv","Ohio.csv","Michigan.csv","Wisconsin.csv","Minnesota.csv","Iowa.csv","Colorado.csv","Nevada.csv","Arizona.csv","Virginia.csv","North_Carolina.csv","Florida.csv")

#Set up vectors (names are fairly descriptive)
electoral_college_votes <- c()
clinton_means <- c()
trump_means <- c()
johnson_means <- c()
variances <- c()
undecided_sizes <- c()
undecided_means <- c()
undecided_variances <- c()


#Iterate through all the state files, call the funciton to aggregate the state polls, and append to the above vectors
for(name in sheet_names){
  state_info <- state_poll_aggregation(name)
  
  electoral_college_votes <- c(electoral_college_votes,state_info[1])
  clinton_means <- c(clinton_means,state_info[2])
  trump_means <- c(trump_means,state_info[3])
  johnson_means <- c(johnson_means,state_info[4])
  variances <- c(variances,state_info[5])
  undecided_sizes <- c(undecided_sizes,state_info[6])
  undecided_means <- c(undecided_means,state_info[7])
  undecided_variances <- variances
}

variances <- 2*variances
#Parameter for the National Simulation: Number of Trials
num_trials <- 10000

#Set-up variables for the national simulation
clinton_victories <- 0
trump_victories <- 0
johnson_victories <- 0

#Keep Track of How Many Electoral Votes Each Candidate Wins
clinton_electoral_votes <- c()
trump_electoral_votes <- c()
johnson_electoral_votes <- c()

#Keep Track of How Many Times Each Candidate Wins a State
clinton_state_victories <- rep(0,13)
trump_state_victories <- rep(0,13)
johnson_state_victories <- rep(0,13)

uncorr_clinton_state_victories <- rep(0,13)
uncorr_trump_state_victories <- rep(0,13)
uncorr_johnson_state_victories <- rep(0,13)


clinton_state_results <- matrix(nrow = num_trials, ncol = 13)
trump_state_results <- matrix(nrow = num_trials, ncol = 13)
johnson_state_results <- matrix(nrow = num_trials, ncol = 13)


#Account for State Correlations

#Read in matrix of r values
rs <- read.csv("Correlation_Matrix.csv", header = FALSE)
r_matrix <- data.matrix(rs, rownames.force = NA)

#----------------------------------National Simulation---------------------------------
for(a in 1:num_trials){
  
  
  clinton_votes <- 191 #The number of Clinton votes that we are essentially certain of
  trump_votes <- 180 #The number of Trump votes that we are essentially certain of
  johnson_votes <- 0 #The number of Johnson votes that we are essentially certain of
  
  clinton_results <- rep(0,13)
  trump_results <- rep(0,13)
  johnson_results <- rep(0,13)
  
  #--------Iterate through all of the states, simulating a result for each candidate in each state
  for(i in 1:length(electoral_college_votes)){
    #Simulate the proportion of undecided voters who will flock towards clinton
    undecided_vote <- rnorm(n=1,mean=undecided_means[i],sd=sqrt(undecided_variances[i]))
    
    #----First go through each state and draw from clinton, trump, johnson, undecided distributions
    clinton_results[i] <- rnorm(n=1,mean=clinton_means[i],sd=sqrt(variances[i]))+undecided_vote*undecided_sizes[i]
    trump_results[i] <- rnorm(n=1,mean=trump_means[i],sd=sqrt(variances[i]))+(1-undecided_vote)*undecided_sizes[i]
    johnson_results[i] <- rnorm(n=1,mean=johnson_means[i],sd=sqrt(variances[i]))
    
    clinton_state_results[a,i] <- clinton_results[i]
    trump_state_results[a,i] <- trump_results[i]
    johnson_state_results[a,i] <- johnson_results[i]
  }
  
  for(i in 1:length(electoral_college_votes)){
    # print(clinton_results[i])
    
    #For uncorrelated State Probabilities
    if((clinton_results[i]>trump_results[i]) && (clinton_results[i]>johnson_results[i])){
      uncorr_clinton_state_victories[i] <- uncorr_clinton_state_victories[i]+1
    }
    
    if((trump_results[i]>clinton_results[i]) && (trump_results[i]>johnson_results[i])){
      uncorr_trump_state_victories[i] <- uncorr_trump_state_victories[i]+1
    }
    
    if((johnson_results[i]>clinton_results[i]) && (johnson_results[i]>clinton_results[i])){
      uncorr_johnson_state_victories[i] <- uncorr_johnson_state_victories[i]+1
    }
    
  }
  
  #--------Then, Cholesky decompose the fucker and adjust
  clinton_results <- (10*clinton_results+t(chol(r_matrix))%*%clinton_results)/11
  trump_results <- (10*trump_results+ t(chol(r_matrix))%*%trump_results)/11
  johnson_results <- (10*johnson_results+t(chol(r_matrix))%*%johnson_results)/11
  
  #--------Then, go back through each state and select the state winner
  for(i in 1:length(electoral_college_votes)){
    # print(clinton_results[i])
    
    #--Select state winner and add electoral votes to their running total
    if((clinton_results[i]>trump_results[i]) && (clinton_results[i]>johnson_results[i])){
      clinton_votes <- clinton_votes+electoral_college_votes[i]
      clinton_state_victories[i] <- clinton_state_victories[i]+1
    }
    
    if((trump_results[i]>clinton_results[i]) && (trump_results[i]>johnson_results[i])){
      trump_votes <- trump_votes+electoral_college_votes[i]
      trump_state_victories[i] <- trump_state_victories[i]+1
    }
    
    if((johnson_results[i]>clinton_results[i]) && (johnson_results[i]>clinton_results[i])){
      johnson_votes <- johnson_votes+electoral_college_votes[i]
      johnson_state_victories[i] <- johnson_state_victories[i]+1
    }
    
  }
  
  #-Determine the victor of the trial and increment the win count of the winning candidate
  if((clinton_votes>trump_votes) && (clinton_votes>johnson_votes)){
    clinton_victories <- clinton_victories+1
  }
  
  if((trump_votes>clinton_votes) && (trump_votes>johnson_votes)){
    trump_victories <- trump_victories+1
  }
  
  if((johnson_votes>clinton_votes) && (johnson_votes>clinton_votes)){
    johnson_victories <- johnson_victories+1
  }
  
  clinton_electoral_votes <- c(clinton_electoral_votes, clinton_votes)
  trump_electoral_votes <- c(trump_electoral_votes, trump_votes)
  johnson_electoral_votes <- c(johnson_electoral_votes, johnson_votes)
}


#-----------Calculate the national win probabilities after all trials have occurred---------
clinton_probability <- clinton_victories/num_trials
trump_probability <- trump_victories/num_trials
johnson_probability <- johnson_victories/num_trials


#print(sum(clinton_electoral_votes)/num_trials)
#print(sum(trump_electoral_votes)/num_trials)

#-----------Calculate the state win probabilities after all trials have occurred
uncorr_clinton_state_probabilities <- uncorr_clinton_state_victories/num_trials
uncorr_trump_state_probabilities <- uncorr_trump_state_victories/num_trials
uncorr_johnson_state_probabilities <- uncorr_johnson_state_victories/num_trials

clinton_state_probabilities <- clinton_state_victories/num_trials
trump_state_probabilities <- trump_state_victories/num_trials
johnson_state_probabilities <- johnson_state_victories/num_trials

clinton_state_means <- rep(0,13)
trump_state_means <- rep(0,13)
johnson_state_means <- rep(0,13)


print(uncorr_clinton_state_probabilities)
print(clinton_state_probabilities)

for(i in 1:13){
  clinton_state_means[i] =mean(clinton_state_results[,i])
  trump_state_means[i] =mean(trump_state_results[,i])
  johnson_state_means[i] =mean(johnson_state_results[,i])
}

states <- c("New_Hampshire","Pennsylvania","Ohio","Michigan","Wisconsin","Minnesota","Iowa","Colorado","Nevada","Arizona","Virginia","North_Carolina","Florida")
#states <- c("New_Hampshire")

#--------------Write to the state's history csv file
if(writeFiles){
  i <- 0
  for(name in states){
    i <- i + 1
    #----Percent---
    state_percent <- read.csv(paste("State_Change\\",name,"_Percent.csv",sep=""))
    print(state_percent)
    new_percents <- c(clinton_state_probabilities[i], trump_state_probabilities[i], johnson_state_probabilities[i])
    state_percent[,"10/23"] <- new_percents
    write.csv(state_percent, file=paste("State_Change\\",name,"_Percent.csv",sep=""), 
              quote = FALSE, row.names=FALSE)
    
    #-----Mean-----
    state_means <- read.csv(paste("State_Change\\",name,"_Mean.csv",sep=""))
    new_means <- c(clinton_state_means[i], trump_state_means[i], johnson_state_means[i])
    state_means[,"10/19"] <- new_means
    write.csv(state_means, paste("State_Change\\",name,"_Mean.csv",sep=""), 
              quote = FALSE, row.names=FALSE)
  }
}

#------------------------------------Output-------------------------------------
print("Probabilities")
print(paste("Clinton", clinton_probability))
print(paste("Trump", trump_probability))
print(paste("Johnson", johnson_probability))

print("--------State probabilities--------")
state_probs = data.frame(states, clinton_state_probabilities, trump_state_probabilities, johnson_state_probabilities)
names(state_probs) <- c("states", "Clinton", "Trump", "Johnson")
print(state_probs)
write.csv(state_probs, file="stateProbs.csv")

print(" ")
print("----------State means-------------")
state_means = data.frame(states, clinton_state_means, trump_state_means, johnson_state_means)
names(state_means) <- c("states", "Clinton", "Trump", "Johnson")
print(state_means)
write.csv(state_means, file="stateMeans.csv")

#print(clinton_electoral_votes)
#print(trump_electoral_votes)

print(" ")
print("-------Mean electoral votes----------")
print(paste("Clinton", mean(clinton_electoral_votes)))
print(paste("Trump", mean(trump_electoral_votes)))
print(paste("Johnson", mean(johnson_electoral_votes)))

#print(sqrt(variances))

png("ClintonHist.png", width=660, height=700, res=120)
hist(clinton_electoral_votes, xlim = c(150, 400), col=rgb(0,0,1,1/4), main ="Clinton Electoral Votes", xlab="Number of electoral votes", breaks=42)
abline(v=270,col="red")
dev.off()

png("TrumpHist.png", width=660, height=700, res=120)
hist(trump_electoral_votes, xlim = c(150, 400), col=rgb(1,0,0,1/4), main="Trump Electoral Votes", xlab="Number of electoral votes", breaks=42)
abline(v=270,col="red")
dev.off()