#Ethan Cai OKC Internship OA
#10/3/2022
library(readr)
#string passed in read_csv is path of shots_data.csv on computer
shots_dataset <- read_csv("/Users/Caiqile/Downloads/shots_data.csv")

team_index <- 1
x_index <- 2
y_index <- 3
made_index <- 4
x_positive_corner_3 <- 22
x_negative_corner_3 <- -22
y_corner_3 <- 7.8
NC3_dist <- 23.75


#create a matrix teamA and teamB, where each row is 2PT, NC3, C3 shots
#each column is # of made shots, # of attempted shots
#in total, 3x2 matrix
teamA <- matrix(c(0, 0, 0, 0, 0, 0), nrow = 3, ncol = 2)
teamB <- matrix(c(0, 0, 0, 0, 0, 0), nrow = 3, ncol = 2)
rownames(teamA) <- c("2PT", "NC3", "C3")
colnames(teamA) <- c("shots made", "shots attempted")
rownames(teamB) <- c("2PT", "NC3", "C3")
colnames(teamB) <- c("shots made", "shots attempted")

#iterate through every shot in the dataset
for (shot in 1:nrow(shots_dataset[1])) {
  x_val <- shots_dataset[shot, x_index]
  y_val <- shots_dataset[shot, y_index]
  made_val <- shots_dataset[shot, made_index]
  #corner 3 shot
  if ((x_val > x_positive_corner_3 | x_val < x_negative_corner_3) & (y_val <= y_corner_3)) {
    shot_classification <- "C3"
  } 
  #Non Corner 3 shot
  else if ((sqrt(x_val^2 + y_val^2) > NC3_dist) & (y_val > y_corner_3)) {
    shot_classification <- "NC3"
  } 
  #2PT shot
  else {
    shot_classification <- "2PT"
  }
  if (shots_dataset[shot, team_index] == "Team A") {
    #if shot is made, then add 1 to number of makes
    if (made_val == 1) {
      teamA[shot_classification, "shots made"] <- teamA[shot_classification, "shots made"] + 1
    }
    #always adds 1 to number of attempts, whether shot is made or not
    teamA[shot_classification, "shots attempted"] <- teamA[shot_classification, "shots attempted"] + 1
  } 
  else if (shots_dataset[shot, team_index] == "Team B") {
    #if shot is made, then add 1 to number of makes
    if (made_val == 1) {
      teamB[shot_classification, "shots made"] <- teamB[shot_classification, "shots made"] + 1
    }
    #always adds 1 to number of attempts, whether shot is made or not
    teamB[shot_classification, "shots attempted"] <- teamB[shot_classification, "shots attempted"] + 1
  }
}

print(teamA)
print(teamB)

#calculate shot distributions of each shot zone for both teams
paste("Team A 2PT Shot Distribution = ", teamA["2PT", "shots attempted"] / sum(teamA[, "shots attempted"]))
paste("Team A NC3 Shot Distribution = ", teamA["NC3", "shots attempted"] / sum(teamA[, "shots attempted"]))
paste("Team A C3 Shot Distribution = ", teamA["C3", "shots attempted"] / sum(teamA[, "shots attempted"]))
paste("Team B 2PT Shot Distribution = ", teamB["2PT", "shots attempted"] / sum(teamB[, "shots attempted"]))
paste("Team B NC3 Shot Distribution = ", teamB["NC3", "shots attempted"] / sum(teamB[, "shots attempted"]))
paste("Team B C3 Shot Distribution = ", teamB["C3", "shots attempted"] / sum(teamB[, "shots attempted"]))

#calculate EFG for each shot zone for both teams
teamA_2PT_EFG <- teamA["2PT", "shots made"] / teamA["2PT", "shots attempted"]
teamA_NC3_EFG <- (teamA["NC3", "shots made"] + 0.5 * teamA["NC3", "shots made"]) / teamA["NC3", "shots attempted"]
teamA_C3_EFG <- (teamA["C3", "shots made"] + 0.5 * teamA["C3", "shots made"]) / teamA["C3", "shots attempted"]
teamB_2PT_EFG <- teamB["2PT", "shots made"] / teamB["2PT", "shots attempted"]
teamB_NC3_EFG <- (teamB["NC3", "shots made"] + 0.5 * teamB["NC3", "shots made"]) / teamB["NC3", "shots attempted"]
teamB_C3_EFG <- (teamB["C3", "shots made"] + 0.5 * teamB["C3", "shots made"]) / teamB["C3", "shots attempted"]

#print EFGs
paste("Team A 2PT EFG% = ", teamA_2PT_EFG)
paste("Team A NC3 EFG% = ", teamA_NC3_EFG)
paste("Team A C3 EFG% = ", teamA_C3_EFG)
paste("Team B 2PT EFG% = ", teamB_2PT_EFG)
paste("Team B NC3 EFG% = ", teamB_NC3_EFG)
paste("Team B C3 EFG% = ", teamB_C3_EFG)

