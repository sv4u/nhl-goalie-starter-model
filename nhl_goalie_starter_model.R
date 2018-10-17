Sys.setenv(TZ = 'EST')

library(tidyverse)
library(neuralnet)

# good seeds: 11821L, 12508L, 10309L
# set.seed(10309L)

###############################################################################

this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)

rm(list = ls())

###############################################################################

data01 <- read.csv("data/goalie_stats_20172018.csv")
data02 <- read.csv("data/goalie_stats_20162017.csv")
testing <- read.csv("data/goalie_stats_20182019.csv")
old <- read.csv("data/goalie_stats_20152016.csv")

# Data format:
# First.Last, Season, Team, GP, TOI, Corsi Against, Fenwick Against, Shots Against,
#   Goals Against, expected Goals Against, Save Percentage, Fenwick Save Percentagem
#   expected Save Percentage, delta/adjusted Save Percentage, Low Danger SA, Low Danger GA,
#   Low Danger sv%, Medium Danger SA, Medium Danger GA, Medium Danger sv%, High Danger SA,
#   High Danger GA, High Danger sv%, Goals Saved Above Average, GSAA per 30 shots

lowDanger <-
  data.frame(
    name = data01["Player"][[1]],
    ldsa = data01["LDSA"][[1]],
    ldsv = data01["LDSv."][[1]],
    ldga = data01["LDGA"][[1]]
  )
midDanger <-
  data.frame(
    name = data01["Player"][[1]],
    mdsa = data01["MDSA"][[1]],
    mdsv = data01["MDSv."][[1]],
    mdga = data01["MDGA"][[1]]
  )
highDanger <-
  data.frame(
    name = data01["Player"][[1]],
    hdsa = data01["HDSA"][[1]],
    hdsv = data01["HDSv."][[1]],
    hdga = data01["HDGA"][[1]]
  )

###############################################################################

ldGraph <- ggplot(lowDanger, aes(x = ldsa, y = ldsv)) +
  geom_point(alpha = 0.6, aes(size = ldga), show.legend = T) +
  theme_minimal() +
  ggtitle("Low Danger Shots Against vs Save Percentage") +
  labs(x = "Low Danger Shots Against",
       y = "Low Danger Save Percentage",
       size = "Low Danger Goals Against")

mdGraph <- ggplot(midDanger, aes(x = mdsa, y = mdsv)) +
  geom_point(alpha = 0.6, aes(size = mdga), show.legend = T) +
  theme_minimal() +
  ggtitle("Mid Danger Shots Against vs Save Percentage") +
  labs(x = "Mid Danger Shots Against",
       y = "Mid Danger Save Percentage",
       size = "Mid Danger Goals Against")

hdGraph <- ggplot(highDanger, aes(x = hdsa, y = hdsv)) +
  geom_point(alpha = 0.6, aes(size = hdga), show.legend = T) +
  theme_minimal() +
  ggtitle("High Danger Shots Against vs Save Percentage") +
  labs(x = "High Danger Shots Against",
       y = "High Danger Save Percentage",
       size = "High Danger Goals Against")

###############################################################################

# calculateError

calculateError <- function(actual, predicted) {
  if (length(actual) != length(predicted))
    return ~ 1.0
  
  total = length(actual) * 1.0
  error = 0.0
  
  for (i in 1:length(actual))
    if (predicted[i] - actual[i] != 0)
      error = error + 1.0
  
  return (error / total)
}

###############################################################################

learningStarter01 <- list(rep(0, length(data01$Player)))[[1]]
learningStarter01[13:18] = 1
learningStarter01[24:27] = 1
learningStarter01[31] = 1
learningStarter01[33] = 1
learningStarter01[35] = 1
learningStarter01[37:39] = 1
learningStarter01[44:47] = 1
learningStarter01[57:60] = 1
learningStarter01[62] = 1
learningStarter01[64] = 1
learningStarter01[66] = 1
learningStarter01[70:71] = 1
learningStarter01[73] = 1
learningStarter01[75:76] = 1
learningStarter01[80] = 1

learning01 <- data.frame(
  name = data01["Player"][[1]],
  starter = learningStarter01,
  sa = data01["SA"][[1]],
  ga = data01["GA"][[1]],
  sv = data01["Sv."][[1]],
  ldsa = data01["LDSA"][[1]],
  ldga = data01["LDGA"][[1]],
  ldsv = data01["LDSv."][[1]],
  mdsa = data01["MDSA"][[1]],
  mdga = data01["MDGA"][[1]],
  mdsv = data01["MDSv."][[1]],
  hdsa = data01["HDSA"][[1]],
  hdga = data01["HDGA"][[1]],
  hdsv = data01["HDSv."][[1]]
)

learning01 <- learning01[complete.cases(learning01), ]

features <- names(learning01[3:14])
form <- paste(features, collapse = " + ")
form <- paste('learning01$starter ~', form)
form <- as.formula(form)

nn01 <-
  neuralnet(form,
            learning01[3:14],
            hidden = c(10, 10, 10),
            linear.output = FALSE)

###############################################################################

learningStarter02 = list(rep(0, length(data02$Player)))[[1]]
learningStarter02[6] = 1
learningStarter02[12] = 1
learningStarter02[14:20] = 1
learningStarter02[25:28] = 1
learningStarter02[32] = 1
learningStarter02[36:37] = 1
learningStarter02[40:41] = 1
learningStarter02[49] = 1
learningStarter02[58] = 1
learningStarter02[66] = 1
learningStarter02[69:70] = 1
learningStarter02[75] = 1
learningStarter02[77:79] = 1
learningStarter02[84:86] = 1
learningStarter02[89] = 1
learningStarter02[91:92] = 1
learningStarter02[94] = 1

learning02 <- data.frame(
  name = data02["Player"][[1]],
  starter = learningStarter02,
  sa = data02["SA"][[1]],
  ga = data02["GA"][[1]],
  sv = data02["Sv."][[1]],
  ldsa = data02["LDSA"][[1]],
  ldga = data02["LDGA"][[1]],
  ldsv = data02["LDSv."][[1]],
  mdsa = data02["MDSA"][[1]],
  mdga = data02["MDGA"][[1]],
  mdsv = data02["MDSv."][[1]],
  hdsa = data02["HDSA"][[1]],
  hdga = data02["HDGA"][[1]],
  hdsv = data02["HDSv."][[1]]
)

learning02 <- learning02[complete.cases(learning02), ]

###############################################################################

learning03 = rbind(learning01, learning02)

features <- names(learning03[3:14])
form <- paste(features, collapse = " + ")
form <- paste('learning03$starter ~', form)
form <- as.formula(form)

nn02 <-
  neuralnet(form,
            learning03[3:14],
            hidden = c(10, 10, 10),
            linear.output = FALSE)

###############################################################################

testingStarters = list(rep(0, length(testing$Player)))[[1]]
testingStarters[4] = 1
testingStarters[7:10] = 1
testingStarters[12:15] = 1
testingStarters[18:19] = 1
testingStarters[22:23] = 1
testingStarters[25] = 1
testingStarters[27:29] = 1
testingStarters[31:32] = 1
testingStarters[34] = 1
testingStarters[41:43] = 1
testingStarters[45:46] = 1
testingStarters[54:57] = 1

testingData <- data.frame(
  name = testing["Player"][[1]],
  starter = testingStarters,
  sa = testing["SA"][[1]],
  ga = testing["GA"][[1]],
  sv = testing["Sv."][[1]],
  ldsa = testing["LDSA"][[1]],
  ldga = testing["LDGA"][[1]],
  ldsv = testing["LDSv."][[1]],
  mdsa = testing["MDSA"][[1]],
  mdga = testing["MDGA"][[1]],
  mdsv = testing["MDSv."][[1]],
  hdsa = testing["HDSA"][[1]],
  hdga = testing["HDGA"][[1]],
  hdsv = testing["HDSv."][[1]]
)

testingData <- testingData[complete.cases(testingData), ]

###############################################################################

print("nn01: based on 2017-2018 data")
print("nn02: based on 2017-2018 and 2016-2017 data")

###############################################################################

nn01predictOldGoalie <- compute(nn01, learning02[3:14])
nn01predictOldGoalie$round <-
  sapply(nn01predictOldGoalie$net.result, round, digits = 0)

print(paste(
  "2015-2016 prediction (nn01) error rate:",
  calculateError(learning02$starter, nn01predictOldGoalie$round)
))

###############################################################################

nn01predictCurrentGoalie <- compute(nn01, testingData[3:14])
nn01predictCurrentGoalie$round <-
  sapply(nn01predictCurrentGoalie$net.result, round, digits = 0)

print(paste(
  "2018-2019 prediction (nn01) error rate:",
  calculateError(testingData$starter, nn01predictCurrentGoalie$round)
))

###############################################################################

nn02predictOldGoalie <- compute(nn02, learning02[3:14])
nn02predictOldGoalie$round <-
  sapply(nn02predictOldGoalie$net.result, round, digits = 0)

print(paste(
  "2015-2016 prediction (nn02) error rate:",
  calculateError(learning02$starter, nn02predictOldGoalie$round)
))

###############################################################################

nn02predictCurrentGoalie <- compute(nn02, testingData[3:14])
nn02predictCurrentGoalie$round <-
  sapply(nn02predictCurrentGoalie$net.result, round, digits = 0)

print(paste(
  "2018-2019 prediction (nn02) error rate:",
  calculateError(testingData$starter, nn02predictCurrentGoalie$round)
))
