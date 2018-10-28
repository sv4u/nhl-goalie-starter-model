Sys.setenv(TZ = 'EST')

library(tidyverse)
library(neuralnet)

# good seeds: 11821L, 12508L, 10309L
# set.seed(10309L)

###############################################################################

#this.dir <- dirname(parent.frame(2)$ofile)
#setwd(this.dir)

#rm(list = ls())

###############################################################################

data01 <- read.csv("data/goalie_stats_20172018.csv")
data02 <- read.csv("data/goalie_stats_20162017.csv")
current <- read.csv("data/goalie_stats_20182019.csv")
old <- read.csv("data/goalie_stats_20152016.csv")

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

# saveGraph

saveGraph <- function(graph, filename) {
	ggsave(
		filename,
		plot = graph,
		height = 27,
		width = 48,
		units = "cm"
	)
}

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

learning01 <- learning01[complete.cases(learning01),]

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

learning02 <- learning02[complete.cases(learning02),]

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

currentStarters = list(rep(0, length(current$Player)))[[1]]
currentStarters[4] = 1
currentStarters[8:10] = 1
currentStarters[13:16] = 1
currentStarters[19:21] = 1
currentStarters[25:26] = 1
currentStarters[28] = 1
currentStarters[31] = 1
currentStarters[34:37] = 1
currentStarters[45:47] = 1
currentStarters[50:51] = 1
currentStarters[56] = 1
currentStarters[59:60] = 1
currentStarters[62] = 1

currentData <- data.frame(
	name = current["Player"][[1]],
	starter = currentStarters,
	sa = current["SA"][[1]],
	ga = current["GA"][[1]],
	sv = current["Sv."][[1]],
	ldsa = current["LDSA"][[1]],
	ldga = current["LDGA"][[1]],
	ldsv = current["LDSv."][[1]],
	mdsa = current["MDSA"][[1]],
	mdga = current["MDGA"][[1]],
	mdsv = current["MDSv."][[1]],
	hdsa = current["HDSA"][[1]],
	hdga = current["HDGA"][[1]],
	hdsv = current["HDSv."][[1]]
)

currentData <- currentData[complete.cases(currentData),]

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

nn01predictCurrentGoalie <- compute(nn01, currentData[3:14])
nn01predictCurrentGoalie$round <-
	sapply(nn01predictCurrentGoalie$net.result, round, digits = 0)

print(paste(
	"2018-2019 prediction (nn01) error rate:",
	calculateError(currentData$starter, nn01predictCurrentGoalie$round)
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

nn02predictCurrentGoalie <- compute(nn02, currentData[3:14])
nn02predictCurrentGoalie$round <-
	sapply(nn02predictCurrentGoalie$net.result, round, digits = 0)

print(paste(
	"2018-2019 prediction (nn02) error rate:",
	calculateError(currentData$starter, nn02predictCurrentGoalie$round)
))

###############################################################################

saveGraph(ldGraph, "images/low-danger.png")
saveGraph(mdGraph, "images/mid-danger.png")
saveGraph(hdGraph, "images/high-danger.png")

###############################################################################
murrayRaw <- filter(current, current$Player == "MATT.MURRAY")
murrayFrame <- data.frame(
	sa = murrayRaw["SA"][[1]],
	ga = murrayRaw["GA"][[1]],
	sv = murrayRaw["Sv."][[1]],
	ldsa = murrayRaw["LDSA"][[1]],
	ldga = murrayRaw["LDGA"][[1]],
	ldsv = murrayRaw["LDSv."][[1]],
	mdsa = murrayRaw["MDSA"][[1]],
	mdga = murrayRaw["MDGA"][[1]],
	mdsv = murrayRaw["MDSv."][[1]],
	hdsa = murrayRaw["HDSA"][[1]],
	hdga = murrayRaw["HDGA"][[1]],
	hdsv = murrayRaw["HDSv."][[1]]
)

murrayPrediction01 <- compute(nn01, murrayFrame)
print(paste("Percent Chance as Starter [Murray] (nn01):",
			murrayPrediction01$net.result * 100))

murrayPrediction02 <- compute(nn02, murrayFrame)
print(paste("Percent Chance as Starter [Murray] (nn02):",
			murrayPrediction02$net.result * 100))

###############################################################################

raskRaw <- filter(current, current$Player == "TUUKKA.RASK")
raskFrame <- data.frame(
	sa = raskRaw["SA"][[1]],
	ga = raskRaw["GA"][[1]],
	sv = raskRaw["Sv."][[1]],
	ldsa = raskRaw["LDSA"][[1]],
	ldga = raskRaw["LDGA"][[1]],
	ldsv = raskRaw["LDSv."][[1]],
	mdsa = raskRaw["MDSA"][[1]],
	mdga = raskRaw["MDGA"][[1]],
	mdsv = raskRaw["MDSv."][[1]],
	hdsa = raskRaw["HDSA"][[1]],
	hdga = raskRaw["HDGA"][[1]],
	hdsv = raskRaw["HDSv."][[1]]
)

raskPrediction01 <- compute(nn01, raskFrame)
print(paste("Percent Chance as Starter [Rask] (nn01):",
			raskPrediction01$net.result * 100))
raskPrediction02 <- compute(nn02, raskFrame)
print(paste("Percent Chance as Starter [Rask] (nn02):",
			raskPrediction02$net.result * 100))

###############################################################################

condonRaw <- filter(current, current$Player == "MIKE.CONDON")
condonFrame <- data.frame(
	sa = condonRaw["SA"][[1]],
	ga = condonRaw["GA"][[1]],
	sv = condonRaw["Sv."][[1]],
	ldsa = condonRaw["LDSA"][[1]],
	ldga = condonRaw["LDGA"][[1]],
	ldsv = condonRaw["LDSv."][[1]],
	mdsa = condonRaw["MDSA"][[1]],
	mdga = condonRaw["MDGA"][[1]],
	mdsv = condonRaw["MDSv."][[1]],
	hdsa = condonRaw["HDSA"][[1]],
	hdga = condonRaw["HDGA"][[1]],
	hdsv = condonRaw["HDSv."][[1]]
)

condonPrediction01 <- compute(nn01, condonFrame)
print(paste("Percent Chance as Starter [Condon] (nn01):",
			condonPrediction01$net.result * 100))
condonPrediction02 <- compute(nn02, condonFrame)
print(paste("Percent Chance as Starter [Condon] (nn02):",
			condonPrediction02$net.result * 100))