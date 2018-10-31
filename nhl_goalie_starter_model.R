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

print("nn01: based on 2017-2018 data")
print("nn02: based on 2017-2018 and 2016-2017 data")

###############################################################################

learningStarter01 <- list(rep(0, length(data01$Player)))[[1]]
learningStarter01[which(data01$Player == "ANDREI.VASILEVSKIY")] = 1
learningStarter01[which(data01$Player == "CAM.TALBOT")] = 1
learningStarter01[which(data01$Player == "CONNOR.HELLEBUYCK")] = 1
learningStarter01[which(data01$Player == "FREDERIK.ANDERSON")] = 1
learningStarter01[which(data01$Player == "SERGEI.BOBROVSKY")] = 1
learningStarter01[which(data01$Player == "JONATHAN.QUICK")] = 1
learningStarter01[which(data01$Player == "HENRIK.LUNDQVIST")] = 1
learningStarter01[which(data01$Player == "DEVAN.DUBNYK")] = 1
learningStarter01[which(data01$Player == "JOHN.GIBSON")] = 1
learningStarter01[which(data01$Player == "MARTIN.JONES")] = 1
learningStarter01[which(data01$Player == "JACOB.MARKSTROM")] = 1
learningStarter01[which(data01$Player == "JAKE.ALLEN")] = 1
learningStarter01[which(data01$Player == "JIMMY.HOWARD")] = 1
learningStarter01[which(data01$Player == "PEKKA.RINNE")] = 1
learningStarter01[which(data01$Player == "CRAIG.ANDERSON")] = 1
learningStarter01[which(data01$Player == "MIKE.SMITH")] = 1
learningStarter01[which(data01$Player == "BRADEN.HOLTBY")] = 1
learningStarter01[which(data01$Player == "JAROSLAV.HALAK")] = 1
learningStarter01[which(data01$Player == "TUUKKA.RASK")] = 1
learningStarter01[which(data01$Player == "BEN.BISHOP")] = 1
learningStarter01[which(data01$Player == "ROBIN.LEHNER")] = 1
learningStarter01[which(data01$Player == "SEMYON.VARLAMOV")] = 1
learningStarter01[which(data01$Player == "CAREY.PRICE")] = 1
learningStarter01[which(data01$Player == "ANTTI.RAANTA")] = 1
learningStarter01[which(data01$Player == "MARC-ANDRE.FLEURY")] = 1
learningStarter01[which(data01$Player == "JAMES.REIMER")] = 1
learningStarter01[which(data01$Player == "BRIAN.ELLIOT")] = 1
learningStarter01[which(data01$Player == "CAM.WARD")] = 1
learningStarter01[which(data01$Player == "SCOTT.DARLING")] = 1
learningStarter01[which(data01$Player == "KEITH.KINKAID")] = 1
learningStarter01[which(data01$Player == "CORY.SCHNEIDER")] = 1
learningStarter01[which(data01$Player == "PETR.MRAZEK")] = 1
learningStarter01[which(data01$Player == "MATTHEW.MURRAY")] = 1
learningStarter01[which(data01$Player == "MATT.MURRAY")] = 1

learning01 <- data.frame(
	name = data01["Player"],
	starter = learningStarter01,
	gp = data01["GP"] / max(data01["GP"]),
	sa = data01["SA"] / max(data01["SA"]),
	ga = data01["GA"] / max(data01["GA"]),
	sv = data01["Sv."] / max(data01["Sv."]),
	ldsa = data01["LDSA"] / max(data01["LDSA"]),
	ldga = data01["LDGA"] / max(data01["LDGA"]),
	ldsv = data01["LDSv."] / max(data01["LDSv."]),
	mdsa = data01["MDSA"] / max(data01["MDSA"]),
	mdga = data01["MDGA"] / max(data01["MDSA"]),
	mdsv = data01["MDSv."] / max(data01["MDSv."]),
	hdsa = data01["HDSA"] / max(data01["HDSA"]),
	hdga = data01["HDGA"] / max(data01["HDGA"]),
	hdsv = data01["HDSv."] / max(data01["HDSv."])
)

learning01 <- learning01[complete.cases(learning01),]

features <- names(learning01[3:15])
form <- paste(features, collapse = " + ")
form <- paste('learning01$starter ~', form)
form <- as.formula(form)

nn01 <-
	neuralnet(form,
			  learning01[3:15],
			  hidden = c(5, 5, 5, 5, 5, 5, 5, 5, 5, 5),
			  linear.output = FALSE,
			  algorithm = "rprop-",
			  learningrate = 0.05)

###############################################################################

learningStarter02 = list(rep(0, length(data02$Player)))[[1]]
learningStarter02[which(data02$Player == "CAM.TALBOT")] = 1
learningStarter02[which(data02$Player == "FREDERIK.ANDERSON")] = 1
learningStarter02[which(data02$Player == "DEVAN.DUBNYK")] = 1
learningStarter02[which(data02$Player == "TUUKKA.RASK")] = 1
learningStarter02[which(data02$Player == "MARTIN.JONES")] = 1
learningStarter02[which(data02$Player == "BRADEN.HOLTBY")] = 1
learningStarter02[which(data02$Player == "SERGEI.BOBROVSKY")] = 1
learningStarter02[which(data02$Player == "CAREY.PRICE")] = 1
learningStarter02[which(data02$Player == "CAM.WARD")] = 1
learningStarter02[which(data02$Player == "JAKE.ALLEN")] = 1
learningStarter02[which(data02$Player == "PEKKA.RINNE")] = 1
learningStarter02[which(data02$Player == "CORY.SCHNEIDER")] = 1
learningStarter02[which(data02$Player == "PETER.BUDAJ")] = 1
learningStarter02[which(data02$Player == "KARI.LEHTONEN")] = 1
learningStarter02[which(data02$Player == "ROBIN.LEHNER")] = 1
learningStarter02[which(data02$Player == "STEVE.MASON")] = 1
learningStarter02[which(data02$Player == "HENRIK.LUNDQVIST")] = 1
learningStarter02[which(data02$Player == "CONNOR.HELLEBUYCK")] = 1
learningStarter02[which(data02$Player == "COREY.CRAWFORD")] = 1
learningStarter02[which(data02$Player == "MIKE.SMITH")] = 1
learningStarter02[which(data02$Player == "RYAN.MILLER")] = 1
learningStarter02[which(data02$Player == "JOHN.GIBSON")] = 1
learningStarter02[which(data02$Player == "THOMAS.GREISS")] = 1
learningStarter02[which(data02$Player == "ANDREI.VASILEVSKIY")] = 1
learningStarter02[which(data02$Player == "CALVIN.PICKARD")] = 1
learningStarter02[which(data02$Player == "PETR.MRAZEK")] = 1
learningStarter02[which(data02$Player == "BRIAN.ELLIOT")] = 1
learningStarter02[which(data02$Player == "MATTHEW.MURRAY")] = 1
learningStarter02[which(data02$Player == "JAMES.REIMER")] = 1
learningStarter02[which(data02$Player == "MIKE.CONDON")] = 1
learningStarter02[which(data02$Player == "CRAIG.ANDERSON")] = 1
learningStarter02[which(data02$Player == "ROBERTO.LUONGO")] = 1

learning02 <- data.frame(
	name = data02["Player"],
	starter = learningStarter02,
	gp = data02["GP"] / max(data02["GP"]),
	sa = data02["SA"] / max(data02["SA"]),
	ga = data02["GA"] / max(data02["GA"]),
	sv = data02["Sv."] / max(data02["Sv."]),
	ldsa = data02["LDSA"] / max(data02["LDSA"]),
	ldga = data02["LDGA"] / max(data02["LDGA"]),
	ldsv = data02["LDSv."] / max(data02["LDSv."]),
	mdsa = data02["MDSA"] / max(data02["MDSA"]),
	mdga = data02["MDGA"] / max(data02["MDSA"]),
	mdsv = data02["MDSv."] / max(data02["MDSv."]),
	hdsa = data02["HDSA"] / max(data02["HDSA"]),
	hdga = data02["HDGA"] / max(data02["HDGA"]),
	hdsv = data02["HDSv."] / max(data02["HDSv."])
)

learning02 <- learning02[complete.cases(learning02),]

###############################################################################

currentStarters = list(rep(0, length(current$Player)))[[1]]
currentStarters[which(current$Player == "JOHN.GIBSON")] = 1
currentStarters[which(current$Player == "ANTTI.RAANTA")] = 1
currentStarters[which(current$Player == "TUUKKA.RASK")] = 1
currentStarters[which(current$Player == "CARTER.HUTTON")] = 1
currentStarters[which(current$Player == "MIKE.SMITH")] = 1
currentStarters[which(current$Player == "COREY.CRAWFORD")] = 1
currentStarters[which(current$Player == "SEMYON.VARLAMOV")] = 1
currentStarters[which(current$Player == "SERGEI.BOBROVSKY")] = 1
currentStarters[which(current$Player == "BEN.BISHOP")] = 1
currentStarters[which(current$Player == "JIMMY.HOWARD")] = 1
currentStarters[which(current$Player == "CAM.TALBOT")] = 1
currentStarters[which(current$Player == "ROBERTO.LUONGO")] = 1
currentStarters[which(current$Player == "JAMES.REIMER")] = 1
currentStarters[which(current$Player == "JONATHAN.QUICK")] = 1
currentStarters[which(current$Player == "DEVAN.DUBNYK")] = 1
currentStarters[which(current$Player == "CAREY.PRICE")] = 1
currentStarters[which(current$Player == "PEKKA.RINNE")] = 1
currentStarters[which(current$Player == "JUUSE.SAROS")] = 1
currentStarters[which(current$Player == "KEITH.KINKAID")] = 1
currentStarters[which(current$Player == "CORY.SCHNEIDER")] = 1
currentStarters[which(current$Player == "HENRIK.LUNDQVIST")] = 1
currentStarters[which(current$Player == "CRAIG.ANDERSON")] = 1
currentStarters[which(current$Player == "BRAIN.ELLIOT")] = 1
currentStarters[which(current$Player == "MATT.MURRAY")] = 1
currentStarters[which(current$Player == "JAKE.ALLEN")] = 1
currentStarters[which(current$Player == "MARTIN.JONES")] = 1
currentStarters[which(current$Player == "ANDREI.VASILEVSKIY")] = 1
currentStarters[which(current$Player == "FREDERIK.ANDERSON")] = 1
currentStarters[which(current$Player == "ANDERS.NILSSON")] = 1
currentStarters[which(current$Player == "MARC-ANDRE.FLEURY")] = 1
currentStarters[which(current$Player == "BRADEN.HOLTBY")] = 1
currentStarters[which(current$Player == "CONNER.HELLEBUYCK")] = 1

currentData <- data.frame(
	name = current["Player"],
	starter = currentStarters,
	gp = current["GP"] / max(current["GP"]),
	sa = current["SA"] / max(current["SA"]),
	ga = current["GA"] / max(current["GA"]),
	sv = current["Sv."] / max(current["Sv."]),
	ldsa = current["LDSA"] / max(current["LDSA"]),
	ldga = current["LDGA"] / max(current["LDGA"]),
	ldsv = current["LDSv."] / max(current["LDSv."]),
	mdsa = current["MDSA"] / max(current["MDSA"]),
	mdga = current["MDGA"] / max(current["MDGA"]),
	mdsv = current["MDSv."] / max(current["MDSv."]),
	hdsa = current["HDSA"] / max(current["HDSA"]),
	hdga = current["HDGA"] / max(current["HDGA"]),
	hdsv = current["HDSv."] / max(current["HDSv."])
)

currentData <- currentData[complete.cases(currentData),]

###############################################################################

nn01predictOldGoalie <- compute(nn01, learning02[3:15])
nn01predictOldGoalie$round <-
	sapply(nn01predictOldGoalie$net.result, round, digits = 0)

print(paste(
	"2015-2016 prediction (nn01) error rate:",
	calculateError(learning02$starter, nn01predictOldGoalie$round)
))

###############################################################################

learning03 = rbind(learning01, learning02)

features <- names(learning03[3:15])
form <- paste(features, collapse = " + ")
form <- paste('learning03$starter ~', form)
form <- as.formula(form)

nn02 <-
	neuralnet(form,
			  learning03[3:15],
			  hidden = c(5, 5, 5, 5, 5, 5, 5, 5, 5, 5),
			  linear.output = FALSE,
			  algorithm = "rprop-",
			  learningrate = 0.05)

###############################################################################

nn02predictOldGoalie <- compute(nn02, learning02[3:15])
nn02predictOldGoalie$round <-
	sapply(nn02predictOldGoalie$net.result, round, digits = 0)

print(paste(
	"2015-2016 prediction (nn02) error rate:",
	calculateError(learning02$starter, nn02predictOldGoalie$round)
))

###############################################################################

saveGraph(ldGraph, "images/low-danger.png")
saveGraph(mdGraph, "images/mid-danger.png")
saveGraph(hdGraph, "images/high-danger.png")

###############################################################################

murrayRaw <- filter(current, current$Player == "MATT.MURRAY")
murrayFrame <- data.frame(
	gp = murrayRaw["GP"] / max(current["GP"]),
	sa = murrayRaw["SA"] / max(current["SA"]),
	ga = murrayRaw["GA"] / max(current["GA"]),
	sv = murrayRaw["Sv."] / max(current["Sv."]),
	ldsa = murrayRaw["LDSA"] / max(current["LDSA"]),
	ldga = murrayRaw["LDGA"] / max(current["LDGA"]),
	ldsv = murrayRaw["LDSv."] / max(current["LDSv."]),
	mdsa = murrayRaw["MDSA"] / max(current["MDSA"]),
	mdga = murrayRaw["MDGA"] / max(current["MDGA"]),
	mdsv = murrayRaw["MDSv."] / max(current["MDSv."]),
	hdsa = murrayRaw["HDSA"] / max(current["HDSA"]),
	hdga = murrayRaw["HDGA"] / max(current["HDSA"]),
	hdsv = murrayRaw["HDSv."] / max(current["HDSv."])
)

murrayPrediction01 <- compute(nn01, murrayFrame)
print(paste("Percent Chance as Starter [Murray] (nn01):",
			murrayPrediction01$net.result))

murrayPrediction02 <- compute(nn02, murrayFrame)
print(paste("Percent Chance as Starter [Murray] (nn02):",
			murrayPrediction02$net.result))

###############################################################################

raskRaw <- filter(current, current$Player == "TUUKKA.RASK")
raskFrame <- data.frame(
	gp = raskRaw["GP"] / max(current["GP"]),
	sa = raskRaw["SA"] / max(current["SA"]),
	ga = raskRaw["GA"] / max(current["GA"]),
	sv = raskRaw["Sv."] / max(current["Sv."]),
	ldsa = raskRaw["LDSA"] / max(current["LDSA"]),
	ldga = raskRaw["LDGA"] / max(current["LDGA"]),
	ldsv = raskRaw["LDSv."] / max(current["LDSv."]),
	mdsa = raskRaw["MDSA"] / max(current["MDSA"]),
	mdga = raskRaw["MDGA"] / max(current["MDGA"]),
	mdsv = raskRaw["MDSv."] / max(current["MDSv."]),
	hdsa = raskRaw["HDSA"] / max(current["HDSA"]),
	hdga = raskRaw["HDGA"] / max(current["HDSA"]),
	hdsv = raskRaw["HDSv."] / max(current["HDSv."])
)

raskPrediction01 <- compute(nn01, raskFrame)
print(paste("Percent Chance as Starter [Rask] (nn01):",
			raskPrediction01$net.result))
raskPrediction02 <- compute(nn02, raskFrame)
print(paste("Percent Chance as Starter [Rask] (nn02):",
			raskPrediction02$net.result))

###############################################################################

condonRaw <- filter(current, current$Player == "MIKE.CONDON")
condonFrame <- data.frame(
	gp = condonRaw["GP"] / max(current["GP"]),
	sa = condonRaw["SA"] / max(current["SA"]),
	ga = condonRaw["GA"] / max(current["GA"]),
	sv = condonRaw["Sv."] / max(current["Sv."]),
	ldsa = condonRaw["LDSA"] / max(current["LDSA"]),
	ldga = condonRaw["LDGA"] / max(current["LDGA"]),
	ldsv = condonRaw["LDSv."] / max(current["LDSv."]),
	mdsa = condonRaw["MDSA"] / max(current["MDSA"]),
	mdga = condonRaw["MDGA"] / max(current["MDGA"]),
	mdsv = condonRaw["MDSv."] / max(current["MDSv."]),
	hdsa = condonRaw["HDSA"] / max(current["HDSA"]),
	hdga = condonRaw["HDGA"] / max(current["HDSA"]),
	hdsv = condonRaw["HDSv."] / max(current["HDSv."])
)

condonPrediction01 <- compute(nn01, condonFrame)
print(paste("Percent Chance as Starter [Condon] (nn01):",
			condonPrediction01$net.result))
condonPrediction02 <- compute(nn02, condonFrame)
print(paste("Percent Chance as Starter [Condon] (nn02):",
			condonPrediction02$net.result))