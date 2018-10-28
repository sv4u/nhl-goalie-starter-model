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
	name = data01["Player"][[1]],
	starter = learningStarter01,
	gp = data01["GP"][[1]],
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
	name = data02["Player"][[1]],
	starter = learningStarter02,
	gp = data02["GP"][[1]],
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
	name = current["Player"][[1]],
	starter = currentStarters,
	gp = current["GP"][[1]],
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
	gp = murrayRaw["GP"][[1]],
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
			murrayPrediction01$net.result))

murrayPrediction02 <- compute(nn02, murrayFrame)
print(paste("Percent Chance as Starter [Murray] (nn02):",
			murrayPrediction02$net.result))

###############################################################################

raskRaw <- filter(current, current$Player == "TUUKKA.RASK")
raskFrame <- data.frame(
	gp = raskRaw["GP"][[1]],
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
			raskPrediction01$net.result))
raskPrediction02 <- compute(nn02, raskFrame)
print(paste("Percent Chance as Starter [Rask] (nn02):",
			raskPrediction02$net.result))

###############################################################################

condonRaw <- filter(current, current$Player == "MIKE.CONDON")
condonFrame <- data.frame(
	gp = condonRaw["GP"][[1]],
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
			condonPrediction01$net.result))
condonPrediction02 <- compute(nn02, condonFrame)
print(paste("Percent Chance as Starter [Condon] (nn02):",
			condonPrediction02$net.result))