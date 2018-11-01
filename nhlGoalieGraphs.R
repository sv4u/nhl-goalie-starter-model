Sys.setenv(TZ = 'EST')

library(tidyverse)

###############################################################################

setwd(getwd())

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
	labs(x = "Low Danger Shots Against", y = "Low Danger Save Percentage", size = "Low Danger Goals Against")

mdGraph <- ggplot(midDanger, aes(x = mdsa, y = mdsv)) +
	geom_point(alpha = 0.6, aes(size = mdga), show.legend = T) +
	theme_minimal() +
	ggtitle("Mid Danger Shots Against vs Save Percentage") +
	labs(x = "Mid Danger Shots Against", y = "Mid Danger Save Percentage", size = "Mid Danger Goals Against")

hdGraph <- ggplot(highDanger, aes(x = hdsa, y = hdsv)) +
	geom_point(alpha = 0.6, aes(size = hdga), show.legend = T) +
	theme_minimal() +
	ggtitle("High Danger Shots Against vs Save Percentage") +
	labs(x = "High Danger Shots Against", y = "High Danger Save Percentage", size = "High Danger Goals Against")

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

###############################################################################

saveGraph(ldGraph, "images/low-danger.png")
saveGraph(mdGraph, "images/mid-danger.png")
saveGraph(hdGraph, "images/high-danger.png")
