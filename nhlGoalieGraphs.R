Sys.setenv(TZ = 'EST')

library(ggplot2)

###############################################################################

this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)

rm(list = ls())

###############################################################################

current <- read.csv("data/goalie_stats_20182019.csv")
current = current[complete.cases(current),]

lowDanger <-
	data.frame(
		name = current["Player"][[1]],
		ldsa = current["LDSA"][[1]],
		ldsv = current["LDSv."][[1]],
		ldga = current["LDGA"][[1]]
	)
midDanger <-
	data.frame(
		name = current["Player"][[1]],
		mdsa = current["MDSA"][[1]],
		mdsv = current["MDSv."][[1]],
		mdga = current["MDGA"][[1]]
	)
highDanger <-
	data.frame(
		name = current["Player"][[1]],
		hdsa = current["HDSA"][[1]],
		hdsv = current["HDSv."][[1]],
		hdga = current["HDGA"][[1]]
	)

###############################################################################

ldGraph <- ggplot(lowDanger, aes(x = ldsa, y = ldsv)) +
	geom_point(alpha = 0.6, aes(size = ldga), show.legend = T) +
	theme_minimal() +
	geom_text(check_overlap = TRUE,
			  label = lowDanger$name,
			  hjust = 0,
			  nudge_x = 0.5) +
	ggtitle("Low Danger Shots Against vs Save Percentage: 2018-2019") +
	labs(x = "Low Danger Shots Against",
		 y = "Low Danger Save Percentage",
		 size = "Low Danger Goals Against")

mdGraph <- ggplot(midDanger, aes(x = mdsa, y = mdsv)) +
	geom_point(alpha = 0.6, aes(size = mdga), show.legend = T) +
	theme_minimal() +
	geom_text(check_overlap = TRUE,
			  label = midDanger$name,
			  hjust = 0,
			  nudge_x = 0.5) +
	ggtitle("Mid Danger Shots Against vs Save Percentage: 2018-2019") +
	labs(x = "Mid Danger Shots Against",
		 y = "Mid Danger Save Percentage",
		 size = "Mid Danger Goals Against")

hdGraph <- ggplot(highDanger, aes(x = hdsa, y = hdsv)) +
	geom_point(alpha = 0.6, aes(size = hdga), show.legend = T) +
	theme_minimal() +
	geom_text(check_overlap = TRUE,
			  label = highDanger$name,
			  hjust = 0,
			  nudge_x = 0.5) +
	ggtitle("High Danger Shots Against vs Save Percentage: 2018-2019") +
	labs(x = "High Danger Shots Against",
		 y = "High Danger Save Percentage",
		 size = "High Danger Goals Against")

###############################################################################

# saveGraph

saveGraph <- function(graph, filename) {
	ggsave(
		filename,
		plot = graph,
		height = 30,
		width = 50,
		units = "cm"
	)
}

###############################################################################

saveGraph(ldGraph, "images/low-danger.png")
saveGraph(mdGraph, "images/mid-danger.png")
saveGraph(hdGraph, "images/high-danger.png")
