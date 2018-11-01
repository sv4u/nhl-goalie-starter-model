CC = Rscript

default: graphs

graphs: nhlGoalieGraphs.R
	$(CC) nhlGoalieGraphs.R
	rm Rplots.pdf
