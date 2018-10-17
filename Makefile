CC = Rscript

default: normal

normal: nhl_goalie_starter_model.R
	$(CC) nhl_goalie_starter_model.R
	rm Rplots.pdf
