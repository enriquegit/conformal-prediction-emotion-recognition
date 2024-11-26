source("format_results_functions.R")
library(reticulate)

reticulate::source_python("..//notebooks//globals.py")



pwidth <- 5; pheight <- 5 # Set the plots width and height.

res1 <- summarize.iterations(DATASET_PATH, "1")

res2 <- summarize.all(DATASET_PATH, "1")

#pairwise.occurrences(DATASET_PATH, "1", pwidth, pheight)

#plot.confusion.matrix(DATASET_PATH, "1", pwidth, pheight)

plot.scatter(DATASET_PATH)

plot.allInOne(DATASET_PATH, "1", 10, 5)

latex.summary(DATASET_PATH, "1")
