
# Read original file.
data <- read.csv("female.csv", header = T)

# Move last column to first position.
data <- cbind(class=data$labels, data[,-ncol(data)])

# Save the data.
write.csv(data, "../data/female/data.csv", 
          quote = F, 
          row.names = F)
