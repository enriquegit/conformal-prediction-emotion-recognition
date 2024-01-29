
# Read original file.
data <- read.csv("music.csv", header = T)

# Delete first column.
data <- data[,-1]

# Move last column to first position.
data <- cbind(class=data$label, data[,-ncol(data)])

# Save the data.
write.csv(data, "../data/music/data.csv", 
          quote = F, 
          row.names = F)
