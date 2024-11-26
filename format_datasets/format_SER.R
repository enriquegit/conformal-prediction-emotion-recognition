
#### Female data ####

# Read original file.
data <- read.csv("Female_features.csv", header = T)

# Move last column to first position.
data <- cbind(class=data$labels, data[,-ncol(data)])

# Save the data.
write.csv(data, "../data/female/data.csv", 
          quote = F, 
          row.names = F)

#### Male data ####

# Read original file.
data <- read.csv("Male_features.csv", header = T)

# Move last column to first position.
data <- cbind(class=data$labels, data[,-ncol(data)])

# Save the data.
write.csv(data, "../data/male/data.csv", 
          quote = F, 
          row.names = F)

#### Combined data ####

# Read original file.
dataf <- read.csv("Female_features.csv", header = T)

datam <- read.csv("Male_features.csv", header = T)

data <- rbind(dataf, datam)

# Move last column to first position.
data <- cbind(class=data$labels, data[,-ncol(data)])

# Save the data.
write.csv(data, "../data/combined/data.csv", 
          quote = F, 
          row.names = F)
