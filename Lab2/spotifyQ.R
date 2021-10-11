library(dplyr)

# Get the Spotify file
file <- read.csv("spotifyplaylists.csv")
# Getting overviews of the files
head(file)
str(file)
dim(file)

# Turn the file into a data frame
data_frame <- read.csv("spotifyplaylists.csv")


# Use a pipe on the data frame to calculate statistics

# Finding the average duration
data_frame %>%
  summarise(aveDur = mean(duration_ms))


# Finding the average number of beats
data_frame %>%
  summarise(aveBeats = mean(tempo))


# Finding the proportion of songs for each mode
# Mode 1
data_frame %>%
  # Note that summarise creates a new data frame, so we can just find the
  # Length of this data frame for each mode
  # Filter data frame for only column entries that have 1 for their mode
  filter(mode == 1) %>%
  # Use summarise to count the number of rows for this filtered data frame
  summarise(mode1_prop = n() / nrow(data_frame))
# Mode 0
data_frame %>%
  filter(mode == 0) %>%
  summarise(mode0_prop = n() / nrow(data_frame))


# Finding the total duration of each playlist
df <- read.csv("spotifyplaylists.csv")
df %>%
  count(playlist_id)
# FINISH


# Finding the average valence for each mode
df %>%
  # how to automate this so it does this for each mode automatically?


  # Finding the loudest song
  data_frame() %>%
  filter(loudness == max(loudness)) %>%
  select(c("track_name", "loudness", "danceability"))


# Finding the most and least dancable song
least_danceable <- min(data_frame["danceability"])
most_danceable <- max(data_frame["danceability"])
data_frame %>%
  filter(danceability == least_danceable | danceability == most_danceable) %>%
  select(c("track_name", "danceability"))


# Find how many songs are more danceable that the loudest song
loudest_song_dance <- 0.8 # From above
data_frame %>%
  filter(danceability > loudest_song_dance) %>%
  summarise(Number_of_Songs = n())

# The five happiest songs
data_frame[order(data_frame$liveness, decreasing = TRUE), ]
data_frame <- data_frame[1:5, ]
data_frame %>%
  summarise(Name = track_name, Happiness = liveness)

# Creating a scatterplot of danceability vs tempo
df <- read.csv("spotifyplaylists.csv")
new_data_frame <- select(df, c("danceability", "tempo"))
plot(new_data_frame)

# Creating a box plot for valence grouped by each key
boxplot(valence ~ key, data = df)