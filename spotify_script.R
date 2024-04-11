# Load the spotifyr library
install.packages("spotifyr")
library(spotifyr)
library(tidyverse)
library(scales)

mood_choices = list("Happy" = "happy",
                    "Angry" = "angry",
                    "Sad" = "sad",
                    "Chill" = "chill")

genre_choices = list("Pop" = "pop",
                     "Rock" = "rock",
                     "Hip-Hop" = "hiphop",
                     "Jazz" = "jazz",
                     "Electronic" = "electronic")


