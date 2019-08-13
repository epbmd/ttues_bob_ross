# let's see what makes a popular bob ross episode
# make manamde to nature ratio, plot against popularity

# load libraries
library(tidyverse)
library(stringr)
library(janitor)
library(tuber)

# pull data
df <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-08-06/bob-ross.csv")

# clean data (courtesy of TTuesday)
df <- df %>%
  janitor::clean_names() %>%
  separate(episode, into = c("season", "episode"), sep = "E") %>%
  mutate(season = str_extract(season, "[:digit:]+")) %>%
  mutate_at(vars(season, episode), as.integer)

# pull youtube data from bob ross channel using tuber package ----
# code to pull data with tuber adapted/copied from this excellent tutorial: 
# https://cran.r-project.org/web/packages/tuber/vignettes/tuber-ex.html

api_id = "..."
api_secret = "..."
token = "..."
yt_oauth(app_id = api_id, app_secret = api_secret, token = token)

# Get channel data
a <- list_channel_resources(filter = c(channel_id = "UCxcnsr1R5Ge_fbTu5ajt8DQ"), part="contentDetails")

# Uploaded playlists:
playlist_id <- a$items[[1]]$contentDetails$relatedPlaylists$uploads

# Get videos on the playlist
vids <- get_playlist_items(filter= c(playlist_id=playlist_id),
                           max_results = 200)
# Video ids
vid_ids <- as.vector(vids$contentDetails.videoId)

# Function to scrape stats for all vids
get_all_stats <- function(id) {
  get_stats(id)
}

# Get stats and convert results to data frame
res <- lapply(vid_ids, get_all_stats)
details <- lapply(vid_ids, get_video_details)
res_df <- do.call(rbind, lapply(res, data.frame))

details_tot <- data.frame(id = NA, title = NA)

for (p in 1:length(details)) {
  id <- details[[p]]$items[[1]]$id
  Title <- details[[p]]$items[[1]]$snippet$title

  detail <- data_frame(id = id, title = Title)
  details_tot <- rbind(detail, details_tot)
}
View(res_df)
vids<- merge(details_tot, res_df, by = "id")

# clean titles from youtube data 
# separate out 'season' and 'episode' from title to then merge with ttuesday df

vids <- vids %>%
  select(title, viewCount, likeCount, dislikeCount) %>%
  separate(title, into = c("title", "details"), sep = "[(]") %>%
  mutate(details = str_replace_all(details, c("[)]" = "",
                                              "Season" = "",
                                              "Episode" = ""))) %>%
  filter(!is.na(details)) %>%
  separate(details, into = c("season", "episode"), sep = "  ") %>%
  mutate_at(vars(season, episode), as.integer)

# make vectors of manmade vs natural structures, then compute ratio later

manmade = c("barn", "boat", "bridge", "building", "cabin", "dock", "fire",
            "fence", "mill", "path", "person", "structure", "windmill")

natural = c("aurora_borealis", "beach", "bushes", "cactus", "cirrus", "cliff", 
            "clouds", "conifer", "cumulus", "deciduous", "flowers", "fog", 
            "grass", "hills", "lake", "lakes", "moon", "mountain", "mountains", 
            "night", "ocean", "palm_trees", "river", "rocks", "snow", "snowy_mountain", 
            "sun", "tree", "trees", "waterfall", "waves", "winter")


# plot natural/manmade as function of youtube viewCounts
# first join df with vids dataframes

inner_join(df, vids %>%
                    select(season, episode, viewCount), by = c("season" = "season",
                                                               "episode" = "episode")) %>% 
  mutate(title = str_to_title(title), 
         title = str_replace_all(title, "[^[:alnum:]]", " "), 
         title = str_trim(title),
         manmade = rowSums(select(., manmade))+ 0.0000001,
         natural = rowSums(select(., natural))+ 0.0000001,
         ratio = natural/manmade,
         viewCount = as.numeric(as.character(viewCount)),
         popular_groups = cut_width(viewCount, width = 1000000)) %>%
  arrange(viewCount) %>%
  group_by(popular_groups) %>%
  summarize(mean_view = round(mean(viewCount), digits = 0),
            ratio = round(mean(ratio), digits = 2), 
            manmade = mean(manmade)) %>%
ggplot(aes(x= factor(mean_view), y = ratio, fill = factor(mean_view))) +
  geom_bar(stat = "identity") +
  labs(title = "Ratio of natural/manmade structures in Bob Ross paintings \nas a function of Youtube popularity (view count)",
       caption = "View count as of 8/10/2019")+
  xlab("\nAvg number of views per video") +
  ylab("Ratio: Natural/Manmade items") + 
  theme(legend.position = "none")

#does not be seem to be clearly related...
