library(rtweet)
library(gridExtra)
library(dplyr)
library(lubridate)
library(ggplot2)
library(ggraph)
library(igraph)
library(ggiraph)


# R.home()
# 
# user_renviron = path.expand(file.path("~", ".Renviron"))
# if(!file.exists(user_renviron)) # check to see if the file already exists
#   file.create(user_renviron)
# 
# file.edit(user_renviron)
# 
# Sys.getenv("TWITTER_APP")
# 
# create_token(
#   app = Sys.getenv("TWITTER_APP"),
#   consumer_key = Sys.getenv("TWITTER_CONSUMER_KEY"),
#   consumer_secret = Sys.getenv("TWITTER_CONSUMER_SECRET")
# ) -> twitter_token

rladies_tweets <- get_timeline("rladies_iwd2018", n = 500)
rladies_tweets <- arrange(rladies_tweets, created_at)
head(rladies_tweets[,"created_at"])

names(rladies_tweets)

# favorites
janitor::tabyl(rladies_tweets$favorite_count)

rladies_tweets %>%
  subset(favorite_count > 30) %>%
  select(status_id, favorite_count, text) %>%
  arrange(desc(favorite_count))

twitter_handles_fav <- c("@ledell", "@ma_salmon", "@minebocek", 
                         "@JennyBryan", "@juliasilge", "@gdequeiroz", 
                         "@AmeliaMN", "@Zofiathewitch", "@lornamariak")

rladies_tweets %>%
  subset(favorite_count > 30 & status_id != "971152051991384065") %>%
  select(status_id, favorite_count, text) %>%
  arrange(desc(favorite_count)) %>%
  cbind(twitter_handles_fav, .)

# retweets
rladies_tweets %>%
  janitor::tabyl(retweet_count) %>%
  arrange(desc(retweet_count))

max(rladies_tweets$retweet_count)

which(rladies_tweets$retweet_count==26)
# [1] 3 first tweet
which(rladies_tweets$retweet_count==12)
# [1] 441 thank you and hannah's game tweet
which(rladies_tweets$retweet_count==11)
# [1] 7 maelle
which(rladies_tweets$retweet_count==9)
# [1]  56 442 game announcement / follow us tweet
which(rladies_tweets$retweet_count==7)
# [1] 12 16 gabriela / hazel
which(rladies_tweets$retweet_count==6)
# [1] 4 114 presenting the twitter account / SHIRIN GLANDER
which(rladies_tweets$retweet_count==5)
# [1] 17  23 152 160 327 erin / alice daish / paula moraga / MA SILVIA GUTIÉRREZ / LORNA MARIA AINE
which(rladies_tweets$retweet_count==4)
# [1] 8   9 217 238 348 amelia / mine c / kelly o'brien / CHELSEA MORRIS / Bea
which(rladies_tweets$retweet_count==3)
# [1] 6  13  14  22  24  30  40  69 169 188 277 321
which(rladies_tweets$retweet_count==2)
# [1]  15  20  25  36  44  73  78 125 143 149 159 172 197 220 258 289
# [17] 301 308 396 406 427
which(rladies_tweets$retweet_count==1)
# [1]  21  26  27  28  29  37  59  60  61  92 102 108 127 128 129 130
# [17] 131 132 133 134 135 136 137 138 139 140 141 142 156 162 163 165
# [33] 173 176 177 178 179 181 182 183 189 193 196 199 205 207 208 216
# [49] 218 229 241 242 243 262 274 292 295 324 336 361 389 392 400 401
# [65] 403 404 430 432

as.list(rladies_tweets[3 ,c("retweet_count", "text", "status_id")])

rladies_tweets <- arrange(rladies_tweets, desc(retweet_count))
head(rladies_tweets[,"retweet_count"], 100)
as.list(rladies_tweets[,"retweet_count"])

id_vector <- rladies_tweets %>% 
  filter(retweet_count > 0) %>%
  pull(status_id)
  

as.list(rladies_tweets[3 ,c("retweet_count", "text", "status_id")])

#----

tweet_url <- "https://twitter.com/rladies_iwd2018/status/971140001495908353"
id <- gsub(".*status/", "", tweet_url)

tweets <- search_tweets(q = "@rladies_iwd2018 OR to:rladies_iwd2018 OR rladies_iwd2018",
                                sinceId = id,
                                n = 2000, 
                                include_rts = FALSE)

tweets <- tweets %>%
  distinct()

tweets <- arrange(tweets, desc(created_at))
head(tweets[,"created_at"])

# if (!file.exists("tweets.rda")) {
#   file.create("tweets.rda")
# }
save(tweets, file = "tweets.rda")

load("tweets.rda")

which(!tweets$text %in% rladies_tweets$text) 

tweets$text[1] == rladies_tweets$text[1]

# @LucyStats: I did some adventurous iterating to grab all the correct tweets by essentially grabbing all ids that replied to the original tweet, then all ids that replied to those replies, and so on, subsetting to only tweets involved in this particular tree.

# Lucy's code - modified

# get tweets status_id s

# cbind(tweets$status_id, tweets$reply_to_status_id)
# tweets_subset <- subset(tweets, !is.na(tweets$reply_to_status_id))
# cbind(tweets_subset$status_id, tweets_subset$reply_to_status_id)
# unique(tweets_subset$reply_to_status_id)
# length(tweets_subset$reply_to_status_id)
# table(tweets_subset$reply_to_status_id)
# max(table(tweets_subset$reply_to_status_id))
# which(table(tweets_subset$reply_to_status_id) > 6)


# table(tweets_subset$reply_to_status_id)

tweets %>% 
  janitor::tabyl(reply_to_status_id) %>%
  arrange(desc(n)) %>%
  head(10)

names(tweets)

tweets %>% 
  janitor::tabyl(user_id) %>%
  arrange(desc(n)) %>%
  head(10)

as_screenname("957247055898046464")
            

max(table(tweets$reply_to_status_id))
which(table(tweets$reply_to_status_id) > 6)
  indexes <- which(
  tweets_subset$reply_to_status_id == "971560433835565057")

indexes <- which(
  tweets$reply_to_status_id == "971560433835565057")

tweets$reply_to_status_id[indexes]

cbind(tweets$status_id[indexes], 
      tweets$reply_to_status_id[indexes])

tweets$status_id[indexes][1]

# Grab the tweets
# id <- tweets$status_id[indexes][1]

id <- "971560433835565057"
diff <- 1
while (diff != 0) {
  id_next <- tweets %>%
    filter(reply_to_status_id %in% id) %>%
    pull(status_id)
  id_new <- unique(c(id, id_next))
  diff <- length(id_new) - length(id)
  id <- id_new
}

# diff <- 1
# while (diff != 0) {
#   id_next <- tweets %>%
#     filter(reply_to_status_id %in% id_vector) %>%
#     pull(status_id)
#   id_new <- unique(c(id_vector, id_next))
#   diff <- length(id_new) - length(id_vector)
#   id <- id_new
# }

all_replies <- tweets %>% 
  filter(reply_to_status_id %in% id)

# Pull the replyee and replier text

# from_text <- all_replies %>%
#   select(reply_to_status_id, screen_name, text) 
# %>%
#   # left_join(all_replies, c("reply_to_status_id" = "status_id")) %>%
#   left_join(all_replies, "reply_to_status_id") %>%
#   select(attributes(all_replies)$screen_name, attributes(all_replies)$text)

# Pull the replyee and replier text
from_text <- all_replies %>%
  select(reply_to_status_id) %>%
  left_join(all_replies, c("reply_to_status_id" = "status_id")) %>%
  select(screen_name, text)

to_text <- paste0(all_replies$screen_name, ": ", all_replies$text)
to_text <- gsub("'", "`", to_text)
from_text <- paste0(from_text$screen_name, ": ", from_text$text)
from_text <- gsub("'", "`", from_text)

# Set the text for the infamous tweet_0.
tweet_0 <- "@hspter: Do you like getting google calendar invites from your friends for lunches / coffees / etc.?"

# Create the edges
edges <- tibble::tibble(
  from = from_text,
  to = to_text
) %>%
  mutate(from = ifelse(
    from == "NA: NA",
    tweet_0,
    from)
  )

# Create the graph
# haven't run. Install graph packages first.
graph <- graph_from_data_frame(edges, directed = TRUE)
V(graph)$tooltip <- V(graph)$name

set.seed(8318)
p <- ggraph(graph, layout = "nicely") + 
  geom_edge_link() + 
  geom_point_interactive(aes(x, y, color = "red", alpha = 0.05, tooltip = tooltip)) +
  theme_void() + 
  theme(legend.position = "none")
ggiraph(code = print(p),
        width_svg = 10,
        zoom_max = 4)


# Maelle's

# When were tweets posted?
# rladies tweets
wday = as.factor(wday(created_at, 
                      label = TRUE))
rladies_tweets <- mutate(rladies_tweets, 
                         hour = as.factor(hour(created_at)))
rladies_tweets <- mutate(rladies_tweets, 
                         week = week(created_at))
rladies_tweets <- mutate(rladies_tweets, 
                         day = as.Date(created_at))

weekday_dat <- rladies_tweets %>%
  group_by(week, wday) %>%
  summarize(n = n(), created_at = created_at[1]) 

arrange(weekday_dat, created_at) %>%
  head() %>%
  knitr::kable()

# all tweets
tweets <- mutate(tweets, wday = as.factor(wday(created_at, label = TRUE)))
tweets <- mutate(tweets, hour = as.factor(hour(created_at)))
tweets <- mutate(tweets, week = week(created_at))
tweets <- mutate(tweets, day = as.Date(created_at))

weekday_dat <- tweets %>%
  group_by(week, wday) %>%
  summarize(n = n(), created_at = created_at[1]) 

arrange(weekday_dat, created_at) %>%
  head() %>%
  knitr::kable()

# How popular were the rladies_iwd2018 tweets?
# These plots don't really show what I'm looking for
plot1 <- ggplot(tweets) + 
  geom_histogram(aes(favorite_count))

plot2 <- ggplot(rladies_tweets) +
  geom_histogram(aes(favorite_count))

grid.arrange(plot1, plot2, ncol=2)

plot3 <- ggplot(rladies_tweets) +
  geom_histogram(aes(retweet_count))

plot4 <- ggplot(tweets) +
  geom_histogram(aes(retweet_count))

grid.arrange(plot3, plot4, ncol=2)

arrange(tweets, desc(retweet_count)) %>%
  head() %>%
  knitr::kable()

arrange(tweets, desc(favorite_count))

# Proportion of favorites
sum(rladies_tweets$favorite_count != 0)
sum(tweets$favorite_count != 0)

sum(rladies_tweets$favorite_count != 0)/length(rladies_tweets$favorite_count)
sum(tweets$favorite_count != 0)/length(tweets$favorite_count)

# Proportion of retweets
sum(rladies_tweets$retweet_count != 0)
sum(tweets$retweet_count != 0)

sum(rladies_tweets$retweet_count != 0)/length(rladies_tweets$retweet_count)
sum(tweets$retweet_count != 0)/length(tweets$retweet_count)

# Correlation between favorite and retweet
cor(rladies_tweets$favorite_count, rladies_tweets$retweet_count, method = "spearman")
cor(tweets$favorite_count, tweets$retweet_count, method = "spearman")

plot5 <- ggplot(rladies_tweets) +
  geom_point(aes(retweet_count, favorite_count))

plot6 <- ggplot(tweets) +
  geom_point(aes(retweet_count, favorite_count))

grid.arrange(plot5, plot6, ncol=2)

# geocode, country, etc
## rladies_tweets - nothing
index <- which(!is.na(rladies_tweets$country))
table(rladies_tweets$country[index])

as.data.frame(rladies_tweets[index,c("country", "screen_name")])


## tweets
index_country <- which(!is.na(tweets$country))

tweets %>%
  subset(!is.na(country)) %>%
  janitor::tabyl(country) %>%
  arrange(desc(country))

table(tweets$country[index_country])

as.data.frame(tweets[index,c("country", "screen_name")])
# Uruguay and Dani are winning here
# make map of this?

round(sum(!is.na(tweets$country))/nrow(tweets), 2)

# who won the game?
all_replies %>%
  arrange(desc(retweet_count)) %>%
  select(status_id, reply_to_status_id, reply_to_user_id, screen_name, retweet_count)

tweets %>%
  arrange(desc(retweet_count)) %>%
  select(status_id, reply_to_status_id, reply_to_user_id, screen_name, retweet_count)

rladies_tweets %>%
  arrange(desc(retweet_count)) %>%
  select(status_id, reply_to_status_id, reply_to_user_id, screen_name, retweet_count)

all_replies[,c("status_id", "reply_to_status_id", "reply_to_user_id", "screen_name", "retweet_count")]


index_userid <- which(!is.na(tweets$reply_to_user_id))

tweets_subset <- tweets[tweets$reply_to_user_id == "957247055898046464", c("screen_name", "status_id")]

unique(tweets_subset$screen_name)
sort(table(tweets_subset$screen_name), decreasing = TRUE)
tweets_subset[which(tweets_subset$screen_name == "_lacion_"),]
# Laura won the game!

# Conclusions so far
# the majority of tweets happened on 08.03.2018 - exactly as planned for iwd2018
# people tend to fav tweets, but not necessarily to retweet (correlation ~50%)
# the majority of the tweet location is Uruguay (but there are a lot of missing locations)
# Laura Ación @_lacion_ won the game!
