#!/usr/bin/Rscript

twt_datetime_format <- '%a %b %d %H:%M:%S +0000 %Y'

twt_analysis_sum_stats <- list()

require(data.table)

setwd('/home/ubuntu/referendum_ita_2016/twitter')

# Load
load('data/geolocated_users.RData')

source('https://raw.githubusercontent.com/fraba/R_cheatsheet/master/database.R')
db <- '/home/ubuntu/referendum_ita_2016/twitter/data/referendum_16_twt_store.sqlite'
tweets <- data.table(sqliteGetTable(db, 'tweet'))
users <- data.table(sqliteGetTable(db, 'user'))
hashtags <- data.table(sqliteGetTable(db, 'hashtag'))
mentions <- data.table(sqliteGetTable(db, 'user_mention'))
hashtags <- data.table(sqliteGetTable(db, 'hashtag'))
urls <- data.table(sqliteGetTable(db, 'url'))

# Add coordinates
setkey(geolocated_users, 'id')
setkey(users, 'id')
users <- merge(users, geolocated_users, all.x = TRUE)

twt_analysis_sum_stats$n_tweets <- nrow(tweets)
twt_analysis_sum_stats$n_users <- nrow(users)
twt_analysis_sum_stats$n_geo_users <- sum(!is.na(users$lat))

# Count yes / no hashtags
twt_analysis_sum_stats$hashtags <- list()
twt_analysis_sum_stats$hashtags$yes <-  sum(grepl('si$', hashtags$text, ignore.case = T))
twt_analysis_sum_stats$hashtags$no <- sum(grepl('no$', hashtags$text, ignore.case = T))
twt_analysis_sum_stats$hashtags$yes_no <- 
  twt_analysis_sum_stats$hashtags$yes + twt_analysis_sum_stats$hashtags$no

# Assign yes / no to hashtags  and users
yes_no_dt <- hashtags[grepl('si$|no$',text,ignore.case=T),]
yes_no_dt$yes_or_no <- ifelse(grepl('si$', yes_no_dt$text, ignore.case=T),
                              'SI',
                              'NO')
setkey(yes_no_dt, 'tweet_id')
tweets$tweet_id <- tweets$id
tweets$tweet_id[tweets$retweeted_status_id != ''] <-
  tweets$retweeted_status_id[tweets$retweeted_status_id != ''] 
setkey(tweets, 'tweet_id')
yes_no_dt <- merge(yes_no_dt, tweets[,.(id, tweet_id, user_id, created_at)], all.x = T)

require(dplyr)
yes_no_by_user_dt <- 
  yes_no_dt %>%
  dplyr::group_by(user_id) %>%
  dplyr::summarize(vote_yes = sum(yes_or_no == 'SI') / n()) %>%
  data.table()

twt_analysis_sum_stats$users <- list()
twt_analysis_sum_stats$users$yes <- sum(yes_no_by_user_dt$vote_yes > 0.5)
twt_analysis_sum_stats$users$no <- sum(yes_no_by_user_dt$vote_yes < 0.5)
twt_analysis_sum_stats$users$yes_no <- 
  twt_analysis_sum_stats$users$yes + twt_analysis_sum_stats$users$no

# 
geolocated_users$user_id <- geolocated_users$id

setkey(geolocated_users, 'user_id')
setkey(yes_no_by_user_dt, 'user_id')

yes_no_by_geolocated_user_dt <- merge(yes_no_by_user_dt, 
                                      geolocated_users)
yes_no_by_geolocated_user_dt <- yes_no_by_geolocated_user_dt[!is.na(region),]

yes_no_by_region_dt <- 
  yes_no_by_geolocated_user_dt %>%
  dplyr::group_by(region) %>%
  dplyr::summarize(mean_yes = mean(vote_yes),
                   n_users = n())

#
users_by_region <- 
  geolocated_users %>% 
  dplyr::group_by(region) %>%
  dplyr::summarize(n_users = n()) %>%
  data.table()
users_by_region <- users_by_region[!is.na(region),]


# TS  
yes_no_dt$posix <- as.POSIXct(yes_no_dt$created_at, format = twt_datetime_format,
                              tz = 'UTC') 
attributes(yes_no_dt$posix)$tzone <- "Europe/Rome"
yes_no_dt$hour <- gsub(":\\d{2}:\\d{2}$", ":00:00", as.character(yes_no_dt$posix))


ts_hashtag_hour <- 
  yes_no_dt %>%
  dplyr::group_by(hour) %>% 
  dplyr::summarize(n_tweets = length(unique(id)),
                   n_users = length(unique(user_id)),
                   n_hashtags = n(),
                   yes_hashtags = sum(yes_or_no == 'SI'),
                   no_hashtags = sum(yes_or_no == 'NO'))

ts_user_hour <-
  yes_no_dt %>%
  dplyr::group_by(hour, user_id) %>% 
  dplyr::summarize(yes_ratio = sum(yes_or_no == 'SI') / n()) %>%
  dplyr::group_by(hour) %>%
  dplyr::summarize(yes_user = sum(yes_ratio > 0.5),
                   no_user = sum(yes_ratio < 0.5))

ts_hour <- merge(ts_hashtag_hour, ts_user_hour, by = 'hour')

# Geo 
require(sp)
load('data/Reg2011_WGS84_simp.RData')
italy_adm1_all_users <- sp::merge(italy_adm1, users_by_region, by.x = 'NOME', by.y = 'region', all.x = T)
italy_adm1_yes_no <- sp::merge(italy_adm1, as.data.frame(yes_no_by_region_dt), by.x = 'NOME', by.y = 'region', all.x = T)

# Add rolling means
library(zoo)
vars <- names(ts_hour)[2:ncol(ts_hour)]

ks <- c(1,2,4,8,16,32,64) * 24
ks <- ks[ks <= nrow(ts_hour)]

for(v in vars) {
  if (length(ks)>0) {
    for (win in ks) {
      ts_hour[[paste0(v, "_rollmean_", win, "d")]] <-  
        rollmean(ts_hour[[v]], win, fill = NA)
    } 
  }
}
ts_hour <- melt(ts_hour, id.vars = 'hour')

# Tops 
day_width <- 5

tweets$posix <- as.POSIXct(tweets$created_at, format = twt_datetime_format, tz = 'UTC')
last_tweets <- tweets[posix >= as.POSIXct(Sys.Date() - day_width, tz = 'AEDT') &
                        retweeted_status_id != "",]
last_tweets_yes <- last_tweets[user_id %in% yes_no_by_user_dt[vote_yes > 0.5,]$user_id,]
last_tweets_no <- last_tweets[user_id %in% yes_no_by_user_dt[vote_yes < 0.5,]$user_id,]

freq_retweeted_by_yes <- 
  table(last_tweets_yes$retweeted_status_id) %>%
  as.data.frame() %>%
  dplyr::arrange(desc(Freq))

freq_retweeted_by_no <- 
  table(last_tweets_no$retweeted_status_id) %>%
  as.data.frame() %>%
  dplyr::arrange(desc(Freq))

top_5_retweets_yes <- as.character(freq_retweeted_by_yes$Var1[1:5])
top_5_retweets_no <- as.character(freq_retweeted_by_no$Var1[1:5])

## Mentions
setkey(mentions, 'tweet_id')
setkey(tweets, 'tweet_id')
mentions <- merge(mentions, tweets[,.(tweet_id, user_id)], all.x=T)

freq_mentions_no <- 
  mentions[user_id.y %in% yes_no_by_user_dt[vote_yes < 0.5,]$user_id] %>%
  dplyr::group_by(name, screen_name, user_id.x) %>%
  dplyr::summarize(freq = n()) %>%
  dplyr::arrange(desc(freq))

freq_mentions_no <- freq_mentions_no[1:50,]

freq_mentions_yes <- 
  mentions[user_id.y %in% yes_no_by_user_dt[vote_yes > 0.5,]$user_id] %>%
  dplyr::group_by(name, screen_name, user_id.x) %>%
  dplyr::summarize(freq = n()) %>%
  dplyr::arrange(desc(freq))

freq_mentions_yes <- freq_mentions_yes[1:50,]

library(twitteR)
source('twt_local_info_not_for_git.R')
setup_twitter_oauth(twitter_auth_dict[['consumer_key']], twitter_auth_dict[['consumer_secret']],
                    twitter_auth_dict[['access_token']], twitter_auth_dict[['access_token_secret']])
# NO
users <- lookupUsers(users = freq_mentions_no$user_id.x)
users_df <- data.frame(user_id = sapply(users, function(x) get("id", x)),
                       image = sapply(users, function(x) get("profileImageUrl", x)),
                       stringsAsFactors = FALSE)
freq_mentions_no <- merge(freq_mentions_no, users_df, all.x = TRUE, by.x = 'user_id.x', by.y = 'user_id')
freq_mentions_no <- freq_mentions_no[order(freq_mentions_no$freq, decreasing = T),]

# YES
users <- lookupUsers(users = freq_mentions_yes$user_id.x)
users_df <- data.frame(user_id = sapply(users, function(x) get("id", x)),
                       image = sapply(users, function(x) get("profileImageUrl", x)),
                       stringsAsFactors = FALSE)
freq_mentions_yes <- merge(freq_mentions_yes, users_df, all.x = TRUE, by.x = 'user_id.x', by.y = 'user_id')
freq_mentions_yes <- freq_mentions_yes[order(freq_mentions_yes$freq, decreasing = T),]

## Hashtags
setkey(hashtags, 'tweet_id')
hashtags <- merge(hashtags, tweets[,.(tweet_id, user_id)], all.x=T)
hashtags$text <- tolower(hashtags$text) 

freq_hashtags_no <- 
  hashtags[user_id %in% yes_no_by_user_dt[vote_yes < 0.5,]$user_id] %>%
  dplyr::group_by(text) %>%
  dplyr::summarize(freq = n()) %>%
  dplyr::arrange(desc(freq))

freq_hashtags_no <- freq_hashtags_no[freq_hashtags_no$freq > 1, ]

freq_hashtags_yes <- 
  hashtags[user_id %in% yes_no_by_user_dt[vote_yes > 0.5,]$user_id] %>%
  dplyr::group_by(text) %>%
  dplyr::summarize(freq = n()) %>%
  dplyr::arrange(desc(freq))

freq_hashtags_yes <- freq_hashtags_yes[freq_hashtags_yes$freq > 1, ]


## Resources 
setkey(urls, 'tweet_id')
urls <- merge(urls, last_tweets[,.(tweet_id, user_id)], all.x=T)

parseDomain <- function(x) strsplit(gsub("http://|https://|www\\.", "", x), "/")[[c(1, 1)]]

urls$domain <- sapply(urls$expanded_url, parseDomain, USE.NAMES = F)

freq_resources_no <- 
  urls[user_id %in% yes_no_by_user_dt[vote_yes < 0.5,]$user_id] %>%
  dplyr::group_by(expanded_url, domain) %>%
  dplyr::summarize(freq = n()) %>%
  dplyr::arrange(desc(freq))

freq_resources_no <- freq_resources_no[freq_resources_no$freq > 1, ]
freq_resources_no <- freq_resources_no[order(freq_resources_no$freq, decreasing = T), ]

freq_urls_no <- 
  urls[user_id %in% yes_no_by_user_dt[vote_yes < 0.5,]$user_id] %>%
  dplyr::group_by(domain) %>%
  dplyr::summarize(freq = n()) %>%
  dplyr::arrange(desc(freq))

freq_urls_no <- freq_urls_no[freq_urls_no$freq > 1, ]

freq_urls_yes <- 
  urls[user_id %in% yes_no_by_user_dt[vote_yes > 0.5,]$user_id] %>%
  dplyr::group_by(domain) %>%
  dplyr::summarize(freq = n()) %>%
  dplyr::arrange(desc(freq))

freq_urls_yes <- freq_urls_yes[freq_urls_yes$freq > 1, ]

freq_resources_yes <- 
  urls[user_id %in% yes_no_by_user_dt[vote_yes > 0.5,]$user_id] %>%
  dplyr::group_by(expanded_url, domain) %>%
  dplyr::summarize(freq = n()) %>%
  dplyr::arrange(desc(freq))

freq_resources_yes <- freq_resources_yes[freq_resources_yes$freq > 1, ]
freq_resources_yes <- freq_resources_yes[order(freq_resources_yes$freq, decreasing = T), ]

# Limit to 1000 rows
first1kRows <- function(df) {
  if (nrow(df)>1000) return(df[1:1000,])
  else return(df)
}

freq_hashtags_no <- first1kRows(freq_hashtags_no)
freq_hashtags_yes <- first1kRows(freq_hashtags_yes)
freq_urls_no <- first1kRows(freq_urls_no)
freq_urls_yes <- first1kRows(freq_urls_yes)
freq_resources_no <- first1kRows(freq_resources_no) 
freq_resources_yes <- first1kRows(freq_resources_yes)

save(twt_analysis_sum_stats, yes_no_dt, yes_no_by_user_dt,
     yes_no_by_geolocated_user_dt, users_by_region, ts_hour,
     italy_adm1_all_users, italy_adm1_yes_no, 
     top_5_retweets_yes, top_5_retweets_no,
     freq_mentions_no, freq_mentions_yes,
     freq_hashtags_no, freq_hashtags_yes,
     freq_urls_no, freq_urls_yes, 
     freq_resources_no, freq_resources_yes, 
     file = "data/twt_analysis.RData")
