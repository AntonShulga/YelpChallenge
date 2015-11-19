require(jsonlite)
require(dplyr)
require(plyr)
require(reshape2)
require(doParallel)
require(lubridate)
require(caret)
require(tm)
require(kernlab)
require(randomForest)
require(gbm)
require(pls)
require(rpart)
require(nnet)
require(pROC)


Sys.setlocale("LC_ALL", "english")
registerDoParallel()
SAVE_FILES <- FALSE # set TRUE in case you want to save user data in PreProcess


ReadFromInitialSource <- function() {

   # The RDS files are produced with the stream_in functions below and then flatten
   # These files should be in the working directory for the YRead function to work
   # properly.
   
   review_data <<- stream_in(file("yelp_academic_dataset_review.json"))
   business_data <<- stream_in(file("yelp_academic_dataset_business.json"))
   tips_data <<- stream_in(file("yelp_academic_dataset_tip.json"))
   user_data <<- stream_in(file("yelp_academic_dataset_user.json"))
   checkin_data <<- stream_in(file("yelp_academic_dataset_checkin.json"))
   
}


YRead <- function(d_base, n_entries = 0, random = FALSE) {
   
   # YRead function reads the data from pre-saved files and returns n_entries 
   # of first entries or permuteted data set 
   
   data <- readRDS(paste0(d_base,"_data.RDS"))
   
   if (random == TRUE) {
      set.seed(112233)
      data <- data[sample(1:nrow(data), n_entries),]
   } else {
      if (n_entries > 0) data <- data[1:n_entries,] 
   }
   
   return(data)
   
}


GetBusinessTypes <- function(business_ids = NULL) {
   
   # This function derives the list of all types of businesses in the business
   # data set and returns the summaty table for all the types per state per 
   # city 
   
   if (is.null(business_ids)) {
      
      categ <- business_data$categories %>% unlist %>% unique
      business_data$Num <- 1
      sum_data <- NULL
      
      for (ind_cat in 1:length(categ)) {
         filterd_data <- business_data[sapply(business_data$categories, 
                                              function(x) any(x == categ[ind_cat])),]
         filterd_data <- aggregate(Num ~ state + city, data = filterd_data, sum)
         filterd_data$CatName <- categ[ind_cat]
         sum_data <- rbind(sum_data, filterd_data)
      }
      
      names(sum_data) <- c("State", "City", "NumBus", "CatName")
      
      return(sum_data[order(sum_data$NumBus, decreasing = TRUE), c(4,1:3)])
   
   } else {
      
      return (business_data %>%
         filter(business_id %in% business_ids) %>%
         .$categories %>%
         unlist %>%
         unique)
      
   }
}


GetBusinessAddress <- function(business_ids = NULL, type) {

   if (is.null(business_ids)) {
      
      return(NULL)
      
   } else {
      
      return(switch(type,
             city = business_data %>%
                 filter(business_id %in% business_ids) %>%
                 .$city %>%
                 unique,
             state = business_data %>%
                filter(business_id %in% business_ids) %>%
                .$state %>%
                unique,
             country = {
                states <- business_data %>%
                  filter(business_id %in% business_ids) %>%
                  .$state
                country_names <- NULL
                if ("BW" %in% states) country_names <- country_names <- c(country_names, "Germany")
                if ("EDH" %in% states) country_names <- country_names <- c(country_names, "UK")
                if (any(c("ON", "QC") %in% states)) country_names <- c(country_names, "Canada")
                if (!all(states %in% c("ON", "QC", "EDH", "BW"))) country_names <- c(country_names, "USA")
                country_names
             })
      )
      
   }
}





GetFriends <- function(user_id, friend_level = 1) {
   
      f_ids <- user_data[user_data$user_id == user_id, "friends"][[1]]
      
      if (friend_level > 1 & length(f_ids) > 0) {
         f_nextlevel <- NULL
         for (ind in 1:length(f_ids)) {
            f_nextlevel <- c(f_nextlevel, GetFriends(f_ids[ind], friend_level = friend_level-1))
         }
         f_ids <- unique(c(f_ids, f_nextlevel))
      }
      
      return(f_ids)

} 


NonEmptyMonthsCount <- function(dates, bin_dates, n_months) {
   Count <- 0
   for (ind in 1:n_months) {
      Count <- Count + as.integer(any(dates < bin_dates[ind] & dates >= bin_dates[ind+1]))
   }
   return(Count)
}


GetDTMTopTerms <- function(text_corpus, topN) {
   
   t_data <- Corpus(DataframeSource(data.frame(text_corpus)), 
                    readerControl = list(reader = readPlain,
                                         language = "english",
                                         load = TRUE))
   
   t_data <- tm_map(t_data, stripWhitespace)
   t_data <- tm_map(t_data, content_transformer(tolower))
   t_data <- tm_map(t_data, removeNumbers)
   t_data <- tm_map(t_data, removePunctuation)
   
   dtm <- DocumentTermMatrix(t_data, list(stemming = TRUE,
                                          stopwords = TRUE,
                                          bounds = list(global = c(100, Inf))))
                             
   dtm <- as.matrix(dtm)
   dtm <- dtm[,order(apply(dtm, 2, sum),
                    decreasing = TRUE)]
   dtm <- as.data.frame(dtm[,1:topN])
   
   return(names(dtm))
}

GetDTM <- function(text_corpus, top_terms) {
   
   t_data <- Corpus(DataframeSource(data.frame(text_corpus)), 
                    readerControl = list(reader = readPlain,
                                         language = "english",
                                         load = TRUE))
   
   t_data <- tm_map(t_data, stripWhitespace)
   t_data <- tm_map(t_data, content_transformer(tolower))
   t_data <- tm_map(t_data, removeNumbers)
   t_data <- tm_map(t_data, removePunctuation)
   
   dtm <- DocumentTermMatrix(t_data, list(stemming = TRUE,
                                          stopwords = TRUE,
                                          dictionary = top_terms))
   
   dtm <- as.matrix(dtm)
   dtm <- as.data.frame(dtm)
   
   return(dtm)
}


GetRatingsByUser <- function(rev_data) {
   
   return (rev_data %>%
      select(business_id, stars) %>%
      aggregate(data = ., stars ~ business_id, mean, na.rm = TRUE)
   )
}


ProcessUsers <- function(users) {

   ## derives number of friends of 1st and 2nd levels for each user
   ## very time consuming so I saved the file with results

   for (ind in users) {
      user_data$FriendsNum1[ind] <<- length(GetFriends(user_data$user_id[ind]))
      user_data$FriendsNum2[ind] <<- length(GetFriends(user_data$user_id[ind], friend_level = 2))
   }
   print("FriendsNum1 and FriendsNum2 - OK")
   if (SAVE_FILES == TRUE) saveRDS(user_data, file = "user_full_data.RDS")

   
   # >> elite_start_year and elite_age (in years)
   for (ind in users) {
      if (length((user_data$elite[ind][[1]]) > 0)) {
         user_data$elite_start_year[ind] <<- min(user_data$elite[ind][[1]])   
      } else user_data$elite_start_year[ind] <<- NA
   }
   user_data <<- mutate(user_data, elite_age = 2015 - elite_start_year + 1)
   print("elite_start_year and elite_age - OK")
   if (SAVE_FILES == TRUE) saveRDS(user_data, file = "user_full_data.RDS")

   
   # >> yelp_start_year
   user_data$yelp_start_year <<- user_data$yelping_since %>% 
      sapply(., function(x) substring(x, 1,4)) %>%
      as.integer
   print("yelp_start_year - OK")
   if (SAVE_FILES == TRUE) saveRDS(user_data, file = "user_full_data.RDS")
   
   
   # >> yelp_age (in days, approximation, latest being Jan 2015)
   s_date <- as.Date("2015-01-30", "%Y-%m-%d")
   user_data$yelp_age <<- user_data$yelping_since %>%
      sapply(., function(x) s_date - as.Date(paste0(x,"-15"), "%Y-%m-%d")) %>%
      as.integer
   print("yelp_age - OK")
   if (SAVE_FILES == TRUE) saveRDS(user_data, file = "user_full_data.RDS")
   
   
   # >> elite_diff_since_start (in years)
   user_data$elite_diff_since_start <<- with(user_data, elite_start_year - yelp_start_year) 
   print("elite_diff_since_start - OK")
   if (SAVE_FILES == TRUE) saveRDS(user_data, file = "user_full_data.RDS")
   
   
   # >> tip_count and tip_likes_total
   tip_by_users <- AggregateTips("user")
   user_data <<- merge(user_data, 
                       tip_by_users, 
                       by = "user_id", 
                       all = TRUE)
   print("tip_count and tip_likes_total - OK")
   if (SAVE_FILES == TRUE) saveRDS(user_data, file = "user_full_data.RDS")
   
   
   # >> bus_types_reviewed_count
   user_data$bus_types_reviewed_count <<- sapply(user_data$user_id, function(x) {
         review_data %>%
         filter(user_id == x) %>%
         .$business_id %>%
         GetBusinessTypes %>%
         length
   })
   print("bus_types_reviewed_count - OK")
   if (SAVE_FILES == TRUE) saveRDS(user_data, file = "user_full_data.RDS")
   
   
   # >> bus_types_tips_count
   user_data$bus_types_tips_count <<- sapply(user_data$user_id, function(x) {
      tip_data %>%
         filter(user_id == x) %>%
         .$business_id %>%
         GetBusinessTypes %>%
         length   
   })
   print("bus_types_tips_count - OK")
   if (SAVE_FILES == TRUE) saveRDS(user_data, file = "user_full_data.RDS")
   
   
   # >> cities_commented_count
   user_data$cities_commented_count <<- sapply(user_data$user_id, function(x) {
      review_data %>%
         filter(user_id == x) %>%
         .$business_id %>%
         GetBusinessAddress(., type = "city") %>%
         length   
   })
   print("cities_commented_count - OK")
   if (SAVE_FILES == TRUE) saveRDS(user_data, file = "user_full_data.RDS")
   
   
   # >> states_commented_count (countries apart from USA are accounted like 1 state)
   user_data$states_commented_count <<- sapply(user_data$user_id, function(x) {
      review_data %>%
         filter(user_id == x) %>%
         .$business_id %>%
         GetBusinessAddress(., type = "state") %>%
         length   
   })
   print("states_commented_count - OK")
   if (SAVE_FILES == TRUE) saveRDS(user_data, file = "user_full_data.RDS")
   
   
   # >> countries_commented_count
   user_data$countries_commented_count <<- sapply(user_data$user_id, function(x) {
      review_data %>%
         filter(user_id == x) %>%
         .$business_id %>%
         GetBusinessAddress(., type = "country") %>%
         length   
   })
   print("countries_commented_count - OK")
   if (SAVE_FILES == TRUE) saveRDS(user_data, file = "user_full_data.RDS")
   
   
   # >> sex? - derive by name? texts?
   
   # >> reviews_by_stars_count - DOES NOT MAKE SENCE AS THE REVIEW DATA APPEARS TO BE A RANDOM SUBSET OF ALL REVIEWS CREATED BY USERS
   rev_data <- select(review_data, user_id, stars)
   rev_data$Count <- 1
   rev_data <- dcast(rev_data, user_id ~ stars, sum, value.var = "Count")
   names(rev_data) <- c("user_id", "star1", "star2","star3","star4","star5")
   user_data <- merge(user_data, 
                  rev_data,
                  by = "user_id",
                  all = TRUE)
   print("reviews_by_stars_count - OK")
   if (SAVE_FILES == TRUE) saveRDS(user_data, file = "user_full_data.RDS")
   

   # >> reviews_frequency_avX (timing) 3, 6, 12, 24 and 36 months averages
   bin_dates <- c(3,6,12,24,36)*30 # assuming 30 days in a month (rounded)
   end_date <- as.Date(max(review_data$date))
   bin_dates <- end_date - bin_dates   
   
   rev_data <- review_data %>%
      select(user_id, date) %>%
      mutate(date = as.Date(date)) %>%
      filter(date >= bin_dates[1]) %>%
      group_by(user_id) %>%
      summarise(
         reviews_frequency_av3 = length(date)/3
      )
   user_data <- merge(user_data, 
                      rev_data,
                      by = "user_id",
                      all = TRUE)
   
   rev_data <- review_data %>%
      select(user_id, date) %>%
      mutate(date = as.Date(date)) %>%
      filter(date >= bin_dates[2]) %>%
      group_by(user_id) %>%
      summarise(
         reviews_frequency_av6 = length(date)/6
      )
   user_data <- merge(user_data, 
                      rev_data,
                      by = "user_id",
                      all = TRUE)
   
   rev_data <- review_data %>%
      select(user_id, date) %>%
      mutate(date = as.Date(date)) %>%
      filter(date >= bin_dates[3]) %>%
      group_by(user_id) %>%
      summarise(
         reviews_frequency_av12 = length(date)/12
      )
   user_data <- merge(user_data, 
                      rev_data,
                      by = "user_id",
                      all = TRUE)   
   
   rev_data <- review_data %>%
      select(user_id, date) %>%
      mutate(date = as.Date(date)) %>%
      filter(date >= bin_dates[4]) %>%
      group_by(user_id) %>%
      summarise(
         reviews_frequency_av24 = length(date)/24
      )
   user_data <- merge(user_data, 
                      rev_data,
                      by = "user_id",
                      all = TRUE)  
   
   rev_data <- review_data %>%
      select(user_id, date) %>%
      mutate(date = as.Date(date)) %>%
      filter(date >= bin_dates[5]) %>%
      group_by(user_id) %>%
      summarise(
         reviews_frequency_av36 = length(date)/36
      )
   user_data <- merge(user_data, 
                      rev_data,
                      by = "user_id",
                      all = TRUE)
   
   print("reviews_frequency_avX - OK")
   if (SAVE_FILES == TRUE) saveRDS(user_data, file = "user_full_data.RDS")
      
   
   # >> reviews_frequency_consistX (timing) number of non-empty months in last 3, 6, 12, 24 and 36 months
   bin_dates <- c(0:36)*30 # assuming 30 days in a month (rounded)
   end_date <- as.Date(max(review_data$date))
   bin_dates <- end_date - bin_dates 
   
   rev_data <- review_data %>%
      select(user_id, date) %>%
      mutate(date = as.Date(date)) %>%
      group_by(user_id) %>%
      summarise(
         reviews_frequency_consist3 = NonEmptyMonthsCount(date, bin_dates, 3),
         reviews_frequency_consist6 = NonEmptyMonthsCount(date, bin_dates, 6),
         reviews_frequency_consist12 = NonEmptyMonthsCount(date, bin_dates, 12),
         reviews_frequency_consist24 = NonEmptyMonthsCount(date, bin_dates, 24),
         reviews_frequency_consist36 = NonEmptyMonthsCount(date, bin_dates, 36)
      )
   
   user_data <- merge(user_data, 
                      rev_data,
                      by = "user_id",
                      all = TRUE)

   print("reviews_frequency_consistX - OK")
   if (SAVE_FILES == TRUE) saveRDS(user_data, file = "user_full_data.RDS")
   
   
   # >> tips_frequency_avX (timing) 3, 6, 12, 24 and 36 months averages
   bin_dates <- c(3,6,12,24,36)*30 # assuming 30 days in a month (rounded)
   end_date <- as.Date(max(review_data$date))
   bin_dates <- end_date - bin_dates   
   
   t_data <- tip_data %>%
      select(user_id, date) %>%
      mutate(date = as.Date(date)) %>%
      filter(date >= bin_dates[1]) %>%
      group_by(user_id) %>%
      summarise(
         tips_frequency_av3 = length(date)/3
      )
   user_data <- merge(user_data, 
                      t_data,
                      by = "user_id",
                      all = TRUE)
   
   t_data <- tip_data %>%
      select(user_id, date) %>%
      mutate(date = as.Date(date)) %>%
      filter(date >= bin_dates[2]) %>%
      group_by(user_id) %>%
      summarise(
         tips_frequency_av6 = length(date)/6
      )
   user_data <- merge(user_data, 
                      t_data,
                      by = "user_id",
                      all = TRUE)
   
   t_data <- tip_data %>%
      select(user_id, date) %>%
      mutate(date = as.Date(date)) %>%
      filter(date >= bin_dates[3]) %>%
      group_by(user_id) %>%
      summarise(
         tips_frequency_av12 = length(date)/12
      )
   user_data <- merge(user_data, 
                      t_data,
                      by = "user_id",
                      all = TRUE)   
   
   t_data <- tip_data %>%
      select(user_id, date) %>%
      mutate(date = as.Date(date)) %>%
      filter(date >= bin_dates[4]) %>%
      group_by(user_id) %>%
      summarise(
         tips_frequency_av24 = length(date)/24
      )
   user_data <- merge(user_data, 
                      t_data,
                      by = "user_id",
                      all = TRUE)  
   
   t_data <- tip_data %>%
      select(user_id, date) %>%
      mutate(date = as.Date(date)) %>%
      filter(date >= bin_dates[5]) %>%
      group_by(user_id) %>%
      summarise(
         tips_frequency_av36 = length(date)/36
      )
   user_data <- merge(user_data, 
                      t_data,
                      by = "user_id",
                      all = TRUE)
   
   print("tips_frequency_avX - OK")
   if (SAVE_FILES == TRUE) saveRDS(user_data, file = "user_full_data.RDS")

   
   # >> tips_frequency_consistX (timing) number of non-empty months in last 3, 6, 12, 24 and 36 months
   bin_dates <- c(0:36)*30 # assuming 30 days in a month (rounded)
   end_date <- as.Date(max(review_data$date))
   bin_dates <- end_date - bin_dates 
   
   t_data <- tip_data %>%
      select(user_id, date) %>%
      mutate(date = as.Date(date)) %>%
      group_by(user_id) %>%
      summarise(
         tips_frequency_consist3 = NonEmptyMonthsCount(date, bin_dates, 3),
         tips_frequency_consist6 = NonEmptyMonthsCount(date, bin_dates, 6),
         tips_frequency_consist12 = NonEmptyMonthsCount(date, bin_dates, 12),
         tips_frequency_consist24 = NonEmptyMonthsCount(date, bin_dates, 24),
         tips_frequency_consist36 = NonEmptyMonthsCount(date, bin_dates, 36)
      )
   
   user_data <- merge(user_data, 
                      t_data,
                      by = "user_id",
                      all = TRUE)
   
   print("tips_frequency_consistX - OK")
   if (SAVE_FILES == TRUE) saveRDS(user_data, file = "user_full_data.RDS")
      
   
   # >> reviews_min_length
   # >> reviews_average_length
   # >> reviews_median_length
   # >> reviews_max_length
   rev_data <- review_data %>%
      select(user_id, text) %>%
      mutate(len = nchar(text)) %>%
      group_by(user_id) %>%
      summarise(
         reviews_min_length = min(len, na.rm = TRUE),
         reviews_average_length = as.integer(round(mean(len, na.rm = TRUE))),
         reviews_median_length = as.integer(round(median(len, na.rm = TRUE))),
         reviews_max_length = max(len, na.rm = TRUE)
      )
   user_data <- merge(user_data, 
                  rev_data,
                  by = "user_id",
                  all = TRUE)
   print("reviews_min/max/av/med_length - OK")
   if (SAVE_FILES == TRUE) saveRDS(user_data, file = "user_full_data.RDS")
   
   
   # >> reviews_dtm top-20 word frequencies for users (for reviews only)
   elite_users <- user_data$user_id[!is.na(user_data$elite_start_year)]
   elite_reviews <- review_data$user_id %in% elite_users
   
   bus_english <- business_data$business_id[!(business_data$state %in% c("BW", "ON", "QC"))] # make sure we take english only
   english_reviews <- review_data$business_id %in% bus_english
   
   set.seed(23456)
   e_terms <- GetDTMTopTerms(sample(review_data[elite_reviews & english_reviews, "text"], 10000), 20)
   ne_terms <- GetDTMTopTerms(sample(review_data[(!elite_reviews) & english_reviews, "text"], 10000), 20)
   rev_terms <- unique(e_terms, ne_terms)
   rev_dtm <- GetDTM(review_data$text[1:500000], rev_terms)
   rev_dtm <- rbind(rev_dtm, GetDTM(review_data$text[500001:1000000], rev_terms))
   rev_dtm <- rbind(rev_dtm, GetDTM(review_data$text[1000001:1500000], rev_terms))
   rev_dtm <- rbind(rev_dtm, GetDTM(review_data$text[1500001:nrow(review_data)], rev_terms))
   names(rev_dtm) <- sapply(names(rev_dtm), function(x) paste0("rev_t_", x))
   rev_dtm <- cbind(review_data$user_id, rev_dtm)
   names(rev_dtm)[1] <- "user_id"
   rev_dtm <- aggregate(. ~ user_id, data = rev_dtm, sum, na.rm = TRUE)
   user_data <- merge(user_data, 
                      rev_dtm,
                      by = "user_id",
                      all = TRUE)
   print("reviews_dtm - OK")
   if (SAVE_FILES == TRUE) saveRDS(user_data, file = "user_full_data.RDS")
   
   
   # >> tips_dtm top-20 word frequencies for users (for tips only)
   elite_users <- user_data$user_id[!is.na(user_data$elite_start_year)]
   elite_tips <- tip_data$user_id %in% elite_users
   
   bus_english <- business_data$business_id[!(business_data$state %in% c("BW", "ON", "QC"))] # make sure we take english only
   english_tips <- tip_data$business_id %in% bus_english
   
   set.seed(23456)
   e_terms <- GetDTMTopTerms(sample(tip_data[elite_tips & english_tips, "text"], 10000), 20)
   ne_terms <- GetDTMTopTerms(sample(tip_data[(!elite_tips) & english_tips, "text"], 10000), 20)
   tip_terms <- unique(e_terms, ne_terms)
   tip_dtm <- GetDTM(tip_data$text, tip_terms)
   names(tip_dtm) <- sapply(names(tip_dtm), function(x) paste0("tip_t_", x))
   tip_dtm <- cbind(tip_data$user_id, tip_dtm)
   names(tip_dtm)[1] <- "user_id"
   tip_dtm <- aggregate(. ~ user_id, data = tip_dtm, sum, na.rm = TRUE)
   user_data <- merge(user_data, 
                      tip_dtm,
                      by = "user_id",
                      all = TRUE)
   print("tips_dtm - OK")
   if (SAVE_FILES == TRUE) saveRDS(user_data, file = "user_full_data.RDS")   
   
   
   # >> tips_min_length
   # >> tips_average_length
   # >> tips_median_length
   # >> tips_max_length
   t_data <- tip_data %>%
      select(user_id, text) %>%
      mutate(len = nchar(text)) %>%
      group_by(user_id) %>%
      summarise(
         tips_min_length = min(len, na.rm = TRUE),
         tips_average_length = as.integer(round(mean(len, na.rm = TRUE))),
         tips_median_length = as.integer(round(median(len, na.rm = TRUE))),
         tips_max_length = max(len, na.rm = TRUE)
      )
   user_data <- merge(user_data, 
                      t_data,
                      by = "user_id",
                      all = TRUE)
   print("tips_min/max/av/med_length - OK")
   if (SAVE_FILES == TRUE) saveRDS(user_data, file = "user_full_data.RDS")
   
   
   # >> tips_min_likes
   # >> tips_average_likes
   # >> tips_median_likes
   # >> tips_max_likes
   t_data <- tip_data %>%
      select(user_id, likes) %>%
      group_by(user_id) %>%
      summarise(
         tips_min_likes = min(likes, na.rm = TRUE),
         tips_average_likes = as.integer(round(mean(likes, na.rm = TRUE))),
         tips_median_likes = as.integer(round(median(likes, na.rm = TRUE))),
         tips_max_likes = max(likes, na.rm = TRUE)
      )
   user_data <- merge(user_data, 
                      t_data,
                      by = "user_id",
                      all = TRUE)
   print("tips_min/max/av/med_likes - OK")
   if (SAVE_FILES == TRUE) saveRDS(user_data, file = "user_full_data.RDS")   
   
   
   # SAME AS FOR REVIEWS_BY_STARS_COUNT - REVIEWS DATA SET IS NOT COMPLETE!
   # >> reviews_min_votes.TYPE - by type
   # >> reviews_average_votes.TYPE - by type
   # >> reviews_median_votes.TYPE - by type
   # >> reviews_max_votes.TYPE - by type
   # >> reviews_total_votes.TYPE - by type
   rev_data <- review_data %>%
      select(user_id, votes.funny, votes.useful, votes.cool) %>%
      group_by(user_id) %>%
      summarise(
         reviews_min_votes.funny = min(votes.funny, na.rm = TRUE),
         reviews_average_votes.funny = as.integer(round(mean(votes.funny, na.rm = TRUE))),
         reviews_median_votes.funny = as.integer(round(median(votes.funny, na.rm = TRUE))),
         reviews_max_votes.funny = max(votes.funny, na.rm = TRUE),
         reviews_total_votes.funny = sum(votes.funny, na.rm = TRUE),
         
         reviews_min_votes.useful = min(votes.useful, na.rm = TRUE),
         reviews_average_votes.useful = as.integer(round(mean(votes.useful, na.rm = TRUE))),
         reviews_median_votes.useful = as.integer(round(median(votes.useful, na.rm = TRUE))),
         reviews_max_votes.useful = max(votes.useful, na.rm = TRUE),
         reviews_total_votes.useful = sum(votes.useful, na.rm = TRUE),
         
         reviews_min_votes.cool = min(votes.cool, na.rm = TRUE),
         reviews_average_votes.cool = as.integer(round(mean(votes.cool, na.rm = TRUE))),
         reviews_median_votes.cool = as.integer(round(median(votes.cool, na.rm = TRUE))),
         reviews_max_votes.cool = max(votes.cool, na.rm = TRUE),
         reviews_total_votes.cool = sum(votes.cool, na.rm = TRUE)
      )
   user_data <- merge(user_data, 
                      rev_data,
                      by = "user_id",
                      all = TRUE)
   print("reviews_min/max/av/med_votes - OK")
   if (SAVE_FILES == TRUE) saveRDS(user_data, file = "user_full_data.RDS")
   
   user_data <- mutate(user_data, user_class = !is.na(elite_start_year))
   user_data$user_class <- as.factor(user_data$user_class)
   levels(user_data$user_class) <- c("non elite", "elite")
   if (SAVE_FILES == TRUE) saveRDS(user_data, file = "user_full_data.RDS")
      
   return(NULL)
   
}

ProcessCheckIns <- function() {
   
   Total_Checkins <- apply(checkin_data[,3:170], 1, function(x) sum(x, na.rm = TRUE))
   checkin_data <<- cbind(checkin_data, Total_Checkins)
   
   return(NULL)
}

ProcessBusiness <- function() {
   
   if (is.null(checkin_data$Total_Checkins)) ProcessCheckIns()
   business_data <<- merge(business_data, 
                           select(checkin_data, business_id, Total_Checkins), 
                           by = "business_id", 
                           all = TRUE)
   
   tip_by_business <- AggregateTips("business")
   business_data <<- merge(business_data, 
                           tip_by_business, 
                           by = "business_id", 
                           all = TRUE)
   
   
   return(NULL)
}

AggregateTips <- function(by) {
   
   tip_data$Num <- 1
   switch(by, 
      business = tip_by <- aggregate(cbind(Num, likes) ~ business_id, tip_data, sum),
      user = tip_by <- aggregate(cbind(Num, likes) ~ user_id, tip_data, sum)
   )
   names(tip_by) <- c(paste0(by,"_id"), "tip_count", "tip_likes_total")
   return(tip_by)
}


CheckInVsRaitings <- function() {
   
   if (is.null(business_data$Total_Checkins)) ProcessBusiness()
   
   bins <- c(0, 1:20*10, 3:10*100, 2:10*1000, max(business_data$Total_Checkins,
                                                  na.rm = TRUE))
   business_data$Num <- 1
   
   result <- data.frame()
   for (ind in 1:(length(bins)-1)) {
      temp <- filter(business_data, Total_Checkins > bins[ind] & Total_Checkins <= bins[ind+1])
      result <- rbind(result, as.integer(table(temp$stars)))
   }
   row.names(result) <- paste(as.character(bins[1:(length(bins)-1)]),"to",as.character(bins[2:length(bins)]))
   names(result) <- unique(business_data$stars)[order(unique(business_data$stars), decreasing = FALSE)]
   
   return(result)
}


ReadAll <- function() {
   
   user_data <<- YRead("user_full")  # saved results of ProcessUsers()

   
   business_data <<- YRead("business")
   review_data <<- YRead("review")
   checkin_data <<- YRead("checkin")
   tip_data <<- YRead("tips")
   
   res_CvsR <- CheckInVsRaitings()
   with(melt(res_CvsR), plot(variable, value))
   
   
   # elite are closer in their ratings to total raiting of the business?
   # elite are different in behaviour patterns?
   # should we try find only "fresh" elite users? are they different to older elite users?
   

     
}

WhyWeNeedThisAnalysis <- function() {
   
   # discover whether elite ratings are closer to final in comparison to non elite    
   
   review_data <- YRead("review")
   business_data <- YRead("business")
   
   elite_ratings <- GetRatingsByUser(
      filter(review_data, 
             user_id %in% user_data$user_id[user_data$user_class == "elite"]))
   names(elite_ratings)[2] <- "stars_elite"
   
   non_elite_ratings <- GetRatingsByUser(
      filter(review_data, 
             user_id %in% user_data$user_id[user_data$user_class == "non elite"]))
   names(non_elite_ratings)[2] <- "stars_non_elite"
   
   all_ratings <- merge(elite_ratings,
                        non_elite_ratings,
                        by = "business_id",
                        all = FALSE)
   
   all_ratings <- merge(all_ratings,
                        select(business_data, business_id, stars),
                        by = "business_id",
                        all = FALSE)
   
   all_ratings <- mutate(all_ratings,
                         dif_elite = stars_elite - stars,
                         dif_non_elite = stars_non_elite - stars)
   
   return(all_ratings)
   
}

ShowClassDifferences <- function() {
   
   # calculating differnces of major user features by type
   sum_user_data <- user_data %>%
      select(user_class, FriendsNum1, fans, review_count, Total_Tips, votes.funny, 
             votes.useful, votes.cool, compliments.cool, Total_Tip_Likes, 
             reviews_average_length, reviews_frequency_av36, 
             reviews_frequency_consist36, bus_types_reviewed_count) %>%
      mutate(Total_Votes = votes.funny + votes.useful + votes.cool, 
             votes.funny = NULL, votes.useful = NULL, votes.cool = NULL) %>%
      group_by(user_class) %>%
      aggregate(data = ., . ~ user_class, mean, na.rm = TRUE)
   sum_user_data <- as.data.frame(t(sum_user_data))
   names(sum_user_data) <- c("elite", "non_elite")
   sum_user_data <- sum_user_data[-1,] 
   
   return(sum_user_data)
   
}

PlotFeatures <- function(user_data) {
   # plotting the factors for a random subset of data points
   set.seed(23456)
   u_data <- user_data[sample(1:nrow(user_data), 25000),]
   png(filename = "features.png", width = 3000, height = 2835)
   featurePlot(u_data[,1:120], 
               u_data$user_class,
               plot = "box",
               scales = list(y = list(relation="free"),
                             x = list(rot = 90)),
               between = list(x=1, y=1))
   dev.off()
}

PreProcessUserData <- function(user_data) {
   
   # setting NAs to 0
   for (ind in 1:ncol(user_data)) 
      user_data[is.na(user_data[,ind]), ind] <- 0
   
   # near zero varience features elimination
   set.seed(23456)
   u_data <- user_data[sample(1:nrow(user_data), 25000),]
   n_elite <- nearZeroVar(u_data[u_data$user_class == "elite",1:120])
   n_non_elite <- nearZeroVar(u_data[u_data$user_class == "non elite",1:120])
   nzv_eliminate <- intersect(n_elite, n_non_elite)
   # names(user_data)[nzv_eliminate]
   user_data <- user_data[, -nzv_eliminate]
   # names(user_data)

   # look for correlations between factors 
   high_corr <- findCorrelation(cor(u_data[,1:(ncol(u_data)-1)]))
   user_data <- user_data[,-high_corr]
   
   return(user_data)
}

GetModelResults <- function(user_data, train_size, useMetric = "Accuracy") {
   
   # build train and test sets
   set.seed(23456)
   number_of_methods <- 8
   resSummary <- data.frame(Method = rep("None",number_of_methods),
                            Accuracy = numeric(number_of_methods), 
                            Sensitivity = numeric(number_of_methods))
   resSummary$Method <- as.character(resSummary$Method)
   factorNames <- names(user_data)[1:(ncol(user_data)-1)]
   methodInd <- 1
   
   
   train_set <- user_data[sample(1:nrow(user_data), train_size*2),]
   test_set <- train_set[1:train_size,]
   train_set <- train_set[(train_size+1):nrow(train_set),]
#    summary(train_set$user_class)/nrow(train_set)*100 # check that class distribution is ok
#    summary(test_set$user_class)/nrow(test_set)*100 # check that class distribution is ok
   
   # build logistic regresstion model
   set.seed(23456)
   useTrControl <- trainControl(method = "cv", number = 10) 
   logRegModel <- train(user_class ~ .,
                        data = train_set,
                        method = "glm",
                        preProc = c("center", "scale", "BoxCox", "pca"),
                        trControl = useTrControl,
                        metric = useMetric)
   # plot(logRegModel)
   
   logRegPredictions <- predict(logRegModel, test_set)
   matr <- confusionMatrix(logRegPredictions, test_set$user_class)
   resSummary$Method[methodInd] <- "Logistic Regression"
   resSummary$Accuracy[methodInd] <- matr$overall[[1]]
   resSummary$Sensitivity[methodInd] <- matr$byClass[[1]]
   impMatrix <- as.data.frame(matrix(rep(NA, length(factorNames)), nrow = 1))
   names(impMatrix) <- factorNames[order(factorNames)]
   methodInd <- methodInd + 1
   
   
   # build SVM model
   set.seed(23456)
   SVMModel <- train(user_class ~ .,
                     data = train_set,
                     method = "svmRadial",
                     preProc = c("center", "scale"),
                     tuneLength = 10,
                     trControl = useTrControl,
                     metric = useMetric)
   # plot(SVMModel)
   
   SVMPredictions <- predict(SVMModel, test_set)
   matr <- confusionMatrix(SVMPredictions, test_set$user_class)
   resSummary$Method[methodInd] <- "Support Vector Machines"
   resSummary$Accuracy[methodInd] <- matr$overall[[1]]
   resSummary$Sensitivity[methodInd] <- matr$byClass[[1]]
   vi <- varImp(SVMModel)
   impMatrix <- rbind(impMatrix, 
                      t(vi$importance)[1,order(row.names(vi$importance))])
   methodInd <- methodInd + 1   
   
   # build partial least squares model
   set.seed(23456)
   PLSModel <- train(user_class ~ .,
                     data = train_set,
                     method = "pls",
                     preProc = c("center", "scale", "BoxCox"),
                     tuneLength = 20,
                     trControl = useTrControl,
                     metric = useMetric)
   # plot(PLSModel)
   
   PLSPredictions <- predict(PLSModel, test_set)
   matr <- confusionMatrix(PLSPredictions, test_set$user_class)   
   resSummary$Method[methodInd] <- "Partial Least Squares"
   resSummary$Accuracy[methodInd] <- matr$overall[[1]]
   resSummary$Sensitivity[methodInd] <- matr$byClass[[1]]
   vi <- varImp(PLSModel)
   impMatrix <- rbind(impMatrix, 
                      t(vi$importance)[1,order(row.names(vi$importance))])
   methodInd <- methodInd + 1   
   
   # build neural networks model 
   set.seed(23456)
   nnetGrid <- expand.grid(decay = c(0, 0.01, 0.1),
                           size = c(1:5)*2,
                           bag = FALSE)
   nnetModel <- train(user_class ~ .,
                      data = train_set,
                      method = "avNNet",
                      preProc = c("center", "scale"),
                      tuneGrid = nnetGrid,
                      trControl = useTrControl,
                      linout = TRUE,
                      trace = FALSE,
                      MaxNWts = 10 * (ncol(train_set) + 1) + 10 +1,
                      maxit = 500,
                      metric = useMetric)
   # plot(nnetModel)
   
   nnetPredictions <- predict(nnetModel, test_set)
   matr <- confusionMatrix(nnetPredictions, test_set$user_class)  
   resSummary$Method[methodInd] <- "Neural Network"
   resSummary$Accuracy[methodInd] <- matr$overall[[1]]
   resSummary$Sensitivity[methodInd] <- matr$byClass[[1]]
   vi <- varImp(nnetModel)
   impMatrix <- rbind(impMatrix, 
                      t(vi$importance)[1,order(row.names(vi$importance))])
   methodInd <- methodInd + 1   
   
   # build K-Nearest neighbors model 
   set.seed(23456)
   knnGrid <- data.frame(k = (1:10)*2)
   knnModel <- train(user_class ~ .,
                     data = train_set,
                     method = "knn",
                     preProc = c("center", "scale"),
                     tuneGrid = knnGrid,
                     trControl = useTrControl,
                     metric = useMetric)
   # plot(knnModel)
   
   knnPredictions <- predict(knnModel, test_set)
   matr <- confusionMatrix(knnPredictions, test_set$user_class)   
   resSummary$Method[methodInd] <- "K-Nearest Neighbors"
   resSummary$Accuracy[methodInd] <- matr$overall[[1]]
   resSummary$Sensitivity[methodInd] <- matr$byClass[[1]]
   vi <- varImp(knnModel)
   impMatrix <- rbind(impMatrix, 
                      t(vi$importance)[1,order(row.names(vi$importance))])
   methodInd <- methodInd + 1   
   
   # build decision tree model 
   set.seed(23456)
   treeModel <- train(user_class ~ .,
                      data = train_set,
                      method = "rpart2",
                      tuneLength = 8,
                      trControl = useTrControl,
                      metric = useMetric)
   # plot(treeModel)
   
   treePredictions <- predict(treeModel, test_set)
   matr <- confusionMatrix(treePredictions, test_set$user_class)  
   resSummary$Method[methodInd] <- "Decision Tree"
   resSummary$Accuracy[methodInd] <- matr$overall[[1]]
   resSummary$Sensitivity[methodInd] <- matr$byClass[[1]]
   vi <- varImp(treeModel)
   impMatrix <- rbind(impMatrix, 
                      t(vi$importance)[1,order(row.names(vi$importance))])
   methodInd <- methodInd + 1   
   
   # build random forest model 
   set.seed(23456)
   rfGrid <- expand.grid(mtry = c(1,3,6,9,12))
   rfModel <- train(user_class ~ .,
                    data = train_set,
                    method = "rf",
                    tuneGrid = rfGrid,
                    ntree = 100,
                    trControl = useTrControl,
                    metric = useMetric)
   # plot(rfModel)
   
   rfPredictions <- predict(rfModel, test_set)
   matr <- confusionMatrix(rfPredictions, test_set$user_class)   
   resSummary$Method[methodInd] <- "Random Forrest"
   resSummary$Accuracy[methodInd] <- matr$overall[[1]]
   resSummary$Sensitivity[methodInd] <- matr$byClass[[1]]
   vi <- varImp(rfModel)
   impMatrix <- rbind(impMatrix, 
                      t(vi$importance)[1,order(row.names(vi$importance))])
   methodInd <- methodInd + 1   
   
   # build boosted trees model 
   set.seed(23456)
   gbmGrid <- expand.grid(interaction.depth = c(1,3)*2, 
                          n.trees = c(1, 4, 8)*25, 
                          shrinkage = .1, 
                          n.minobsinnode = 10)
   gbmModel <- train(user_class ~ .,
                     data = train_set,
                     method = "gbm",
                     tuneGrid = gbmGrid,
                     trControl = useTrControl,
                     verbose = FALSE,
                     metric = useMetric)
   # plot(gbmModel)
   
   gbmPredictions <- predict(gbmModel, test_set)
   matr <- confusionMatrix(gbmPredictions, test_set$user_class)   
   resSummary$Method[methodInd] <- "Boosted Trees"
   resSummary$Accuracy[methodInd] <- matr$overall[[1]]
   resSummary$Sensitivity[methodInd] <- matr$byClass[[1]]
   vi <- varImp(gbmModel)
   impMatrix <- rbind(impMatrix, 
                      t(vi$importance)[1,order(row.names(vi$importance))])
   methodInd <- methodInd + 1
   
   # perform model comparison
   resamp <- resamples(list(Logistic = logRegModel, 
                            SVM = SVMModel,
                            PLS = PLSModel,
                            NNet = nnetModel,
                            KNN = knnModel,
                            DT = treeModel,
                            RF = rfModel,
                            BoostedTrees = gbmModel))
   #  print(parallelplot(resamp, metric = useMetric))
   
   return(list(size = train_size, 
               results = resSummary, 
               resamps = resamp,
               impData = impMatrix))
}

Reporting <- function() {
   
   user_data <- YRead("user_full")  # load saved results of ProcessUsers()
   user_data <- PreProcessUserData(user_data)
   
   forReport <- GetModelResults(3000)
   
   top <- apply(forReport$impData,2, mean, na.rm = TRUE)
   forReport$impData <- rbind(forReport$impData, top)
   topImp <- as.data.frame(
      t(forReport$impData[9,])[order(top, decreasing = TRUE),])
   
}   
   




   