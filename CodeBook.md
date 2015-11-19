# Yelp Data Challenge Assignment 
## Code Book 

This file contains description of initial data set manipulations and final data set


### Initial (raw) Data Set Information

The initial data set is available here      https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/yelp_dataset_challenge_academic_dataset.zip .
Please read http://www.yelp.com/dataset_challenge for more information on initial data set.


### Data Set transformations

Initial data set was enriched to produce additional user features:
- number of friends (1st and 2nd connection levels)
- elite start year and elite status "age" (to be used for class definitions only)
- yelping start year and "age"
- total tip count and relevant tip likes count per user
- count of unique business types for which user generated reviews and tips
- count of unique cities/states/countries that host these businesses
- count of reviews by rating (though limited by the amount of data made available by Yelp)
- intensity and uniformity of review and tip creation in recent period of time
- lenth of reviews and tips
- count of top-20 words (stems) used in overall corpuses of reviews and tips
- data on review votes by vote type
 

### Processed Data Set description

You will find the resulted data set at https://yadi.sk/d/qdsL718tkaey2

Below is description of the variables:

1. user_id

     Unique user identifier (character)

2. yelping_since

     Date of first user experience in the format  “YYYY-MM” (character)

3. review_count

     Total number of reviews composed by the user since start (integer, non-negative)

4. name

     Name of the user (character)

5. friends

     user ids of 1-st level friends (list of character)

6. fans

     Total number of user’s fans (integer, non-negative)

7. average_stars

     Average rating across all reviews since start (numeric, from 0 to 5)

8. type

     Data base label (character, “user”)

9. elite

     Number of years with elite status (integer, non-negative)

10. votes.[type]

     type = funny, useful, cool
     Total number of votes (of each type) given by Yelp members since start (integer, non-negative)

11. compliments.[type]

     type = profile, cute, funny, plain, writer, note, photos, hot, cool, more, list
     Total number of compliments (of each type) given by Yelp members since start (integer, non-negative)

12. FriendsNum1, FriendsNum2

     Total number of friends of 1-st and 2-nd levels (integer, non-negative)

13. Total_Tips

     Total number of tips created by user since start (limited to tips data set provided by Yelp) (integer, non-negative)

14. Total_Tip_Likes

     Total number of likes received by user for tips since start (limited to tips data set provided by Yelp) (integer, non-negative)

15. elite_start_year

     Year of user elite status admission (character)

16. elite_age

     Number of years from start to elite status submission (numeric, non-negative)

17. yelp_start_year

     Year of user start Yelping experience (character)

18. yelp_age

     Number of days since start till Jan 30 2015 (integer, non-negative)

19. elite_diff_since_start

     Number of years from start to elite status submission (numeric, non-negative)

20. bus_types_reviewed_count

     Total number of types of businesses (per Yelp classification) reviewed by user since start (limited to tips data set provided by Yelp) (integer, non-negative)

21. bus_types_tips_count

     Total number of types of businesses (per Yelp classification) commented in tips by user since start (limited to tips data set provided by Yelp) (integer, non-negative)

22. cities_commented_count

     Total number of location (cities) of businesses reviewed by user since start (limited to tips data set provided by Yelp) (integer, non-negative)

23. states_commented_count

     Total number of location (states) of businesses reviewed by user since start (limited to tips data set provided by Yelp) (integer, non-negative)

24. countries_commented_count

     Total number of location (countries) of businesses reviewed by user since start (limited to tips data set provided by Yelp) (integer, non-negative)

25. star[X]

     X = 1..5
     Total number of reviews with rating X written by user since start (limited to tips data set provided by Yelp) (integer, non-negative)

26. reviews_[xxx]_length

     xxx = min, average, median, max
     Metric xxx of length of the all reviews written by user since start (limited to tips data set provided by Yelp) (numeric, non-negative)

27. tips_[xxx]_length

     xxx = min, average, median, max
     Metric xxx of length of the all tips written by user since start (limited to tips data set provided by Yelp) (numeric, non-negative)

28. tips_[xxx]_likes

     xxx = min, average, median, max
     Metric xxx of number of likes per tip (limited to tips data set provided by Yelp) (numeric, non-negative)

29. reviews_[xxx]_votes.[type]

     xxx = min, average, median, max
     type = funny, useful, cool
     Metric xxx of number of votes of certain type per review (limited to tips data set provided by Yelp) (numeric, non-negative)

30. reviews_frequency_av[X]

     X = 3, 6, 12, 24, 26
     Average number (per month) of reviews written by user in last X months (limited to tips data set provided by Yelp) (numeric, non-negative)

31. reviews_frequency_consist[X]

     X = 3, 6, 12, 24, 26
     Number of months in last X months in which user has posted at least one review (limited to tips data set provided by Yelp) (numeric, non-negative)

32. tips_frequency_av[X]

     X = 3, 6, 12, 24, 26
     Average number (per month) of tips written by user in last X months (limited to tips data set provided by Yelp) (numeric, non-negative)

33. tips_frequency_consist[X]

     X = 3, 6, 12, 24, 26
     Number of months in last X months in which user has posted at least one tip (limited to tips data set provided by Yelp) (numeric, non-negative)

34. rev_t_[stem]

     stem = also, back, can, food, friend, get, good, great, just, like, love, nice, one, other, place, realli, servic, time, tri, veri
     total count of [stem] in the text of all reviews written by user since start (limited to tips data set provided by Yelp) (integer, non-negative)

35. tip_t_[stem]

     stem = best, can, day, dont, food, free, get, good, great, happi, hour, just, like, love, lunch, one, place, servic, time, tri
     total count of [stem] in the text of all tips written by user since start (limited to tips data set provided by Yelp) (integer, non-negative)

36. user_class

     User status as of today (character, “elite” and “non elite”)
