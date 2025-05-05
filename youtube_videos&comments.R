## YOUTUBE 

#### PART 1 : getting the videos ####

library(httr)
library(dplyr)

api_key <- "AIzaSyDSDxqYSZKyWcuSDLTg1XMdQCcqaVWHo4s"
url <- "https://www.googleapis.com/youtube/v3/search"
all_videos <- data.frame()
next_page_token <- NULL

repeat {
  response <- GET(
    url,
    query = list(
      part = "snippet",
      q = "femminicidio",
      type = "video",
      maxResults = 50,
      pageToken = next_page_token,
      key = api_key,
      regionCode = "IT"  # 🇮🇹 Focus on Italian region
    )
  )
  
  content <- content(response, "parsed")
  
  # Extract video data + channel info
  current_batch <- data.frame(
    video_id = sapply(content$items, function(x) x$id$videoId),
    title = sapply(content$items, function(x) x$snippet$title),
    published_at = sapply(content$items, function(x) x$snippet$publishedAt),
    channel_title = sapply(content$items, function(x) x$snippet$channelTitle),
    channel_id = sapply(content$items, function(x) x$snippet$channelId),
    stringsAsFactors = FALSE
  )
  
  all_videos <- bind_rows(all_videos, current_batch)
  
  # Pagination
  next_page_token <- content$nextPageToken
  if (is.null(next_page_token)) break
  
  Sys.sleep(1)  # Avoid rate limits
}

# Remove duplicates
all_videos <- distinct(all_videos, video_id, .keep_all = TRUE)

# Save results
write.csv(all_videos, "youtube_femminicidio_italian_channels.csv", row.names = FALSE)

# -------------------------------------------------------------------
#### PART 2 : getting the comments ####

library(httr)
library(dplyr)

get_video_comments <- function(video_id, api_key, max_results = 100) {
  url <- "https://www.googleapis.com/youtube/v3/commentThreads"
  res <- GET(
    url,
    query = list(
      part = "snippet",
      videoId = video_id,
      maxResults = max_results,
      textFormat = "plainText",
      key = api_key
    )
  )
  content <- content(res, "parsed")
  if (is.null(content$items)) return(NULL)
  tibble(
    video_id = video_id,
    comment = sapply(content$items, function(x) x$snippet$topLevelComment$snippet$textDisplay),
    author = sapply(content$items, function(x) x$snippet$topLevelComment$snippet$authorDisplayName),
    published_at = sapply(content$items, function(x) x$snippet$topLevelComment$snippet$publishedAt)
  )
}

# ---- Fetch Comments for All Videos
library(purrr)
all_comments <- data.frame()

for (id in all_videos$video_id) {
  cat("Fetching comments for video:", id, "\n")
  
  next_page_token <- NULL
  
  repeat {
    res <- GET(
      "https://www.googleapis.com/youtube/v3/commentThreads",
      query = list(
        part = "snippet",
        videoId = id,
        maxResults = 100,
        textFormat = "plainText",
        pageToken = next_page_token,
        key = api_key
      )
    )
    
    content <- content(res, "parsed")
    if (is.null(content$items)) break
    
    current_comments <- tibble(
      video_id = id,
      comment = map_chr(content$items, ~ .x$snippet$topLevelComment$snippet$textDisplay %||% ""),
      author = map_chr(content$items, ~ .x$snippet$topLevelComment$snippet$authorDisplayName %||% ""),
      published_at = map_chr(content$items, ~ .x$snippet$topLevelComment$snippet$publishedAt %||% "")
    )
    
    all_comments <- bind_rows(all_comments, current_comments)
    
    next_page_token <- content$nextPageToken
    if (is.null(next_page_token)) break
    
    Sys.sleep(1)  # To avoid hitting rate limits
  }
}

# ---- Save the comments
write.csv(all_comments, "youtube_femminicidio_comments.csv", row.names = FALSE)