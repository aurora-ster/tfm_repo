## YOUTUBE (enriched info - da provare se funziona)

#### PART 1 : getting the videos ####

library(httr)
library(dplyr)
library(jsonlite)

# API setup
#usethis::edit_r_environ()
api_key <- Sys.getenv("api_key")

search_url <- "https://www.googleapis.com/youtube/v3/search"
videos_url <- "https://www.googleapis.com/youtube/v3/videos"

all_videos <- data.frame()
next_page_token <- NULL

extract_safe <- function(x, path) {
  val <- tryCatch(eval(parse(text = paste0("x$", path))), error = function(e) NA)
  if (is.null(val)) NA else val
}
get_tags <- function(x) {
  tg <- x$snippet$tags
  if (is.null(tg)) NA_character_ else paste(tg, collapse = ";")
}

repeat {
  response <- GET(
    search_url,
    query = list(
      part = "snippet",
      q = "femminicidio",
      type = "video",
      maxResults = 50,
      pageToken = next_page_token,
      key = api_key,
      regionCode = "IT"
    )
  )
  content <- content(response, "parsed")
  if (is.null(content$items)) break
  
  video_ids <- sapply(content$items, function(x) x$id$videoId)
  if (length(video_ids) == 0) break
  
  video_details <- GET(
    videos_url,
    query = list(
      part = "snippet,contentDetails,statistics",
      id = paste(video_ids, collapse = ","),
      key = api_key
    )
  )
  details_content <- content(video_details, "parsed")
  if (is.null(details_content$items) || length(details_content$items) == 0) next
  
  current_batch <- data.frame(
    video_id = sapply(details_content$items, function(x) extract_safe(x, "id")),
    title = sapply(details_content$items, function(x) extract_safe(x, "snippet$title")),
    description = sapply(details_content$items, function(x) extract_safe(x, "snippet$description")),
    tags = sapply(details_content$items, get_tags),
    published_at = sapply(details_content$items, function(x) extract_safe(x, "snippet$publishedAt")),
    channel_title = sapply(details_content$items, function(x) extract_safe(x, "snippet$channelTitle")),
    channel_id = sapply(details_content$items, function(x) extract_safe(x, "snippet$channelId")),
    category_id = sapply(details_content$items, function(x) extract_safe(x, "snippet$categoryId")),
    duration = sapply(details_content$items, function(x) extract_safe(x, "contentDetails$duration")),
    view_count = sapply(details_content$items, function(x) extract_safe(x, "statistics$viewCount")),
    like_count = sapply(details_content$items, function(x) extract_safe(x, "statistics$likeCount")),
    comment_count = sapply(details_content$items, function(x) extract_safe(x, "statistics$commentCount")),
    language = sapply(details_content$items, function(x) extract_safe(x, "snippet$defaultAudioLanguage")),
    stringsAsFactors = FALSE
  )
  
  if (nrow(current_batch) > 0) {
    all_videos <- bind_rows(all_videos, current_batch)
  }
  
  next_page_token <- content$nextPageToken
  if (is.null(next_page_token)) break
  Sys.sleep(1)
}


# Remove duplicates
all_videos <- distinct(all_videos, video_id, .keep_all = TRUE)

# Save results
write.csv(all_videos, "youtube_video.csv", row.names = FALSE)

# -------------------------------------------------------------------
#### PART 2 : getting the comments ####

library(httr)
library(dplyr)
library(purrr)

get_video_comments <- function(video_id, api_key, max_results = 100) {
  url <- "https://www.googleapis.com/youtube/v3/commentThreads"
  all_comments <- tibble()
  next_page_token <- NULL
  
  repeat {
    res <- GET(
      url,
      query = list(
        part = "snippet",
        videoId = video_id,
        maxResults = max_results,
        textFormat = "plainText",
        pageToken = next_page_token,
        key = api_key
      )
    )
    content <- content(res, "parsed")
    if (is.null(content$items)) break
    
    current_comments <- tibble(
      video_id = video_id,
      comment = purrr::map_chr(content$items, ~ .x$snippet$topLevelComment$snippet$textDisplay %||% ""),
      author = purrr::map_chr(content$items, ~ .x$snippet$topLevelComment$snippet$authorDisplayName %||% ""),
      author_channel_id = purrr::map_chr(content$items, ~ .x$snippet$topLevelComment$snippet$authorChannelId$value %||% NA_character_),
      published_at = purrr::map_chr(content$items, ~ .x$snippet$topLevelComment$snippet$publishedAt %||% ""),
      like_count = purrr::map_chr(content$items, ~ .x$snippet$topLevelComment$snippet$likeCount %||% NA_character_),
      reply_count = purrr::map_chr(content$items, ~ .x$snippet$totalReplyCount %||% NA_character_),
      comment_id = purrr::map_chr(content$items, ~ .x$snippet$topLevelComment$id %||% NA_character_),
    )
    all_comments <- bind_rows(all_comments, current_comments)
    
    next_page_token <- content$nextPageToken
    if (is.null(next_page_token)) break
    Sys.sleep(1)
  }
  return(all_comments)
}


# ---- Fetch Comments for All Videos
library(purrr)
all_comments <- data.frame()

for (id in all_videos$video_id) {
  cat("Fetching comments for video:", id, "\n")
  video_comments <- get_video_comments(id, api_key)
  if (!is.null(video_comments)) {
    all_comments <- bind_rows(all_comments, video_comments)
  }
}


# Remove duplicate comments (same video_id and comment text)
all_comments <- distinct(all_comments, video_id, comment, .keep_all = TRUE)

# ---- Save the comments
write.csv(all_comments, "youtube_comments.csv", row.names = FALSE)

