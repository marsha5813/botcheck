
#' botcheck
#' 
#' EDIT: 
#' 
#' This fork introduces three changes on botcheck 
#' 1 - Updated the botometer host to rapidAPI, as the current oficial version still tries to connect to Mashape.
#' 2 - Output is a list with all the indexes returned by botometer API.
#' I made (and propose) this change because I mostly extract twitter data from profiles in portuguese, thus
#' the scores output in english were useless for me.
#'
#' Cesar_Coelho, Aug 30, 2021
#'
#' This function makes a call to the Botomter API and returns a list of scores evaluating whether a specified Twitter user may be a bot.
#' @param user The Twitter handle you want to check, e.g., "barackobama". The "@" is not required.
#' @keywords Botometer API wrapper
#' @export
#' @examples
#' botcheck("barackobama")


if (!('botcheck' %in% installed.packages()[,'Package'])){install_github("marsha5813/botcheck")}; require(botcheck);     # get prob of bot account
if (!('httr' %in% installed.packages()[,'Package'])){install.packages("httr")}; require(httr);                          # for botcheck
if (!('xml2' %in% installed.packages()[,'Package'])){install.packages("xml2")}; require(xml2);                          # for botcheck
if (!('RJSONIO' %in% installed.packages()[,'Package'])){install.packages("RJSONIO")}; require(RJSONIO);                 # for botcheck


botcheck <- function(user, rapidapi_key){
  # rapidapi key is the access token to botometer API in the rapidAPI platform
  
  users_url <- "https://api.twitter.com/1.1/users/show.json?screen_name="
  statuses_url <- "https://api.twitter.com/1.1/statuses/user_timeline.json?screen_name="
  search_url <- "https://api.twitter.com/1.1/search/tweets.json?q=%40"
  rapidapi_url <- "https://botometer-pro.p.rapidapi.com/4/check_account"                                                # this line should be updated whenever the API updates
  opts = "&count=200"
  
  userdata = GET(paste0(users_url, user, opts), config = sig)
  tweets = GET(paste0(statuses_url, user, opts), config = sig)
  mentions = GET(paste0(search_url, user, opts), config = sig)
  
  body = list(timeline = content(tweets, type = "application/json"), 
              mentions = content(mentions, type = "application/json"), 
              user = content(userdata, type = "application/json"))
  body_json = RJSONIO::toJSON(body, auto_unbox = T, pretty = T)
  
  result = POST(rapidapi_url, 
                encode = "json", add_headers(`X-RapidAPI-Key` = rapidapi_key), 
                body = body_json)
  result = content(result, as = "parsed")
  
  return(result$display_scores)
}

