
#' botcheck
#'
#' This function makes a call to the Botomter API and returns the probability that a specified Twitter user is a bot.
#' @param user The Twitter handle you want to check, e.g., "barackobama". The "@" is not required.
#' @keywords Botometer API wrapper
#' @export
#' @examples
#' botcheck("barackobama")


botcheck = function(user) {
  
  users_url = "https://api.twitter.com/1.1/users/show.json?screen_name="
  statuses_url = "https://api.twitter.com/1.1/statuses/user_timeline.json?screen_name="
  search_url = "https://api.twitter.com/1.1/search/tweets.json?q=%40"
  opts = "&count=200"
  
  # API call to get user
  userdata = GET(paste0(users_url,user,opts), sig)
  
  # API call to get tweets
  tweets = GET(paste0(statuses_url,user,opts), sig)
  
  # API call to get mentions
  mentions = GET(paste0(search_url,user,opts), sig)
  
  
  # Put everything in a list
  body = list(
    timeline = content(tweets, type="application/json"),
    mentions = content(mentions, type="application/json"),
    user = content(userdata, type="application/json")
  )
  
  # Convert to JSON
  body_json = RJSONIO::toJSON(body, auto_unbox = T, pretty = T)
  
  # Make the API request
  result = POST("https://osome-botometer.p.mashape.com/2/check_account",
                 encode="json",
                 add_headers(`X-Mashape-Key`=Mashape_key),
                 body=body_json)
  
  # Parse result
  result = content(result, as = "parsed")
  
  # Return "English" score
  return(result$display_scores$english)
}

