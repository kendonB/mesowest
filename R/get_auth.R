get_new_token <- function(api_key){
  base_url <- getOption("mesowest.base_url")
  out <- jsonlite::fromJSON(paste0(base_url, "auth?apikey=", api_key))
  out$TOKEN
}

get_existing_tokens <- function(api_key){
  base_url <- getOption("mesowest.base_url")
  out <- jsonlite::fromJSON(paste0(base_url, "auth?apikey=", api_key, "&list=1"))
  if(length(out$TOKENS) == 0){
    out$TOKENS <- character(0)
  }
  out$TOKENS
}

disable_tokens <- function(api_key, tokens){
  base_url <- getOption("mesowest.base_url")
  for(token_i in tokens){
    out <- jsonlite::fromJSON(paste0(base_url, "auth?apikey=", api_key, "&disableToken=", token_i))
  }
  new_tokens <- get_existing_tokens(api_key)
  if(length(intersect(tokens, new_tokens)) == 0){
    return(TRUE)
  } else {
    stop(paste0("Disabling tokens failed. API message is: ", out$MESSAGE))
  }
}
