source("secrets.R")
library(httr2)
library(sodium)

reset_user_password <- function(username, new_password) {
  new_hash <- sodium::password_store(new_password)
  
  request(SUPABASE_URL) %>%
    req_url_path_append("rest/v1/users") %>%
    req_headers(
      "apikey" = SUPABASE_KEY,
      "Authorization" = paste("Bearer", SUPABASE_KEY),
      "Content-Type" = "application/json",
      "Prefer" = "return=minimal"
    ) %>%
    req_url_query(username = paste0("eq.", username)) %>%
    req_body_json(list(password_hash = new_hash)) %>%
    req_method("PATCH") %>%
    req_perform()
}

# to use this function:
# reset_user_password("username", "temporarypassword")