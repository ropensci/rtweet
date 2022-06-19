
#' @examples 
#' test_png <- "https://raw.githubusercontent.com/TimTeaFan/dynamicTwitterHeader/main/data/test.png"
#' test2_png <- "https://pbs.twimg.com/profile_banners/39387668/1524823160/1500x500"
#' profile_banner("test.png")
profile_banner <- function(banner_file, token = NULL) {
  token <- check_token(token)
  if (missing(banner_file)) {
    r <- TWIT_get(token, "/1.1/users/profile_banner", list(screen_name = "twitterapi"))
  } else {
    banner_uri <- base64enc::base64encode(banner_file)
    r <- TWIT_post(token,  "/1.1/account/update_profile_banner", params = list(banner = banner_uri))
    message("your profile banner image has been updated!")
  }
  r
}
