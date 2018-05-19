#' progressively
#'
#' gives progressbar capabilities to purrr
#'
#' @export
progressively <- function (.f, .n, ...){
  pb <- progress::progress_bar$new(total = .n, ...)
  function(...) {
    pb$tick()
    .f(...)
  }
}

#' post_login
#'
#' login to account
#'
#' @export
post_login <- function(user, pw){
  base_url <- "https://login.xing.com/"
  remDr$navigate(base_url)
  remDr$findElement("id", "login_form_username")$sendKeysToElement(list(user))
  remDr$findElement("id", "login_form_password")$sendKeysToElement(list(pw))
  remDr$findElement("class name", "element-form-button-solid-lime")$clickElement()
}
