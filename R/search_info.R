#' create_query
#'
#' create vector of search pages
#'
#' @param keywords charcter vector
#' @param pages integer vector
#' @return queries
#'
#' @export
create_query <- function(keywords, pages){

  searc_base <- "https://www.xing.com/search/members?advanced_form=open&hrd=1&keywords="

  query <- paste(keywords, collapse = "+") %>%
    paste0(searc_base, ., "&page=", 1:pages, "&sc_o=da980_c")

  return(query)
}

#' get_users
#'
#' extract list of users from search request
#'
#' @param keywords charcter vector
#' @param pages integer vector
#' @return queries
#'
#' @export
get_users <- function(html){
  name <- html %>%
    #html_nodes(".cursor-pointer") %>%
    rvest::html_nodes("div.component-user-name.component-user-name-15 > a") %>%
    rvest::html_text()

  photo <- html %>%
    #html_nodes(".cursor-pointer") %>%
    rvest::html_nodes(".user-photo") %>%
    as.character() %>%
    stringr::str_extract("https?[:]//[[:graph:]]+") %>%
    stringr::str_replace('\"', "")

  name_id <- html %>%
    #html_nodes(".cursor-pointer") %>%
    rvest::html_nodes("div.component-user-name.component-user-name-15 > a") %>%
    as.character() %>%
    stringr::str_extract('/profile/.*?/') %>%
    stringr::str_replace_all("/profile/|/", "")

  premium <- html %>%
    #html_nodes(".cursor-pointer") %>%
    rvest::html_nodes(".component-user-name-15") %>%
    rvest::html_nodes("span") %>%
    rvest::html_text() %>%
    stringr::str_trim()

  return(dplyr::tibble(name, photo, name_id, premium))
}


#' get_users
#'
#' extract list of users from search request
#'
#' @param keywords charcter vector
#' @param pages integer vector
#' @return queries
#'
#' @export
scrape_users <- function(x){

  remDr$navigate(x)

  iframe <- remDr$getPageSource() %>%
    .[[1]] %>%
    xml2::read_html()

  return(get_users(iframe))
}


#' scrape_main
#'
#' main method for the scraper which gathers all programms
#'
#' @param keywords charcter vector
#' @return queries
#'
#' @export
scrape_main <- function(keywords){

  check_pages <- create_query(keywords, 1)

  remDr$navigate(check_pages)

  page_content <- remDr$getPageSource() %>%
    .[[1]] %>%
    xml2::read_html()

  pages <- page_content %>%
    rvest::html_node(".pagination-selected~ li+ li a") %>%
    rvest::html_text() %>%
    as.numeric()

  all_pages <- create_query(keywords, pages)

  scrape_users_progress <- progressively(scrape_users, length(all_pages))

  final <- all_pages %>%
    as.list() %>%
    purrr::map(scrape_users_progress) %>%
    purrr::reduce(bind_rows)

  Sys.sleep(rnorm(1, 1)^2)

  return(final)
}
