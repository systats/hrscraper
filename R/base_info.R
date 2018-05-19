#' get_pref
#'
#' get self reported preferences
#'
#' @param html params
#' @return df
#'
#' @export
get_pref <- function(html){
  top_haves <- html %>%
    rvest::html_node(".top-haves-container") %>%
    rvest::html_nodes("li") %>%
    rvest::html_text() %>%
    stringr::str_trim() %>%
    list()

  haves <- html %>%
    rvest::html_node("#haves .pt0") %>%
    rvest::html_nodes("li") %>%
    rvest::html_text() %>%
    stringr::str_trim() %>%
    list()

  wants <- html %>%
    rvest::html_node("#wants .pt0") %>%
    rvest::html_nodes("li") %>%
    rvest::html_text() %>%
    stringr::str_trim() %>%
    list()

  return(tibble(top_haves, haves, wants))
}

#' get_job
#'
#' get one self reported occupation
#'
#' @param x xml_node
#' @return df
#'
#' @export
get_job <- function(x){

  time <- x %>%
    rvest::html_node(".top") %>%
    rvest::html_text() %>%
    ifelse(is.na(.), NA, .)

  jobs <- x %>%
    rvest::html_node(".job-title") %>%
    rvest::html_text() %>%
    ifelse(is.na(.), NA, .)

  business <- x %>%
    rvest::html_node("div.title > h4") %>%
    rvest::html_text() %>%
    stringr::str_trim() %>%
    ifelse(is.na(.), NA, .)

  business_url <- x %>%
    rvest::html_node("div > a") %>%
    rvest::html_text() %>%
    ifelse(is.na(.), NA, .)

  return(dplyr::tibble(time, jobs, business, business_url))
}

#' get_job_list
#'
#' get self reported occupation list
#'
#' @param html
#' @return df
#'
#' @export
get_job_list <- function(html){
  jobs_list <- html %>%
    rvest::html_node("#work-experience") %>%
    rvest::html_nodes(".details") %>%
    purrr::map(~get_job(.x)) %>%
    purrr::reduce(bind_rows)

  return(dplyr::tibble(jobs_list = list(jobs_list)))
}

#' get_edu
#'
#' get list of educational steps
#'
#' @param html
#' @return df
#'
#' @export
get_edu <- function(html){
  edu_time <- html %>%
    rvest::html_nodes("#education .details") %>%
    rvest::html_nodes(".top") %>%
    rvest::html_text() %>%
    stringr::str_trim()

  edu_inst <- html %>%
    rvest::html_nodes("#education .details") %>%
    rvest::html_nodes(".inv-link") %>%
    rvest::html_text()

  edu_desc <- html %>%
    rvest::html_nodes("#education .details") %>%
    rvest::html_nodes(".education-description") %>%
    rvest::html_text() %>%
    stringr::str_trim()

  edu <- dplyr::tibble(edu = list(dplyr::tibble(edu_time, edu_inst, edu_desc)))
  return(edu)
}

#' get_langs
#'
#' get list of self reported language skills
#'
#' @param html
#' @return df
#'
#' @export
get_lang <- function(html){
  lang <- html %>%
    rvest::html_nodes("#language-skills div") %>%
    rvest::html_text() %>%
    stringr::str_trim() %>%
    stringr::str_split("\\s+") %>%
    map_chr(~.x[1])

  skill <- html %>%
    rvest::html_nodes("#language-skills div") %>%
    rvest::html_text() %>%
    stringr::str_trim() %>%
    stringr::str_split("\\s+") %>%
    purrr::map_chr(~.x[2]) %>%
    stringr::str_replace_all("\\(|\\)", "")


  langs <- dplyr::tibble(lang, skill)
  return(dplyr::tibble(langs = list(langs)))
}

#' get_qualis
#'
#' get list of self reported qualifications
#'
#' @param html
#' @return df
#'
#' @export
get_quali <- function(html){
  html %>%
    rvest::html_nodes("#qualifications .details") %>%
    rvest::html_text() %>%
    stringr::str_trim() %>%
    list()
}

#' get_inter
#'
#' get list of self reported interests
#'
#' @param html
#' @return df
#'
#' @export
get_inter <- function(html){
  inter <- html %>%
    rvest::html_nodes("#qualifications .details") %>%
    rvest::html_text() %>%
    stringr::str_trim()

  return(dplyr::tibble(inter = list(inter)))
}

#' get_member
#'
#' get time of mebership and profile visits
#'
#' @param html
#' @return df
#'
#' @export
get_member <- function(html){
  visits <- html %>%
    rvest::html_node("#member-since .title") %>%
    rvest::html_text() %>%
    stringr::str_trim() %>%
    stringr::str_extract("\\d*?$") %>%
    as.numeric

  since <- html %>%
    rvest::html_node("#member-since .title") %>%
    rvest::html_text() %>%
    stringr::str_trim() %>%
    stringr::str_extract("^.*?\\d{4}")

  member <- dplyr::tibble(visits, since)
  return(member)
}

#' get_group
#'
#' get list joined groups
#'
#' @param html
#' @return df
#'
#' @export
get_group <- function(html){

  caption <- html %>%
    rvest::html_nodes("strong") %>%
    rvest::html_text()

  group_stats <- html %>%
    rvest::html_nodes("span") %>%
    rvest::html_text()

  members <- group_stats %>%
    stringr::str_detect("Mitglieder") %>%
    group_stats[.] %>%
    stringr::str_extract("\\d+")

  posts <- group_stats %>%
    stringr::str_detect(" Beiträge") %>%
    group_stats[.] %>%
    stringr::str_extract("\\d+")

  comments <- group_stats %>%
    stringr::str_detect(" Kommentare") %>%
    group_stats[.] %>%
    stringr::str_extract("\\d+")

  groups <- dplyr::tibble(caption, members, posts, comments)
  return(dplyr::tibble(groups = list(groups)))
}
