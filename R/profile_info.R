#' get_profile
#'
#' gathers all base information scrapers by name_id
#'
#' @param keywords charcter vector
#' @param pages integer vector
#' @return queries
#'
#' @export
get_profile <- function(name_id){

  #name_id <- "Amelie_Schwinghammer"
  base <- "https://www.xing.com/profile/"
  embedd <- "https://www.xing.com/profile/version/embedded/"

  name_profile <- paste0(base, name_id)
  name_cv <- paste0(embedd, name_id, "/cv")
  name_group <- paste0(embedd, name_id, "/groups")

  # ### First Phase
  # remDr$navigate(name_profile)
  # remDr$screenshot(display = T)
  #
  # meta_content <- remDr$getPageSource() %>%
  #   .[[1]] %>%
  #   read_html
  #
  # profile <- get_profile_meta(meta_content)
  #
  remDr$navigate(name_cv)
  name_content <- remDr$getPageSource() %>%
    .[[1]] %>%
    xml2::read_html()

  pref <- get_pref(name_content)
  jobs_list <- get_job_list(name_content)
  edu <- get_edu(name_content)
  lang <- get_lang(name_content)
  inter <- get_inter(name_content)
  member <- get_member(name_content)

  remDr$navigate(name_group)
  group_content <- remDr$getPageSource() %>%
    .[[1]] %>%
    xml2::read_html()

  groups <- get_group(group_content)

  merge <- dplyr::tibble(name_id) %>%
    dplyr::bind_cols(.,pref, jobs_list, edu, lang, inter, member, groups)

  return(merge)
}
