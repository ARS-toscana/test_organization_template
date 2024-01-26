# Import mailing list and create variable/secret
# repo scope, https://issue-forms-creator.netlify.app/new

create_mailing_list <- function() {
  # Parameters
  create_secret <- F
  mailing_list_loc <- here::here("p_PI", "mailing_list.csv")
  
  # Retrieve remote name
  remotes <- gert::git_remote_list()
  remote <- sub('\\.git$', '', remotes[remotes$name == "origin", "url"])
  
  # Read csv containing the mailing list
  mailing_list <- data.table::as.data.table(utils::read.table(text = readLines(mailing_list_loc, warn = FALSE),
                                                              header = T, sep = ",", strip.white = T, comment.char = "#"))
  
  # Setup parameter for GitHub request
  body_request <- paste0('{"name":"MAILING_LIST","value":"', paste0(mailing_list$email, collapse = ", "), '"}')
  headers <- c(
    Accept = "Accept: application/vnd.github+json",
    "Content-Type" = "application/json",
    "X-GitHub-Api-Version" = "2022-11-28"
  )
  
  # Auxiliary function
  split_path <- function(path) {
    if (dirname(path) %in% c(".", path)) return(basename(path))
    return(c(basename(path), split_path(dirname(path))))
  }
  
  owner <- split_path(remote)[2]
  repo <- split_path(remote)[1]
  
  # First request to retrieve all variables/secrets
  res <- gh::gh("GET /repos/{owner}/{repo}/actions/variables", owner = owner,  repo = repo, .send_headers = headers)
  
  # Extract names and values
  gh_vars <- gh_var_names <- sapply(res$variable, "[[", "value")
  names(gh_vars) <- sapply(res$variable, "[[", "name")
  
  # Important
  rm(res)
  
  # If MAILING_LIST does not exists yet create it otherwise if value has changed update it
  if ("MAILING_LIST" %in% names(gh_vars)) {
    if (gh_vars["MAILING_LIST"] != paste0(mailing_list$email, collapse = ", ")) {
      res <- gh::gh("PATCH /repos/{owner}/{repo}/actions/variables/{name}", owner = owner, repo = repo,
                    name = "MAILING_LIST", charToRaw(body_request), .send_headers = headers)
    }
  } else {
    res <- gh::gh("POST /repos/{owner}/{repo}/actions/variables", owner = owner, repo = repo,
                  charToRaw(body_request), .send_headers = headers)
  }
  
  # Print result for user
  if (exists("res")) {
    message("mailing list has been updated")
  } else {
    message("mailing list has not changed since last time")
  }
  
}
