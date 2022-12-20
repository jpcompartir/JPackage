#' Quickly make the reequisite folders in the data_science_project_work folder for a project
#'
#' @param project_name Project's name, e.g. 'sustainbaility'
#' @param subdir The company + any additional extensions
#'
#' @return logicals telling you whether folders and files were created
#' @export
#'
#' @examples
#' \dontrun{
#' folder_setup("sustainability", subdir = "company_x/project_folders")
#' }
folder_setup <- function(project_name, subdir = "microsoft/project_work"){

  .ds_proj_work <- "~/Google Drive/My Drive/data_science_project_work/"
  .subdir <- paste0(subdir, "/")
  project_name <- paste0(project_name)

  project_folders <- c("/brief", "/viz", "/code", "/data", "/report")

  project_dir <-paste0(.ds_proj_work, .subdir, project_name)

  dir.create(project_dir)

  dirs_create <- paste0(project_dir, project_folders)

  purrr::map(dirs_create, dir.create)

  file.create(paste0(.ds_proj_work, .subdir, project_name, "/code/", project_name, "_wrangling.Rmd"))
  file.create(paste0(.ds_proj_work, .subdir, project_name, "/code/", project_name, "_initial_analysis.Rmd"))
}
