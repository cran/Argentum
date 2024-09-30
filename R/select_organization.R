#' Select Organization
#'
#' @param search An optional search term to filter organizations
#' @param interactive_select Logical; whether to run in interactive mode. Defaults to `interactive()`
#' @return A data frame containing the selected organization's information, or NULL if no selection is made
#' @export
#' @importFrom utils menu
#'
#' @examples
#' # List available organizations
#' orgs <- argentum_list_organizations()
#' print(orgs)
#'
#' # Select organization non-interactively
#' selected_org <- argentum_select_organization(interactive_select = FALSE)
#' print(selected_org)
#'
#' if(interactive()){
#'   # Select an organization interactively
#'   selected_org <- argentum_select_organization()
#'
#'   # Select an organization with a search term
#'   selected_org <- argentum_select_organization("Example")
#' }
argentum_select_organization <- function(search = NULL, interactive_select = interactive()) {

    orgs <- argentum_list_organizations()

  # Filter by search term if provided
  if (!is.null(search)) {
    orgs <- orgs[grep(search, orgs$Name, ignore.case = TRUE), ]
  }

  # If no organizations match the criteria, return NULL
  if (nrow(orgs) == 0) {
    message("No organizations found matching the criteria.")
    return(NULL)
  }

  # Create a menu of organizations
  cat("Available organizations:\n")
  print(orgs$Name)

  if (interactive_select) {
    selection <- menu(orgs$Name, title = "Select an organization:")

    # Return the selected organization or NULL if none selected
    if (selection == 0) {
      return(NULL)
    } else {
      return(orgs[selection, ])
    }
  } else {
    # For non-interactive use, return the first organization
    message("Non-interactive mode: Selecting the first organization.")
    return(orgs[1, ])
  }
}
