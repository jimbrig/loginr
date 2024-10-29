#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    bslib::page_fluid(
      bslib::card(
        mod_login_ui("login")
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  www_dir <- app_sys("app/www")
  
  # Create www directory if it doesn't exist
  if (!dir.exists(www_dir)) {
    dir.create(www_dir, recursive = TRUE)
  }
  
  add_resource_path(
    "www",
    www_dir
  )

  # Compile SCSS to CSS
  sass::sass(
    input = sass::sass_file(file.path(www_dir, "styles.scss")),
    output = file.path(www_dir, "styles.css"),
    options = sass::sass_options(output_style = "compressed")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = www_dir,
      app_title = "loginr"
    ),
    shinyjs::useShinyjs(),
    # Add the compiled CSS file
    tags$link(rel = "stylesheet", type = "text/css", href = "www/styles.css")
  )
}
