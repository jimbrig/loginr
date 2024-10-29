#' @import shiny
mod_login_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::div(
    class = "login-form",
    style = paste0(
      "max-width: 400px; margin: auto; padding: 30px; ",
      "border: 1px solid #ddd; border-radius: 8px; ",
      "box-shadow: 0 4px 8px rgba(0, 0, 0, 0.1); background-color: #fff;"
    ),
    shiny::h2(
      "Welcome Back",
      style = "text-align: center; margin-bottom: 20px; color: #333;"
    ),
    shiny::textInput(
      ns("username"),
      label = NULL,
      placeholder = "Username"
    ),
    password_input_ui(ns("password")),
    shiny::actionButton(
      ns("login"),
      "Login",
      class = "btn-primary",
      style = "width: 100%; margin-top: 20px;"
    ),
    shiny::tags$div(
      style = "text-align: center; margin-top: 10px;",
      shiny::tags$a(
        href = "#",
        "Forgot Password?",
        style = "color: #007bff; text-decoration: none;"
      )
    )
  )
}

mod_login_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    password_valid <- password_input_server("password")

    shiny::observeEvent(input$login, {
      if (!password_valid()) {
        shiny::showNotification(
          "Please ensure your password meets all requirements",
          type = "error"
        )
        return()
      }

      if (input$username == "") {
        shiny::showNotification(
          "Please enter a username",
          type = "error"
        )
        return()
      }

      # Add your login logic here
      shiny::showNotification(
        "Login successful!",
        type = "success"
      )
    })
  })
}
