#' @import shiny
#' @import shinyjs
#' @import sass
password_input_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::tags$head(
      shiny::tags$link(
        rel = "stylesheet",
        href = paste0(
          "https://cdnjs.cloudflare.com/ajax/libs/",
          "font-awesome/6.0.0/css/all.min.css"
        )
      )
    ),

    shiny::div(
      class = "password-module",
      shiny::div(
        class = "password-container",
        shiny::passwordInput(
          ns("password"),
          NULL,
          placeholder = "Enter password"
        ),
        shiny::tags$i(
          class = "fas fa-eye-slash",
          id = ns("togglePassword"),
          onclick = sprintf(paste0(
            "if(document.getElementById('%s')) {",
            "  var pw = document.getElementById('%s');",
            "  var type = pw.type === 'password' ? 'text' : 'password';",
            "  pw.type = type;",
            "  this.classList.toggle('fa-eye');",
            "  this.classList.toggle('fa-eye-slash');",
            "}"
          ), ns("password"), ns("password"))
        )
      ),
      shiny::div(
        class = "strength-meter",
        shiny::div(
          class = "meter-fill",
          id = ns("meter-fill")
        )
      ),
      shiny::div(
        class = "strength-text",
        shiny::textOutput(ns("strength_text"))
      ),
      shiny::uiOutput(ns("validation_messages"))
    )
  )
}

password_input_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {

    validate_password <- function(password) {
      validations <- list(
        list(
          test = nchar(password) >= 8,
          message = "Password must be at least 8 characters long"
        ),
        list(
          test = grepl("[A-Z]", password),
          message = "Password must contain at least one uppercase letter"
        ),
        list(
          test = grepl("[a-z]", password),
          message = "Password must contain at least one lowercase letter"
        ),
        list(
          test = grepl("[0-9]", password),
          message = "Password must contain at least one number"
        ),
        list(
          test = grepl("[^A-Za-z0-9]", password),
          message = "Password must contain at least one special character"
        )
      )

      failed_validations <- Filter(function(x) !x$test, validations)
      list(
        valid = length(failed_validations) == 0,
        messages = sapply(failed_validations, function(x) x$message)
      )
    }

    calculate_strength <- function(password) {
      score <- 0
      if (nchar(password) >= 8) score <- score + 20
      if (grepl("[A-Z]", password)) score <- score + 20
      if (grepl("[a-z]", password)) score <- score + 20
      if (grepl("[0-9]", password)) score <- score + 20
      if (grepl("[^A-Za-z0-9]", password)) score <- score + 20
      score
    }

    get_strength_color <- function(score) {
      if (score <= 20) return("#f44336")      # Red
      if (score <= 40) return("#ff9800")      # Orange
      if (score <= 60) return("#ffeb3b")      # Yellow
      if (score <= 80) return("#4caf50")      # Light Green
      return("#2e7d32")                       # Dark Green
    }

    get_strength_text <- function(score) {
      if (score <= 20) return("Very Weak")
      if (score <= 40) return("Weak")
      if (score <= 60) return("Medium")
      if (score <= 80) return("Strong")
      return("Very Strong")
    }

    shiny::observe({
      password <- input$password
      if (is.null(password) || password == "") {
        score <- 0
      } else {
        score <- calculate_strength(password)
      }

      color <- get_strength_color(score)

      # Update meter fill
      shinyjs::runjs(sprintf(paste0(
        "document.querySelector('#%s').style.width = '%d%%';",
        "document.querySelector('#%s').style.backgroundColor = '%s';"
      ), session$ns("meter-fill"), score, session$ns("meter-fill"), color))

      # Update strength text
      output$strength_text <- shiny::renderText({
        if (is.null(password) || password == "") return("")
        get_strength_text(score)
      })

      # Update validation messages
      output$validation_messages <- shiny::renderUI({
        if (is.null(password) || password == "") return(NULL)

        validation_result <- validate_password(password)
        if (!validation_result$valid) {
          shiny::div(
            class = "validation-messages",
            lapply(validation_result$messages, function(msg) {
              shiny::div(class = "text-danger", msg)
            })
          )
        }
      })
    })

    shiny::reactive({
      password <- input$password
      if (is.null(password) || password == "") return(FALSE)
      validate_password(password)$valid
    })
  })
}
