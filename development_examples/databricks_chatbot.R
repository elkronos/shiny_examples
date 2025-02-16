library(shiny)
library(ellmer)
library(shinyjs)

ui <- fluidPage(
  useShinyjs(),
  titlePanel("Databricks Chatbot"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("prompt", "Your prompt:", value = "", placeholder = "Type your message here..."),
      actionButton("send", "Send", class = "btn-primary"),
      br(), br(),
      helpText("This dashboard uses ellmer::chat_databricks() to query a Databricks-hosted model.")
    ),
    
    mainPanel(
      h4("Chat History"),
      div(
        id = "chat-container",
        style = "height: 400px; overflow-y: auto; border: 1px solid #ddd; padding: 10px;",
        uiOutput("chatHistory")
      ),
      tags$style(HTML("
        .user-msg { color: #007bff; font-weight: bold; }
        .bot-msg { color: #28a745; margin-left: 10px; }
        .error-msg { color: #dc3545; }
        .chat-entry { margin-bottom: 10px; }
        .timestamp { font-size: 0.8em; color: #888; margin-left: 5px; }
      "))
    )
  ),
  
  tags$script(HTML("
    // Bind the Enter key just once on document ready
    $(document).ready(function() {
      $('#prompt').on('keypress', function(e) {
        if (e.which == 13) {
          $('#send').click();
          e.preventDefault();
        }
      });
    });
    
    // Scroll the chat container to the bottom
    Shiny.addCustomMessageHandler('scrollDown', function(message) {
      var chatDiv = $('#chat-container');
      if (chatDiv.length) {
        chatDiv.scrollTop(chatDiv[0].scrollHeight);
      }
    });
  "))
)

server <- function(input, output, session) {
  
  # Store conversation history with timestamps
  chat <- reactiveValues(history = list())
  
  # Helper to add a message entry to the history
  appendChat <- function(role, message, isError = FALSE) {
    chat$history <- append(chat$history, list(list(
      role = role,
      message = message,
      error = isError,
      time = format(Sys.time(), "%H:%M:%S")
    )))
  }
  
  observeEvent(input$send, {
    # Trim whitespace from the input
    user_input <- trimws(input$prompt)
    
    if (nchar(user_input) == 0) {
      showNotification("Please enter a prompt before sending.", type = "warning", duration = 3)
      return()
    }
    
    disable("send")
    # Optionally, further sanitization could be applied here if needed.
    appendChat("User", user_input)
    updateTextInput(session, "prompt", value = "")
    
    # Initialize a normalized response
    normalized_response <- list(success = FALSE, message = NULL)
    
    res <- withProgress(message = "Waiting for response...", {
      tryCatch({
        # Call the API and normalize the output
        result <- chat_databricks(prompt = user_input)
        
        # If result is a list and contains error info, handle accordingly
        if (is.list(result)) {
          if (!is.null(result$error) && isTRUE(result$error)) {
            normalized_response$message <- as.character(result$message)
          } else if (!is.null(result$message)) {
            normalized_response$success <- TRUE
            normalized_response$message <- as.character(result$message)
          } else {
            normalized_response$success <- TRUE
            normalized_response$message <- paste(result)  # fallback conversion
          }
        } else {
          normalized_response$success <- TRUE
          normalized_response$message <- as.character(result)
        }
        normalized_response
      },
      error = function(e) {
        # Use a normalized error structure
        showNotification(paste("Error:", e$message), type = "error", duration = 5)
        list(success = FALSE, message = paste("Error:", e$message))
      })
    })
    
    # Append bot's response based on normalized output
    if (!isTRUE(res$success)) {
      appendChat("Bot", res$message, isError = TRUE)
    } else {
      appendChat("Bot", res$message)
    }
    
    enable("send")  # Ensure the button is always re-enabled
    
    session$sendCustomMessage("scrollDown", list())
  })
  
  output$chatHistory <- renderUI({
    chat_entries <- lapply(chat$history, function(entry) {
      role_class <- if (entry$role == "User") "user-msg" else "bot-msg"
      if (entry$error) role_class <- paste(role_class, "error-msg")
      
      div(class = "chat-entry",
          span(class = role_class, paste0(entry$role, ": ")),
          span(entry$message),
          span(class = "timestamp", paste0("(", entry$time, ")"))
      )
    })
    tagList(chat_entries)
  })
}

shinyApp(ui, server)
