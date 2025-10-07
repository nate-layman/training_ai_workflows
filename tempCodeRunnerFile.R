.brick.extraction {
        background-image: url('data:image/svg+xml,%3Csvg xmlns=%22http://www.w3.org/2000/svg%22 viewBox=%220 0 90 90%22%3E%3Crect x=%220.5%22 y=%2230.234%22 width=%2289%22 height=%2246.503%22 rx=%222.905%22 fill=%22%232196F3%22 stroke=%22%231d1d1b%22 stroke-width=%221%22/%3E%3Cpath d=%22M 45 76.737 H 4.141 c -2.011 0 -3.641 -1.63 -3.641 -3.641 v -39.22 c 0 -2.011 1.63 -3.641 3.641 -3.641 h 16.103 C 20.436 47.774 25.153 62.834 45 76.737 z%22 fill=%22%23000%22 fill-opacity=%220.15%22/%3E%3Crect x=%2211.706%22 y=%2220.957%22 width=%2210.908%22 height=%2222.076%22 rx=%222.767%22 fill=%22%232196F3%22 stroke=%22%231d1d1b%22 stroke-width=%221%22/%3E%3Cpath d=%22M 19.927 43.033 h -4.752 c -1.916 0 -3.469 -1.553 -3.469 -3.469 V 24.426 c 0 -1.916 1.553 -3.469 3.469 -3.469 C 15.287 28.879 16.03 35.674 19.927 43.033 z%22 fill=%22%23000%22 fill-opacity=%220.15%22/%3E%3Crect x=%2239.546%22 y=%2220.957%22 width=%2210.908%22 height=%2222.076%22 rx=%222.767%22 fill=%22%232196F3%22 stroke=%22%231d1d1b%22 stroke-width=%221%22/%3E%3Cpath d=%22M 45 43.033 h -4.752 c -1.916 0 -3.469 -1.553 -3.469 -3.469 V 24.426 c 0 -1.916 1.553 -3.469 3.469 -3.469 C 40.36 28.879 41.103 35.674 45 43.033 z%22 fill=%22%23000%22 fill-opacity=%220.15%22/%3E%3Crect x=%2267.386%22 y=%2220.957%22 width=%2210.908%22 height=%2222.076%22 rx=%222.767%22 fill=%22%232196F3%22 stroke=%22%231d1d1b%22 stroke-width=%221%22/%3E%3Cpath d=%22M 70.073 43.033 h -4.752 c -1.916 0 -3.469 -1.553 -3.469 -3.469 V 24.426 c 0 -1.916 1.553 -3.469 3.469 -3.469 C 65.433 28.879 66.176 35.674 70.073 43.033 z%22 fill=%22%23000%22 fill-opacity=%220.15%22/%3E%3C/svg%3E');
      }
      
      .brick.prompt {
        background-image: url('data:image/svg+xml,%3Csvg xmlns=%22http://www.w3.org/2000/svg%22 viewBox=%220 0 90 90%22%3E%3Crect x=%220.5%22 y=%2230.234%22 width=%2289%22 height=%2246.503%22 rx=%222.905%22 fill=%22%234CAF50%22 stroke=%22%231d1d1b%22 stroke-width=%221%22/%3E%3Cpath d=%22M 45 76.737 H 4.141 c -2.011 0 -3.641 -1.63 -3.641 -3.641 v -39.22 c 0 -2.011 1.63 -3.641 3.641 -3.641 h 16.103 C 20.436 47.774 25.153 62.834 45 76.737 z%22 fill=%22%23000%22 fill-opacity=%220.15%22/%3E%3Crect x=%2211.706%22 y=%2220.957%22 width=%2210.908%22 height=%2222.076%22 rx=%222.767%22 fill=%22%234CAF50%22 stroke=%22%231d1d1b%22 stroke-width=%221%22/%3E%3Cpath d=%22M 19.927 43.033 h -4.752 c -1.916 0 -3.469 -1.553 -3.469 -3.469 V 24.426 c 0 -1.916 1.553 -3.469 3.469 -3.469 C 15.287 28.879 16.03 35.674 19.927 43.033 z%22 fill=%22%23000%22 fill-opacity=%220.15%22/%3E%3Crect x=%2239.546%22 y=%2220.957%22 width=%2210.908%22 height=%2222.076%22 rx=%222.767%22 fill=%22%234CAF50%22 stroke=%22%231d1d1b%22 stroke-width=%221%22/%3E%3Cpath d=%22M 45 43.033 h -4.752 c -1.916 0 -3.469 -1.553 -3.469 -3.469 V 24.426 c 0 -1.916 1.553 -3.469 3.469 -3.469 C 40.36 28.879 41.103 35.674 45 43.033library(shiny)
library(shinyjs)

# Simple keyword-based classification function
classify_text <- function(text) {
  text_lower <- tolower(text)
  
  # Define theme keywords
  extraction_keywords <- c("grant number", "principal investigator", "pi", "due date", 
                          "funding", "budget", "institution", "co-investigator", 
                          "start date", "end date", "extract", "get", "find")
  
  prompt_keywords <- c("summarize", "synthesis", "explain", "analyze", "compare", 
                      "describe", "review", "evaluate", "identify", "overview",
                      "assess", "examine")
  
  formatting_keywords <- c("table", "markdown", "csv", "json", "format", "structure",
                          "organize", "export", "list", "bullet", "numbered")
  
  # Calculate scores
  extraction_score <- sum(sapply(extraction_keywords, function(kw) grepl(kw, text_lower)))
  prompt_score <- sum(sapply(prompt_keywords, function(kw) grepl(kw, text_lower)))
  formatting_score <- sum(sapply(formatting_keywords, function(kw) grepl(kw, text_lower)))
  
  scores <- c(extraction = extraction_score, prompt = prompt_score, formatting = formatting_score)
  
  # Return theme with highest score, default to extraction if all zero
  if (max(scores) == 0) {
    return("extraction")
  }
  
  names(scores)[which.max(scores)]
}

ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$style(HTML("
      body {
        background-color: #f5f5f5;
        font-family: Arial, sans-serif;
      }
      
      .container-fluid {
        max-width: 1200px;
        margin: 0 auto;
        padding: 20px;
      }
      
      h2 {
        text-align: center;
        color: #333;
        margin-bottom: 30px;
      }
      
      .legend-box {
        background: white;
        border-radius: 10px;
        padding: 20px;
        box-shadow: 0 2px 8px rgba(0,0,0,0.1);
        margin-bottom: 20px;
      }
      
      .legend-box h4 {
        margin-top: 0;
        color: #666;
      }
      
      .legend-items {
        display: flex;
        gap: 30px;
        flex-wrap: wrap;
      }
      
      .legend-item {
        display: flex;
        align-items: center;
        gap: 10px;
      }
      
      .legend-color {
        width: 30px;
        height: 30px;
        border-radius: 5px;
        border: 2px solid rgba(0,0,0,0.3);
      }
      
      .input-area {
        background: white;
        border-radius: 10px;
        padding: 20px;
        box-shadow: 0 2px 8px rgba(0,0,0,0.1);
        margin-bottom: 30px;
      }
      
      .input-area h4 {
        margin-top: 0;
        color: #666;
      }
      
      #task_input {
        width: 100%;
        padding: 12px;
        font-size: 16px;
        border-radius: 5px;
        border: 3px solid #ccc;
        margin-bottom: 10px;
        box-sizing: border-box;
      }
      
      .draggable-brick {
        width: 180px;
        height: 80px;
        margin: 10px auto;
        cursor: move;
        position: relative;
        display: flex;
        align-items: center;
        justify-content: center;
        text-align: center;
        user-select: none;
        background-size: contain;
        background-repeat: no-repeat;
        background-position: center;
        color: white;
        font-weight: bold;
        font-size: 14px;
        padding: 10px;
        word-wrap: break-word;
      }
      
      .draggable-brick.extraction {
        background-image: url('data:image/svg+xml,%3Csvg xmlns=%22http://www.w3.org/2000/svg%22 viewBox=%220 0 90 90%22%3E%3Crect x=%220.5%22 y=%2230.234%22 width=%2289%22 height=%2246.503%22 rx=%222.905%22 fill=%22%232196F3%22 stroke=%22%231d1d1b%22 stroke-width=%221%22/%3E%3Cpath d=%22M 45 76.737 H 4.141 c -2.011 0 -3.641 -1.63 -3.641 -3.641 v -39.22 c 0 -2.011 1.63 -3.641 3.641 -3.641 h 16.103 C 20.436 47.774 25.153 62.834 45 76.737 z%22 fill=%22%23000%22 fill-opacity=%220.15%22/%3E%3Crect x=%2211.706%22 y=%2220.957%22 width=%2210.908%22 height=%2222.076%22 rx=%222.767%22 fill=%22%232196F3%22 stroke=%22%231d1d1b%22 stroke-width=%221%22/%3E%3Cpath d=%22M 19.927 43.033 h -4.752 c -1.916 0 -3.469 -1.553 -3.469 -3.469 V 24.426 c 0 -1.916 1.553 -3.469 3.469 -3.469 C 15.287 28.879 16.03 35.674 19.927 43.033 z%22 fill=%22%23000%22 fill-opacity=%220.15%22/%3E%3Crect x=%2239.546%22 y=%2220.957%22 width=%2210.908%22 height=%2222.076%22 rx=%222.767%22 fill=%22%232196F3%22 stroke=%22%231d1d1b%22 stroke-width=%221%22/%3E%3Cpath d=%22M 45 43.033 h -4.752 c -1.916 0 -3.469 -1.553 -3.469 -3.469 V 24.426 c 0 -1.916 1.553 -3.469 3.469 -3.469 C 40.36 28.879 41.103 35.674 45 43.033 z%22 fill=%22%23000%22 fill-opacity=%220.15%22/%3E%3Crect x=%2267.386%22 y=%2220.957%22 width=%2210.908%22 height=%2222.076%22 rx=%222.767%22 fill=%22%232196F3%22 stroke=%22%231d1d1b%22 stroke-width=%221%22/%3E%3Cpath d=%22M 70.073 43.033 h -4.752 c -1.916 0 -3.469 -1.553 -3.469 -3.469 V 24.426 c 0 -1.916 1.553 -3.469 3.469 -3.469 C 65.433 28.879 66.176 35.674 70.073 43.033 z%22 fill=%22%23000%22 fill-opacity=%220.15%22/%3E%3C/svg%3E');
      }
      
      .draggable-brick.prompt {
        background-image: url('data:image/svg+xml,%3Csvg xmlns=%22http://www.w3.org/2000/svg%22 viewBox=%220 0 90 90%22%3E%3Crect x=%220.5%22 y=%2230.234%22 width=%2289%22 height=%2246.503%22 rx=%222.905%22 fill=%22%234CAF50%22 stroke=%22%231d1d1b%22 stroke-width=%221%22/%3E%3Cpath d=%22M 45 76.737 H 4.141 c -2.011 0 -3.641 -1.63 -3.641 -3.641 v -39.22 c 0 -2.011 1.63 -3.641 3.641 -3.641 h 16.103 C 20.436 47.774 25.153 62.834 45 76.737 z%22 fill=%22%23000%22 fill-opacity=%220.15%22/%3E%3Crect x=%2211.706%22 y=%2220.957%22 width=%2210.908%22 height=%2222.076%22 rx=%222.767%22 fill=%22%234CAF50%22 stroke=%22%231d1d1b%22 stroke-width=%221%22/%3E%3Cpath d=%22M 19.927 43.033 h -4.752 c -1.916 0 -3.469 -1.553 -3.469 -3.469 V 24.426 c 0 -1.916 1.553 -3.469 3.469 -3.469 C 15.287 28.879 16.03 35.674 19.927 43.033 z%22 fill=%22%23000%22 fill-opacity=%220.15%22/%3E%3Crect x=%2239.546%22 y=%2220.957%22 width=%2210.908%22 height=%2222.076%22 rx=%222.767%22 fill=%22%234CAF50%22 stroke=%22%231d1d1b%22 stroke-width=%221%22/%3E%3Cpath d=%22M 45 43.033 h -4.752 c -1.916 0 -3.469 -1.553 -3.469 -3.469 V 24.426 c 0 -1.916 1.553 -3.469 3.469 -3.469 C 40.36 28.879 41.103 35.674 45 43.033 z%22 fill=%22%23000%22 fill-opacity=%220.15%22/%3E%3Crect x=%2267.386%22 y=%2220.957%22 width=%2210.908%22 height=%2222.076%22 rx=%222.767%22 fill=%22%234CAF50%22 stroke=%22%231d1d1b%22 stroke-width=%221%22/%3E%3Cpath d=%22M 70.073 43.033 h -4.752 c -1.916 0 -3.469 -1.553 -3.469 -3.469 V 24.426 c 0 -1.916 1.553 -3.469 3.469 -3.469 C 65.433 28.879 66.176 35.674 70.073 43.033 z%22 fill=%22%23000%22 fill-opacity=%220.15%22/%3E%3C/svg%3E');
      }
      
      .draggable-brick.formatting {
        background-image: url('data:image/svg+xml,%3Csvg xmlns=%22http://www.w3.org/2000/svg%22 viewBox=%220 0 90 90%22%3E%3Crect x=%220.5%22 y=%2230.234%22 width=%2289%22 height=%2246.503%22 rx=%222.905%22 fill=%22%23FFC107%22 stroke=%22%231d1d1b%22 stroke-width=%221%22/%3E%3Cpath d=%22M 45 76.737 H 4.141 c -2.011 0 -3.641 -1.63 -3.641 -3.641 v -39.22 c 0 -2.011 1.63 -3.641 3.641 -3.641 h 16.103 C 20.436 47.774 25.153 62.834 45 76.737 z%22 fill=%22%23000%22 fill-opacity=%220.15%22/%3E%3Crect x=%2211.706%22 y=%2220.957%22 width=%2210.908%22 height=%2222.076%22 rx=%222.767%22 fill=%22%23FFC107%22 stroke=%22%231d1d1b%22 stroke-width=%221%22/%3E%3Cpath d=%22M 19.927 43.033 h -4.752 c -1.916 0 -3.469 -1.553 -3.469 -3.469 V 24.426 c 0 -1.916 1.553 -3.469 3.469 -3.469 C 15.287 28.879 16.03 35.674 19.927 43.033 z%22 fill=%22%23000%22 fill-opacity=%220.15%22/%3E%3Crect x=%2239.546%22 y=%2220.957%22 width=%2210.908%22 height=%2222.076%22 rx=%222.767%22 fill=%22%23FFC107%22 stroke=%22%231d1d1b%22 stroke-width=%221%22/%3E%3Cpath d=%22M 45 43.033 h -4.752 c -1.916 0 -3.469 -1.553 -3.469 -3.469 V 24.426 c 0 -1.916 1.553 -3.469 3.469 -3.469 C 40.36 28.879 41.103 35.674 45 43.033 z%22 fill=%22%23000%22 fill-opacity=%220.15%22/%3E%3Crect x=%2267.386%22 y=%2220.957%22 width=%2210.908%22 height=%2222.076%22 rx=%222.767%22 fill=%22%23FFC107%22 stroke=%22%231d1d1b%22 stroke-width=%221%22/%3E%3Cpath d=%22M 70.073 43.033 h -4.752 c -1.916 0 -3.469 -1.553 -3.469 -3.469 V 24.426 c 0 -1.916 1.553 -3.469 3.469 -3.469 C 65.433 28.879 66.176 35.674 70.073 43.033 z%22 fill=%22%23000%22 fill-opacity=%220.15%22/%3E%3C/svg%3E');
      }
      
      .classification-label {
        margin-top: 10px;
        font-size: 14px;
        color: #666;
      }
      
      .brick {
        width: 180px;
        height: 80px;
        margin: 5px auto;
        position: relative;
        display: flex;
        align-items: center;
        justify-content: center;
        text-align: center;
        color: white;
        font-weight: bold;
        font-size: 14px;
        padding: 10px;
        word-wrap: break-word;
        background-size: contain;
        background-repeat: no-repeat;
        background-position: center;
      }
      
      .brick.extraction {
        background-image: url('data:image/svg+xml,%3Csvg xmlns=%22http://www.w3.org/2000/svg%22 viewBox=%220 0 200 100%22%3E%3Crect width=%22200%22 height=%22100%22 rx=%2210%22 fill=%22%232196F3%22/%3E%3Ccircle cx=%2240%22 cy=%2220%22 r=%228%22 fill=%22rgba(255,255,255,0.3)%22/%3E%3Ccircle cx=%2270%22 cy=%2220%22 r=%228%22 fill=%22rgba(255,255,255,0.3)%22/%3E%3Ccircle cx=%22100%22 cy=%2220%22 r=%228%22 fill=%22rgba(255,255,255,0.3)%22/%3E%3Ccircle cx=%22130%22 cy=%2220%22 r=%228%22 fill=%22rgba(255,255,255,0.3)%22/%3E%3Ccircle cx=%22160%22 cy=%2220%22 r=%228%22 fill=%22rgba(255,255,255,0.3)%22/%3E%3C/svg%3E');
      }
      
      .brick.prompt {
        background-image: url('data:image/svg+xml,%3Csvg xmlns=%22http://www.w3.org/2000/svg%22 viewBox=%220 0 200 100%22%3E%3Crect width=%22200%22 height=%22100%22 rx=%2210%22 fill=%22%234CAF50%22/%3E%3Ccircle cx=%2240%22 cy=%2220%22 r=%228%22 fill=%22rgba(255,255,255,0.3)%22/%3E%3Ccircle cx=%2270%22 cy=%2220%22 r=%228%22 fill=%22rgba(255,255,255,0.3)%22/%3E%3Ccircle cx=%22100%22 cy=%2220%22 r=%228%22 fill=%22rgba(255,255,255,0.3)%22/%3E%3Ccircle cx=%22130%22 cy=%2220%22 r=%228%22 fill=%22rgba(255,255,255,0.3)%22/%3E%3Ccircle cx=%22160%22 cy=%2220%22 r=%228%22 fill=%22rgba(255,255,255,0.3)%22/%3E%3C/svg%3E');
      }
      
      .brick.formatting {
        background-image: url('data:image/svg+xml,%3Csvg xmlns=%22http://www.w3.org/2000/svg%22 viewBox=%220 0 200 100%22%3E%3Crect width=%22200%22 height=%22100%22 rx=%2210%22 fill=%22%23FFC107%22/%3E%3Ccircle cx=%2240%22 cy=%2220%22 r=%228%22 fill=%22rgba(255,255,255,0.3)%22/%3E%3Ccircle cx=%2270%22 cy=%2220%22 r=%228%22 fill=%22rgba(255,255,255,0.3)%22/%3E%3Ccircle cx=%22100%22 cy=%2220%22 r=%228%22 fill=%22rgba(255,255,255,0.3)%22/%3E%3Ccircle cx=%22130%22 cy=%2220%22 r=%228%22 fill=%22rgba(255,255,255,0.3)%22/%3E%3Ccircle cx=%22160%22 cy=%2220%22 r=%228%22 fill=%22rgba(255,255,255,0.3)%22/%3E%3C/svg%3E');
      }
      
      .extraction { background-color: #2196F3; }
      .prompt { background-color: #4CAF50; }
      .formatting { background-color: #FFC107; }
      
      #draggable-container {
        display: flex;
        justify-content: center;
        margin: 20px 0;
      }
      
      .stack-area {
        background: white;
        border-radius: 10px;
        padding: 20px;
        min-height: 400px;
        box-shadow: 0 2px 8px rgba(0,0,0,0.1);
        border: 3px dashed #ccc;
      }
      
      .stack-area.drag-over {
        border-color: #4CAF50;
        background-color: #f0f8f0;
      }
      
      .stack-area h4 {
        margin-top: 0;
        color: #666;
      }
      
      .stack-container {
        min-height: 300px;
        padding-top: 20px;
      }
      
      .clear-btn {
        display: inline-block;
        margin: 0;
        padding: 10px 30px;
        background-color: #f44336;
        color: white;
        border: none;
        border-radius: 5px;
        cursor: pointer;
        font-size: 16px;
      }
      
      .clear-btn:hover {
        background-color: #d32f2f;
      }
      
      .empty-message {
        text-align: center;
        color: #999;
        padding: 50px;
        font-size: 18px;
      }
      
      .drag-instruction {
        color: #666;
        font-style: italic;
        margin-top: 10px;
        font-size: 14px;
      }
    "))
  ),
  
  titlePanel("Research Admin Task Classifier"),
  
  # Legend
  div(class = "legend-box",
    h4("Theme Colors:"),
    div(class = "legend-items",
      div(class = "legend-item",
        div(class = "legend-color extraction"),
        tags$strong("Extraction")
      ),
      div(class = "legend-item",
        div(class = "legend-color prompt"),
        tags$strong("Prompt")
      ),
      div(class = "legend-item",
        div(class = "legend-color formatting"),
        tags$strong("Formatting")
      )
    )
  ),
  
  # Input Area
  div(class = "input-area",
    h4("Create a task brick:"),
    textInput("task_input", NULL, 
              placeholder = "e.g., 'extract grant number' or 'summarize findings' or 'format as table'"),
    div(id = "draggable-container",
      uiOutput("draggable_brick")
    ),
    div(class = "classification-label",
      "Current classification: ",
      tags$strong(textOutput("current_theme", inline = TRUE))
    ),
    div(class = "drag-instruction",
      "↓ Drag the brick above down to the stack below ↓"
    )
  ),
  
  # Stack Area
  div(class = "stack-area", id = "drop-zone",
    div(style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 20px;",
      h4(style = "margin: 0;", textOutput("stack_count", inline = TRUE)),
      conditionalPanel(
        condition = "output.has_blocks",
        actionButton("clear", "Clear Stack", class = "clear-btn")
      )
    ),
    div(class = "stack-container", id = "stack",
      uiOutput("stack_display")
    )
  ),
  
  tags$script(HTML("
    // Store current text and theme
    let currentText = '';
    let currentTheme = '';
    
    // Update current values when input changes
    $(document).on('input', '#task_input', function() {
      currentText = $(this).val();
    });
    
    // Make the draggable brick draggable
    $(document).on('mouseenter', '#draggable-brick', function() {
      if (currentText.trim() !== '') {
        $(this).attr('draggable', 'true');
      }
    });
    
    $(document).on('dragstart', '#draggable-brick', function(e) {
      if (currentText.trim() === '') {
        e.preventDefault();
        return;
      }
      
      currentTheme = $(this).attr('data-theme');
      e.originalEvent.dataTransfer.effectAllowed = 'copy';
      e.originalEvent.dataTransfer.setData('text', currentText);
      e.originalEvent.dataTransfer.setData('theme', currentTheme);
      $(this).css('opacity', '0.5');
    });
    
    $(document).on('dragend', '#draggable-brick', function() {
      $(this).css('opacity', '1');
    });
    
    // Drop zone handlers
    $('#drop-zone').on('dragover', function(e) {
      e.preventDefault();
      e.stopPropagation();
      $(this).addClass('drag-over');
      e.originalEvent.dataTransfer.dropEffect = 'copy';
    });
    
    $('#drop-zone').on('dragleave', function(e) {
      e.preventDefault();
      e.stopPropagation();
      $(this).removeClass('drag-over');
    });
    
    $('#drop-zone').on('drop', function(e) {
      e.preventDefault();
      e.stopPropagation();
      $(this).removeClass('drag-over');
      
      const text = e.originalEvent.dataTransfer.getData('text');
      const theme = e.originalEvent.dataTransfer.getData('theme');
      
      if (text && theme) {
        Shiny.setInputValue('dropped_brick', {
          text: text,
          theme: theme,
          timestamp: Date.now()
        }, {priority: 'event'});
        
        // Clear the input
        $('#task_input').val('');
        $('#task_input').trigger('change');
      }
    });
  "))
)

server <- function(input, output, session) {
  # Reactive value to store the stack
  stack <- reactiveVal(list())
  
  # Reactive value for current classification
  current_classification <- reactive({
    if (nchar(input$task_input) > 0) {
      classify_text(input$task_input)
    } else {
      "extraction"
    }
  })
  
  # Update input border color based on classification
  observe({
    theme <- current_classification()
    color <- switch(theme,
                   extraction = "#2196F3",
                   prompt = "#4CAF50",
                   formatting = "#FFC107",
                   "#ccc")
    
    runjs(sprintf("$('#task_input').css('border-color', '%s');", color))
  })
  
  # Display current theme
  output$current_theme <- renderText({
    theme <- current_classification()
    switch(theme,
           extraction = "Extraction",
           prompt = "Prompt",
           formatting = "Formatting")
  })
  
  # Create draggable brick
  output$draggable_brick <- renderUI({
    if (nchar(trimws(input$task_input)) > 0) {
      theme <- current_classification()
      div(
        id = "draggable-brick",
        class = paste("draggable-brick", theme),
        `data-theme` = theme,
        input$task_input
      )
    } else {
      div(
        id = "draggable-brick",
        class = "draggable-brick extraction",
        `data-theme` = "extraction",
        style = "opacity: 0.3; cursor: not-allowed;",
        "Type a task above..."
      )
    }
  })
  
  # Handle dropped brick
  observeEvent(input$dropped_brick, {
    if (!is.null(input$dropped_brick$text) && nchar(trimws(input$dropped_brick$text)) > 0) {
      current_stack <- stack()
      new_block <- list(
        id = input$dropped_brick$timestamp,
        text = input$dropped_brick$text,
        theme = input$dropped_brick$theme
      )
      stack(c(current_stack, list(new_block)))
    }
  })
  
  # Clear stack
  observeEvent(input$clear, {
    stack(list())
  })
  
  # Display stack
  output$stack_display <- renderUI({
    current_stack <- stack()
    
    if (length(current_stack) == 0) {
      return(div(class = "empty-message", "Drag tasks here to build your stack"))
    }
    
    lapply(current_stack, function(block) {
      div(class = paste("brick", block$theme),
        block$text
      )
    })
  })
  
  # Stack count
  output$stack_count <- renderText({
    count <- length(stack())
    sprintf("Task Stack (%d %s)", count, ifelse(count == 1, "task", "tasks"))
  })
  
  # Check if there are blocks
  output$has_blocks <- reactive({
    length(stack()) > 0
  })
  outputOptions(output, "has_blocks", suspendWhenHidden = FALSE)
}

shinyApp(ui = ui, server = server)
