# training.R
library(shiny)
library(shinyjs)
library(glue)

# Load text2vec if available
if (requireNamespace("text2vec", quietly = TRUE)) {
  library(text2vec)
  text2vec_available <- TRUE
  cat("text2vec library loaded at startup\n")
} else {
  text2vec_available <- FALSE
  cat("text2vec not available at startup\n")
}

#------------------------------------------
# SVG brick generator
#------------------------------------------
lego_svg <- function(color) {
  svg <- sprintf("
    <svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 90 90'>
      <rect x='5.706' y='20.957' width='10.908' height='22.076' rx='2.767' fill='%s'/>
      <rect x='28.546' y='20.957' width='10.908' height='22.076' rx='2.767' fill='%s'/>
      <rect x='51.386' y='20.957' width='10.908' height='22.076' rx='2.767' fill='%s'/>
      <rect x='74.226' y='20.957' width='10.908' height='22.076' rx='2.767' fill='%s'/>
      <rect x='0.5' y='30.234' width='89' height='46.503' rx='2.905' fill='%s'/>
    </svg>", color, color, color, color, color)
  paste0("data:image/svg+xml;utf8,", URLencode(svg, reserved = TRUE))
}

#------------------------------------------
# Base plate SVG (horizontal plate with studs)
#------------------------------------------
base_plate_svg <- function() {
  # Create a horizontal grey plate with multiple studs on top
  # Similar proportions to brick but wider and with more studs
  svg <- sprintf("
    <svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 360 60'>
      <rect x='5.706' y='5' width='10.908' height='22.076' rx='2.767' fill='#666666'/>
      <rect x='28.546' y='5' width='10.908' height='22.076' rx='2.767' fill='#666666'/>
      <rect x='51.386' y='5' width='10.908' height='22.076' rx='2.767' fill='#666666'/>
      <rect x='74.226' y='5' width='10.908' height='22.076' rx='2.767' fill='#666666'/>
      <rect x='97.066' y='5' width='10.908' height='22.076' rx='2.767' fill='#666666'/>
      <rect x='119.906' y='5' width='10.908' height='22.076' rx='2.767' fill='#666666'/>
      <rect x='142.746' y='5' width='10.908' height='22.076' rx='2.767' fill='#666666'/>
      <rect x='165.586' y='5' width='10.908' height='22.076' rx='2.767' fill='#666666'/>
      <rect x='188.426' y='5' width='10.908' height='22.076' rx='2.767' fill='#666666'/>
      <rect x='211.266' y='5' width='10.908' height='22.076' rx='2.767' fill='#666666'/>
      <rect x='234.106' y='5' width='10.908' height='22.076' rx='2.767' fill='#666666'/>
      <rect x='256.946' y='5' width='10.908' height='22.076' rx='2.767' fill='#666666'/>
      <rect x='279.786' y='5' width='10.908' height='22.076' rx='2.767' fill='#666666'/>
      <rect x='302.626' y='5' width='10.908' height='22.076' rx='2.767' fill='#666666'/>
      <rect x='325.466' y='5' width='10.908' height='22.076' rx='2.767' fill='#666666'/>
      <rect x='348.306' y='5' width='10.908' height='22.076' rx='2.767' fill='#666666'/>
      <rect x='2' y='14.234' width='356' height='35' rx='2.905' fill='#666666'/>
    </svg>")
  paste0("data:image/svg+xml;utf8,", URLencode(svg, reserved = TRUE))
}

#------------------------------------------
# UI
#------------------------------------------
ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$script(HTML("
      // Drag and drop functionality
      document.addEventListener('DOMContentLoaded', function() {
        Shiny.addCustomMessageHandler('setupDragDrop', function(message) {
          setupDragAndDrop();
        });
      });

      var dropHandlerAdded = false;
      var poolDropHandlerAdded = false;

      function setupDragAndDrop() {
        const brickPool = document.getElementById('brick_pool');
        const stackArea = document.getElementById('brick_stack');

        // Setup drag for colored bricks in pool only
        function setupBrickDrag() {
          const bricks = document.querySelectorAll('.brick-pool .draggable-brick:not(.grey)');
          bricks.forEach(function(brick) {
            brick.style.cursor = 'move';
            brick.setAttribute('draggable', 'true');
            brick.addEventListener('dragstart', function(e) {
              e.dataTransfer.effectAllowed = 'move';
              e.dataTransfer.setData('brick-number', brick.getAttribute('data-number'));
              e.dataTransfer.setData('source', 'pool');
            });
          });
        }

        // Setup click handlers for all bricks (pool and stack)
        function setupBrickClick() {
          // Handle pool bricks (only draggable-brick, not placeholders)
          const poolBricks = document.querySelectorAll('.brick-pool .draggable-brick');
          poolBricks.forEach(function(brick) {
            // Only add click handler if it has the draggable-brick class (not brick-placeholder)
            if (!brick.classList.contains('brick-placeholder')) {
              brick.style.cursor = 'pointer';
              brick.addEventListener('click', function(e) {
                const number = brick.getAttribute('data-number');
                Shiny.setInputValue('clicked_brick', {number: parseInt(number), timestamp: Date.now()}, {priority: 'event'});
              });
            }
          });

          // Handle stack bricks (in the tower)
          const stackBricks = document.querySelectorAll('.stack-area .brick');
          stackBricks.forEach(function(brick) {
            brick.style.cursor = 'pointer';
            brick.addEventListener('click', function(e) {
              const number = brick.getAttribute('data-number');
              Shiny.setInputValue('clicked_brick', {number: parseInt(number), timestamp: Date.now()}, {priority: 'event'});
            });
          });
        }

        // Setup drag for top brick in stack (last one in DOM due to flex-reverse)
        function setupStackDrag() {
          const stackBricks = document.querySelectorAll('.stack-area .brick');
          if (stackBricks.length > 0) {
            const topBrick = stackBricks[stackBricks.length - 1]; // Last brick is visually on top
            topBrick.style.cursor = 'move';
            topBrick.setAttribute('draggable', 'true');
            topBrick.addEventListener('dragstart', function(e) {
              e.dataTransfer.effectAllowed = 'move';
              e.dataTransfer.setData('brick-number', topBrick.getAttribute('data-number'));
              e.dataTransfer.setData('source', 'stack');
            });
          }
        }

        // Only add drop handler to stack once
        if (stackArea && !dropHandlerAdded) {
          dropHandlerAdded = true;

          stackArea.addEventListener('dragover', function(e) {
            e.preventDefault();
            e.dataTransfer.dropEffect = 'move';
            this.classList.add('drag-over');
          });

          stackArea.addEventListener('dragleave', function(e) {
            this.classList.remove('drag-over');
          });

          stackArea.addEventListener('drop', function(e) {
            e.preventDefault();
            this.classList.remove('drag-over');
            const number = e.dataTransfer.getData('brick-number');
            const source = e.dataTransfer.getData('source');
            if (number && source === 'pool') {
              Shiny.setInputValue('dropped_to_stack', {number: parseInt(number), timestamp: Date.now()}, {priority: 'event'});
            }
          });
        }

        // Only add drop handler to pool once
        if (brickPool && !poolDropHandlerAdded) {
          poolDropHandlerAdded = true;

          brickPool.addEventListener('dragover', function(e) {
            e.preventDefault();
            e.dataTransfer.dropEffect = 'move';
            this.classList.add('drag-over');
          });

          brickPool.addEventListener('dragleave', function(e) {
            this.classList.remove('drag-over');
          });

          brickPool.addEventListener('drop', function(e) {
            e.preventDefault();
            this.classList.remove('drag-over');
            const number = e.dataTransfer.getData('brick-number');
            const source = e.dataTransfer.getData('source');
            if (number && source === 'stack') {
              Shiny.setInputValue('dropped_to_pool', {number: parseInt(number), timestamp: Date.now()}, {priority: 'event'});
            }
          });
        }

        setupBrickDrag();
        setupBrickClick();
        setupStackDrag();
        setupHoverListeners();
      }

      function setupHoverListeners() {
        const allBricks = document.querySelectorAll('.draggable-brick, .brick');
        const statusBar = document.getElementById('status_bar');

        allBricks.forEach(function(brick) {
          brick.addEventListener('mouseenter', function() {
            const tooltip = brick.getAttribute('data-tooltip');
            if (tooltip && statusBar) {
              statusBar.textContent = tooltip;

              // Change color based on brick type
              const classList = brick.classList;
              if (classList.contains('extraction')) {
                statusBar.style.backgroundColor = '#2196F3';
              } else if (classList.contains('prompt')) {
                statusBar.style.backgroundColor = '#4CAF50';
              } else if (classList.contains('formatting')) {
                statusBar.style.backgroundColor = '#FFC107';
                statusBar.style.color = '#333';
              } else {
                statusBar.style.backgroundColor = '#999999';
                statusBar.style.color = 'white';
              }
            }
          });

          brick.addEventListener('mouseleave', function() {
            if (statusBar) {
              statusBar.style.backgroundColor = '#333';
              statusBar.style.color = 'white';
              statusBar.textContent = 'Hover over a brick to see its details';
            }
          });
        });
      }

      // Modal functions
      function showModal(number) {
        document.getElementById('brick_modal').style.display = 'block';
      }

      function closeModal() {
        document.getElementById('brick_modal').style.display = 'none';
      }

      Shiny.addCustomMessageHandler('showModal', function(message) {
        showModal();
      });

      Shiny.addCustomMessageHandler('closeModal', function(message) {
        closeModal();
      });

      document.addEventListener('click', function(e) {
        if (e.target.id === 'brick_modal') {
          closeModal();
        }
        if (e.target.id === 'modal_update_brick') {
          Shiny.setInputValue('save_brick_click', {timestamp: Date.now()}, {priority: 'event'});
        }
      });
    ")),
    tags$style(HTML(
      glue("
      body {{
        background-color: #f5f5f5;
        font-family: Arial, sans-serif;
        user-select: none;
        -webkit-user-select: none;
        -moz-user-select: none;
        -ms-user-select: none;
      }}

      .container-fluid {{
        max-width: 1200px;
        margin: 0 auto;
        padding: 20px;
      }}

      .main-layout {{
        display: flex;
        gap: 20px;
        align-items: flex-start;
      }}

      .left-side {{
        flex: 1;
      }}

      .right-side {{
        flex: 1;
      }}

      h2 {{
        text-align: center;
        color: #333;
        margin-bottom: 30px;
      }}

      h3 {{
        color: #333;
        margin-bottom: 15px;
        text-align: center;
      }}

      .draggable-brick, .brick {{
        width: 180px;
        height: 80px;
        margin: 10px;
        display: flex;
        align-items: center;
        justify-content: center;
        text-align: center;
        color: white;
        font-weight: bold;
        font-size: 24px;
        background-size: contain;
        background-repeat: no-repeat;
        background-position: center;
        border: none;
        position: relative;
        transition: transform 0.1s;
        line-height: 80px;
        padding: 0;
        padding-top: 15px;
      }}

      .draggable-brick.grey {{
        background-image: url('{lego_svg(\"#999999\")}');
      }}

      .draggable-brick:not(.grey):hover, .brick:hover {{
        transform: scale(1.05);
      }}


      .draggable-brick.grey:hover::after, .brick.grey:hover::after {{
        background: rgba(153, 153, 153, 0.85);
        color: white;
      }}

      .draggable-brick.extraction:hover::after, .brick.extraction:hover::after {{
        background: rgba(33, 150, 243, 0.85);
        color: white;
      }}

      .draggable-brick.prompt:hover::after, .brick.prompt:hover::after {{
        background: rgba(76, 175, 80, 0.85);
        color: white;
      }}

      .draggable-brick.formatting:hover::after, .brick.formatting:hover::after {{
        background: rgba(255, 193, 7, 0.85);
        color: black;
      }}

      .draggable-brick.extraction, .brick.extraction {{
        background-image: url('{lego_svg(\"#2196F3\")}');
      }}

      .draggable-brick.prompt, .brick.prompt {{
        background-image: url('{lego_svg(\"#4CAF50\")}');
      }}

      .draggable-brick.formatting, .brick.formatting {{
        background-image: url('{lego_svg(\"#FFC107\")}');
      }}

      .stack-area {{
        background: white;
        border-radius: 10px;
        padding: 20px;
        height: 350px;
        box-shadow: 0 2px 8px rgba(0,0,0,0.1);
        display: flex;
        flex-direction: column-reverse;
        align-items: center;
        position: relative;
      }}

      .base-plate {{
        width: 320px;
        height: 40px;
        background-size: contain;
        background-repeat: no-repeat;
        background-position: center;
        position: absolute;
        bottom: 20px;
        left: 50%;
        transform: translateX(-50%);
        z-index: 0;
      }}

      .stack-area .brick {{
        display: flex;
        height: 80px;
        z-index: 1;
      }}

      .stack-area .brick:not(:last-child) {{
        margin-top: -52px;
      }}

      .stack-area .brick:last-child {{
        z-index: 100;
      }}

      .stack-area.drag-over {{
        border-color: #4CAF50;
        background-color: #f0f8f0;
      }}

      .brick-pool {{
        background: white;
        border-radius: 10px;
        padding: 20px;
        height: 350px;
        box-shadow: 0 2px 8px rgba(0,0,0,0.1);
        display: flex;
        flex-wrap: wrap;
        align-items: flex-start;
        align-content: flex-start;
        justify-content: center;
      }}

      .brick-pool .empty-message {{
        padding: 30px;
      }}

      .brick-placeholder {{
        width: 180px;
        height: 80px;
        background-size: contain;
        background-repeat: no-repeat;
        background-position: center;
        background-image: url('{lego_svg(\"#cccccc\")}');
        opacity: 0.3;
        margin: 10px;
        cursor: default;
        display: flex;
        align-items: center;
        justify-content: center;
        text-align: center;
        color: #999999;
        font-weight: bold;
        font-size: 24px;
        line-height: 80px;
        padding-top: 15px;
        position: relative;
      }}

      .brick-placeholder::after {{
        content: attr(data-number);
        font-size: 24px;
        font-weight: bold;
        color: #999999;
      }}

      .brick-pool.drag-over {{
        background-color: #f0f8f0;
      }}

      .empty-message {{
        text-align: center;
        color: #999;
        padding: 50px;
        font-size: 18px;
      }}

      #status_bar {{
        position: fixed;
        bottom: 0;
        left: 0;
        right: 0;
        background-color: #333;
        color: white;
        padding: 15px 20px;
        font-size: 16px;
        z-index: 9999;
        display: block;
        box-shadow: 0 -2px 10px rgba(0,0,0,0.3);
        transition: background-color 0.2s ease;
      }}

      /* Modal styles */
      .modal {{
        display: none;
        position: fixed;
        z-index: 10000;
        left: 0;
        top: 0;
        width: 100%;
        height: 100%;
        background-color: rgba(0,0,0,0.4);
      }}

      .modal-content {{
        background-color: white;
        margin: 10% auto;
        padding: 30px;
        border-radius: 10px;
        box-shadow: 0 4px 16px rgba(0,0,0,0.2);
        width: 500px;
        max-width: 90%;
      }}


      .modal-update-brick {{
        width: 180px;
        height: 80px;
        margin: 20px auto;
        display: flex;
        align-items: center;
        justify-content: center;
        color: white;
        font-weight: bold;
        font-size: 20px;
        background-size: contain;
        background-repeat: no-repeat;
        background-position: center;
        line-height: 80px;
        padding: 0;
        padding-top: 15px;
        cursor: pointer;
        transition: transform 0.1s;
        border: none;
        background-color: transparent;
      }}

      .modal-update-brick:hover {{
        transform: scale(1.05);
      }}

      .modal-update-brick:active {{
        transform: scale(0.95);
      }}

      .modal-update-brick.grey {{
        background-image: url('{lego_svg(\"#999999\")}');
      }}

      .modal-update-brick.extraction {{
        background-image: url('{lego_svg(\"#2196F3\")}');
      }}

      .modal-update-brick.prompt {{
        background-image: url('{lego_svg(\"#4CAF50\")}');
      }}

      .modal-update-brick.formatting {{
        background-image: url('{lego_svg(\"#FFC107\")}');
      }}
      ")
    ))
  ),

  div(class = "container-fluid",
    h2("AI Workflow Builder"),

    div(class = "main-layout",
      div(class = "left-side",
        h3("Workflow Tower"),
        div(class = "stack-area", id = "brick_stack",
          div(class = "empty-message", "Drag bricks here to build your workflow.")
        )
      ),

      div(class = "right-side",
        h3("Task Bricks"),
        div(class = "brick-pool", id = "brick_pool",
          div(class = "empty-message", "Loading bricks...")
        )
      )
    )
  ),

  # Modal for editing bricks
  tags$div(id = "brick_modal", class = "modal",
    tags$div(class = "modal-content",
      h3("Configure Brick"),
      textAreaInput("modal_task_input", "AI Task Description:",
                   placeholder = "Describe what this brick should do...",
                   rows = 4, width = "100%"),
      tags$button(id = "modal_update_brick", class = "modal-update-brick grey", "Update")
    )
  ),

  # Status bar
  tags$div(id = "status_bar", "Hover over a brick to see its details")
)

#------------------------------------------
# Pre-compute text2vec vectors (outside server for global scope)
#------------------------------------------
category_vectorizer <- NULL
category_dtm <- NULL
using_text2vec <- FALSE

if (text2vec_available) {
  tryCatch({
    # Improved category descriptions focusing on core concepts and typical use cases
    category_docs <- c(
      extraction = "extract pull get fetch retrieve collect gather read find locate search discover identify data information content details value field element attribute property name names from source database api web file document scrape parse access obtain acquire mine harvest capture take grab pick select query lookup investigator pi",
      prompt = "summarize condense analyze examine study check verify validate test compare contrast classify categorize tag label understand comprehend reason think explain describe interpret translate evaluate judge assess appraise critique review process manipulate work edit refine revise synthesize combine merge integrate rewrite paraphrase simplify clarify enhance improve strengthen ensure compliance requirements logic strength quality llm ai model gpt assistant bot agent question query request instruction task tell instruct direct command",
      formatting = "generate create write produce compose draft author pen craft format display output present show render print arrange structure organize layout design style export save store convert transform serialize stringify encode json csv xml html email markdown table grid chart graph visualize prettify beautify clean normalize standardize package prepare finalize deliver spreadsheet report summary document letter memo acknowledgment narrative budget grant award timeline"
    )

    # Pre-compute vocabulary and vectorizer
    tokens <- itoken(category_docs, preprocessor = tolower, tokenizer = word_tokenizer)
    vocab <- create_vocabulary(tokens)
    category_vectorizer <- vocab_vectorizer(vocab)
    category_dtm <- create_dtm(tokens, category_vectorizer)
    using_text2vec <- TRUE
    cat("text2vec loaded successfully for semantic matching\n")
  }, error = function(e) {
    cat("text2vec failed to load:", e$message, "\n")
  })
} else {
  cat("text2vec not available\n")
}

#------------------------------------------
# SERVER
#------------------------------------------
server <- function(input, output, session) {
  pool_bricks <- reactiveVal(list(
    list(number = 1, type = "grey", text = "", id = "1"),
    list(number = 2, type = "grey", text = "", id = "2"),
    list(number = 3, type = "grey", text = "", id = "3"),
    list(number = 4, type = "grey", text = "", id = "4"),
    list(number = 5, type = "grey", text = "", id = "5")
  ))
  stack_bricks <- reactiveVal(list())
  editing_brick <- reactiveVal(NULL)

  # Optimized semantic matching function
  detect_brick_type <- function(text) {
    if (nchar(text) == 0) return("grey")

    text_lower <- tolower(text)

    # Use pre-computed text2vec vectors if available
    if (!is.null(category_vectorizer) && !is.null(category_dtm)) {
      tryCatch({
        # Create DTM for user input using pre-computed vectorizer
        user_tokens <- itoken(text_lower, preprocessor = tolower, tokenizer = word_tokenizer)
        user_dtm <- create_dtm(user_tokens, category_vectorizer)

        # Calculate cosine similarity (using raw term frequencies, not TF-IDF)
        similarities <- c(
          extraction = sim2(user_dtm, category_dtm[1, , drop = FALSE], method = "cosine")[1,1],
          prompt = sim2(user_dtm, category_dtm[2, , drop = FALSE], method = "cosine")[1,1],
          formatting = sim2(user_dtm, category_dtm[3, , drop = FALSE], method = "cosine")[1,1]
        )

        # Return category with highest similarity (always pick the best match)
        if (!all(is.na(similarities))) {
          return(names(which.max(similarities)))
        }
        # Only default to prompt if all similarities are NA
        return("prompt")
      }, error = function(e) {
        # If text2vec fails, log error and default to prompt
        cat("Error in semantic matching:", e$message, "\n")
        return("prompt")
      })
    }

    # If text2vec not available, default to prompt
    return("prompt")
  }

  # Setup drag and drop on load
  observe({
    session$sendCustomMessage("setupDragDrop", list())
  })

  # Handle brick click
  observeEvent(input$clicked_brick, {
    brick_num <- input$clicked_brick$number

    # Find brick in pool by number property
    pb <- pool_bricks()
    brick_idx <- which(sapply(pb, function(b) b$number == brick_num))

    if (length(brick_idx) > 0) {
      brick <- pb[[brick_idx]]
      editing_brick(brick_num)
      updateTextAreaInput(session, "modal_task_input", value = brick$text)

      # Update modal button
      runjs(sprintf("
        var btn = document.getElementById('modal_update_brick');
        btn.className = 'modal-update-brick %s';
      ", brick$type))

      session$sendCustomMessage("showModal", list())
    }
  })

  # Update modal button color as user types
  observeEvent(input$modal_task_input, {
    if (!is.null(editing_brick())) {
      new_text <- trimws(input$modal_task_input)
      new_type <- if (nchar(new_text) == 0) {
        "grey"
      } else {
        detect_brick_type(new_text)
      }

      runjs(sprintf("
        var btn = document.getElementById('modal_update_brick');
        btn.className = 'modal-update-brick %s';
      ", new_type))
    }
  })

  # Handle save brick
  observeEvent(input$save_brick_click, {
    brick_num <- editing_brick()
    if (!is.null(brick_num)) {
      new_text <- trimws(input$modal_task_input)

      # If empty, turn back to grey
      new_type <- if (nchar(new_text) == 0) {
        "grey"
      } else {
        detect_brick_type(new_text)
      }

      # Update brick in pool by finding it by number property
      pb <- pool_bricks()
      if (length(pb) > 0) {
        brick_idx <- which(sapply(pb, function(b) b$number == brick_num))
        if (length(brick_idx) > 0) {
          pb[[brick_idx]]$text <- new_text
          pb[[brick_idx]]$type <- new_type
          pool_bricks(pb)
        }
      }

      # Also check and update in stack
      sb <- stack_bricks()
      if (length(sb) > 0) {
        stack_idx <- which(sapply(sb, function(b) b$number == brick_num))
        if (length(stack_idx) > 0) {
          sb[[stack_idx]]$text <- new_text
          sb[[stack_idx]]$type <- new_type
          stack_bricks(sb)
        }
      }

      editing_brick(NULL)
      session$sendCustomMessage("closeModal", list())
    }
  })

  # Render pool bricks - always show 5 slots
  observe({
    pb <- pool_bricks()

    # Create a list of 5 slots, some may be empty (placeholder)
    html <- paste0(
      sapply(1:5, function(slot_num) {
        # Find brick with this number in pool
        brick_idx <- which(sapply(pb, function(b) b$number == slot_num))

        if (length(brick_idx) > 0) {
          # Brick exists in pool - render it
          brick <- pb[[brick_idx]]
          tooltip <- if (brick$text == "") {
            "Click to add task description"
          } else {
            type_name <- switch(brick$type,
                              extraction = "Extraction",
                              prompt = "Transformation",
                              formatting = "Formatting",
                              "Not configured")
            paste0(type_name, ": ", brick$text)
          }

          sprintf("<div class='draggable-brick %s' data-number='%d' data-tooltip='%s'></div>",
                  brick$type,
                  brick$number,
                  gsub("'", "&apos;", tooltip))
        } else {
          # Brick is in tower - render placeholder (number shown via ::after)
          sprintf("<div class='brick-placeholder' data-number='%d'></div>", slot_num)
        }
      }),
      collapse = ""
    )
    runjs(sprintf("document.getElementById('brick_pool').innerHTML = `%s`; setupDragAndDrop();", html))
  })

  # Handle brick dropped to stack (from pool)
  observeEvent(input$dropped_to_stack, {
    brick_num <- input$dropped_to_stack$number

    # Find brick in pool by number property
    pb <- pool_bricks()
    brick_idx <- which(sapply(pb, function(b) b$number == brick_num))

    if (length(brick_idx) > 0) {
      brick <- pb[[brick_idx]]

      # Only allow colored bricks (not grey)
      if (brick$type != "grey") {
        # Remove from pool
        pool_bricks(pb[-brick_idx])

        # Add to stack
        stack_bricks(c(list(brick), stack_bricks()))
      }
    }
  })

  # Handle brick dropped to pool (from stack top)
  observeEvent(input$dropped_to_pool, {
    brick_num <- input$dropped_to_pool$number

    # Remove top brick from stack
    sb <- stack_bricks()
    if (length(sb) > 0 && sb[[1]]$number == brick_num) {
      stack_bricks(sb[-1])

      # Add back to pool (each brick returns to its numbered slot)
      pool_bricks(c(pool_bricks(), list(sb[[1]])))
    }
  })

  # Render stack bricks
  observe({
    sb <- stack_bricks()

    if (length(sb) == 0) {
      html <- "<div class='empty-message'>Drag bricks here to build your workflow.</div>"
    } else {
      # Reverse order so newest (first in list) appears at top visually
      indices <- rev(seq_along(sb))
      html <- paste0(
        sapply(seq_along(indices), function(i) {
          idx <- indices[i]
          # First brick in list (sb[[1]]) is the top brick
          # It appears last in DOM due to reverse
          is_top <- (idx == 1)

          type_name <- switch(sb[[idx]]$type,
                            extraction = "Extraction",
                            prompt = "Transformation",
                            formatting = "Formatting",
                            "")
          tooltip <- paste0(type_name, ": ", sb[[idx]]$text)

          # All bricks use same classes now, overlapping handled by CSS
          classes <- paste(c("brick", sb[[idx]]$type), collapse = " ")

          sprintf("<div class='%s' data-number='%d' data-tooltip='%s'></div>",
                  classes,
                  sb[[idx]]$number,
                  gsub("'", "&apos;", tooltip))
        }),
        collapse = ""
      )
    }
    runjs(sprintf("document.getElementById('brick_stack').innerHTML = `%s`; setupDragAndDrop();", html))
  })

}

#------------------------------------------
# RUN APP
#------------------------------------------
shinyApp(ui, server)
