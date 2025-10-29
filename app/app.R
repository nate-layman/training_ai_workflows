# training.R
library(shiny)
library(shinyjs)
library(glue)

#------------------------------------------
# Pre-computed GloVe embeddings for semantic similarity
#------------------------------------------
# Generated using generate_glove_embeddings.R
# Contains 10k most common words + pre-computed category embeddings

# Load embeddings
embeddings_data <- readRDS('glove_embeddings.rds')
word_embeddings <- embeddings_data$word_embeddings
category_embeddings <- embeddings_data$category_embeddings

# Load action verbs from curated list
action_words_lines <- readLines('action_words.txt')
action_words_lines <- action_words_lines[!grepl("^#", action_words_lines) & action_words_lines != ""]
action_verb_list <- tolower(trimws(sapply(strsplit(action_words_lines, "\\|"), `[`, 1)))
action_verb_list <- unique(action_verb_list[action_verb_list != ""])

#------------------------------------------
# Find closest word in vocabulary
#------------------------------------------
# Uses character-level similarity when exact match not found
find_closest_word <- function(word, vocabulary) {
  if (word %in% vocabulary) return(word)

  # Calculate simple character overlap score
  scores <- sapply(vocabulary, function(v) {
    word_chars <- strsplit(word, "")[[1]]
    v_chars <- strsplit(v, "")[[1]]

    # Jaccard similarity: intersection / union
    intersection <- sum(word_chars %in% v_chars)
    union <- length(unique(c(word_chars, v_chars)))

    # Also consider length similarity
    len_diff <- abs(nchar(word) - nchar(v))

    # Combined score
    intersection / union - (len_diff * 0.1)
  })

  # Return word with highest score
  vocabulary[which.max(scores)]
}

#------------------------------------------
# Document embedding function with fallback
#------------------------------------------
# Converts text to embedding by averaging word vectors
# Falls back to closest word match if exact word not found
# Excludes filler words that don't carry semantic meaning
# Weights verbs (action words) more heavily than nouns
doc_embed <- function(text, embeddings = word_embeddings) {
  # Tokenize and clean
  words <- tolower(text)
  words <- gsub("[^a-z0-9 ]", " ", words)
  words <- strsplit(words, "\\s+")[[1]]
  words <- words[words != ""]

  if (length(words) == 0) {
    return(rep(0, ncol(embeddings)))
  }

  # Exclude filler words that don't carry semantic meaning
  filler_words <- c("the", "a", "an", "and", "or", "but", "in", "on", "at", "to", "for",
                    "of", "with", "by", "from", "as", "is", "was", "are", "were", "be",
                    "been", "being", "have", "has", "had", "do", "does", "did", "will",
                    "would", "should", "could", "may", "might", "must", "can", "shall",
                    "this", "that", "these", "those", "i", "you", "he", "she", "it",
                    "we", "they", "them", "their", "my", "your", "his", "her", "its",
                    "our", "what", "which", "who", "when", "where", "why", "how")

  # Remove filler words from user input
  words <- words[!(words %in% filler_words)]

  if (length(words) == 0) {
    return(rep(0, ncol(embeddings)))
  }

  # Match each word to vocabulary (exact or closest)
  vocab <- rownames(embeddings)
  matched_words <- sapply(words, function(w) {
    if (w %in% vocab) {
      w
    } else {
      # Find closest match for unknown words
      find_closest_word(w, vocab)
    }
  })

  # Build weighted embedding
  # Action verbs: 4x weight (verbs dominate over nouns)
  # Everything else: 1x weight
  word_vecs <- matrix(0, nrow = 0, ncol = ncol(embeddings))

  for (i in seq_along(matched_words)) {
    word <- matched_words[i]
    if (word %in% rownames(embeddings)) {
      vec <- embeddings[word, ]

      # Determine weight
      weight <- 1
      if (word %in% action_verb_list) {
        weight <- 5  # 5x weight for action verbs
      }

      # Repeat vector 'weight' times to increase its influence
      for (j in 1:weight) {
        word_vecs <- rbind(word_vecs, vec)
      }
    }
  }

  if (nrow(word_vecs) == 0) {
    return(rep(0, ncol(embeddings)))
  }

  # Average the weighted vectors
  colMeans(word_vecs)
}

#------------------------------------------
# Cosine similarity function
#------------------------------------------
cosine_similarity <- function(vec1, vec2) {
  dot_product <- sum(vec1 * vec2)
  norm1 <- sqrt(sum(vec1^2))
  norm2 <- sqrt(sum(vec2^2))

  if (norm1 == 0 || norm2 == 0) return(0)

  dot_product / (norm1 * norm2)
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
      // Show welcome modal on page load
      document.addEventListener('DOMContentLoaded', function() {
        document.getElementById('welcome_modal').style.display = 'block';

        Shiny.addCustomMessageHandler('setupDragDrop', function(message) {
          setupDragAndDrop();
        });
      });

      var dropHandlerAdded = false;
      var poolDropHandlerAdded = false;

      function setupDragAndDrop() {
        try {
          const brickPool = document.getElementById('brick_pool');
          const stackArea = document.getElementById('brick_stack');

          if (!brickPool || !stackArea) return;

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

        // Setup drag for ALL bricks in stack
        function setupStackDrag() {
          const stackBricks = document.querySelectorAll('.stack-area .brick');
          stackBricks.forEach(function(brick) {
            brick.style.cursor = 'move';
            brick.setAttribute('draggable', 'true');
            brick.addEventListener('dragstart', function(e) {
              e.dataTransfer.effectAllowed = 'move';
              e.dataTransfer.setData('brick-number', brick.getAttribute('data-number'));
              e.dataTransfer.setData('source', 'stack');
            });
          });
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
        } catch(e) {
          console.error('Error in setupDragAndDrop:', e);
        }
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
        if (e.target.id === 'close_welcome' ||
            e.target.id === 'welcome_modal') {
          document.getElementById('welcome_modal').style.display = 'none';
        }
        if (e.target.id === 'info_button') {
          document.getElementById('info_modal').style.display = 'block';
        }
        if (e.target.id === 'close_info' ||
            e.target.id === 'info_modal') {
          document.getElementById('info_modal').style.display = 'none';
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

      .brick-symbol {{
        position: absolute;
        font-size: 32px;
        font-weight: bold;
        color: white;
      }}

      .brick-symbol-2, .brick-symbol-5 {{
        font-size: 24px;
      }}

      .brick-symbol-3 {{
        transform: translateY(-2px);
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
        content: '';
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
        overflow-y: auto;
        padding: 40px 20px;
      }}

      .modal-content {{
        background-color: white;
        margin: auto;
        padding: 30px;
        border-radius: 10px;
        box-shadow: 0 4px 16px rgba(0,0,0,0.2);
        width: 500px;
        max-width: 90%;
        max-height: 85vh;
        overflow-y: auto;
      }}

      .modal-welcome {{
        width: 800px;
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
          div(class = "empty-message", "Add descriptions and drag bricks here to build your workflow!")
        )
      ),

      div(class = "right-side",
        h3("Task Bricks"),
        div(class = "brick-pool", id = "brick_pool",
          div(class = "empty-message", "Loading bricks...")
        )
      )
    ),

    div(style = "text-align: center; margin-top: 30px;",
      tags$button(id = "info_button", style = "padding: 10px 20px; background-color: #f0f0f0; border: 1px solid #ddd; border-radius: 5px; cursor: pointer; font-size: 14px; color: #666; font-family: Arial, sans-serif;", "More Info")
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

  # Welcome modal
  tags$div(id = "welcome_modal", class = "modal",
    tags$div(class = "modal-content modal-welcome",
      h3("Welcome to AI Workflow Builder"),
      tags$div(style = "font-size: 14px; line-height: 1.7; color: #666;",
        tags$p("Build research administration workflows by combining three types of AI task bricks!"),
        tags$div(style = "margin: 25px 0;",
          tags$p(
            tags$span(style = "color: #2196F3; font-weight: bold;", "Extraction:"),
            " Data retrieval and gathering.",
            style = "margin: 12px 0;"
          ),
          tags$p(
            tags$span(style = "color: #4CAF50; font-weight: bold;", "Transformation:"),
            " Processing and analysis.",
            style = "margin: 12px 0;"
          ),
          tags$p(
            tags$span(style = "color: #FFC107; font-weight: bold;", "Formatting:"),
            " Format output.",
            style = "margin: 12px 0;"
          )
        ),

        tags$hr(style = "margin: 20px 0; border: none; border-top: 1px solid #ddd;"),

        tags$p(
          tags$strong("1. Click a brick"),
          " and describe each task. They will be categorized automatically.",
          style = "margin: 8px 0;"
        ),
        tags$p(
          tags$strong("2. Drag tasks"),
          " to assemble a workflow",
          style = "margin: 8px 0;"
        ),

        tags$p(
          "Click ",
          tags$span(style = "font-weight: bold;", "More Info"),
          " below for more information.",
          tags$br(),
          style = "margin: 15px 0 0 0; font-size: 13px; color: #999;"
        )
      ),
      tags$button(id = "close_welcome", style = "width: 100%; padding: 12px; background-color: #2196F3; color: white; border: none; border-radius: 5px; font-size: 16px; cursor: pointer; margin-top: 25px;", "Got it!")
    )
  ),

  # More Info modal
  tags$div(id = "info_modal", class = "modal",
    tags$div(class = "modal-content modal-welcome",
      h3("How It Works"),
      tags$div(style = "font-size: 14px; line-height: 1.7; color: #666;",
        tags$div(style = "margin: 20px 0;",
          tags$h4(style = "color: #333; margin: 0 0 10px 0; font-weight: bold;", "Why Repeatable Workflows Matter"),
          tags$p("Consistent, documented workflows reduce errors, save time, and make processes easier to share and improve over time.",
                 style = "margin: 0; color: #777; font-size: 13px;")
        ),
        tags$div(style = "margin: 20px 0;",
          tags$h4(style = "color: #333; margin: 0 0 10px 0; font-weight: bold;", "Breaking Down into Discrete Steps"),
          tags$p("Complex tasks become manageable when broken into small, focused steps. Each step does one thing well, making workflows flexible and reusable.",
                 style = "margin: 0; color: #777; font-size: 13px;")
        ),
        tags$div(style = "margin: 20px 0;",
          tags$h4(style = "color: #333; margin: 0 0 10px 0; font-weight: bold;", "The Three Task Types"),
          tags$p(
            tags$span(style = "color: #2196F3; font-weight: bold;", "Extraction:"),
            " Data retrieval and gathering.",
            tags$br(),
            tags$span(style = "color: #4CAF50; font-weight: bold;", "Transformation:"),
            " Processing and analysis.",
            tags$br(),
            tags$span(style = "color: #FFC107; font-weight: bold;", "Formatting:"),
            " Format output.",
            style = "margin: 0; color: #777; font-size: 13px;"
          )
        ),
        tags$div(style = "margin: 20px 0;",
          tags$h4(style = "color: #333; margin: 0 0 10px 0; font-weight: bold;", "Task Classification"),
          tags$p("We use AI-powered semantic matching with GloVe embeddings—a machine learning approach that understands meaning. Your task description is converted into a numerical pattern and compared against extraction, analysis, and formatting patterns. The system finds the closest match and automatically colors your brick accordingly.",
                 style = "margin: 0 0 8px 0; color: #777; font-size: 13px;"),
          tags$p(tags$strong("Note:"), " The categorization method isn't perfect. We prioritized speed in this demonstration, so some tasks may be miscategorized. You can always click a brick to correct its classification.",
                 style = "margin: 0 0 8px 0; color: #d9534f; font-size: 13px;"),
          tags$p(tags$em("Question: What type of AI task would categorizing tasks be?"),
                 style = "margin: 0; color: #999; font-size: 12px; font-style: italic;")
        ),
        tags$div(style = "margin: 20px 0;",
          tags$h4(style = "color: #333; margin: 0 0 10px 0; font-weight: bold;", "Workflow Order"),
          tags$p(tags$em("Question: Do you need to perform your tasks in a specific order? Or can all tasks be performed at once?"),
                 style = "margin: 0; color: #999; font-size: 12px; font-style: italic;")
        ),
        tags$div(style = "margin: 20px 0;",
          tags$h4(style = "color: #333; margin: 0 0 10px 0; font-weight: bold;", "Breaking Down Tasks"),
          tags$p(
            "A good workflow chains tasks together, each building on the previous:",
            tags$br(),
            tags$em("Extract", style = "color: #2196F3; font-weight: bold;"), " → ",
            tags$em("Transform", style = "color: #4CAF50; font-weight: bold;"), " → ",
            tags$em("Format", style = "color: #FFC107; font-weight: bold;"),
            style = "margin: 0 0 12px 0; color: #777; font-size: 13px;"
          ),
          tags$p(
            tags$strong("Find the right balance:"), " Too few tasks makes them hard to reuse. Too many becomes unwieldy. Each task should have one clear purpose.",
            style = "margin: 0 0 12px 0; color: #777; font-size: 13px;"
          ),
          tags$p(
            tags$strong("Avoid mixing concepts:"), " Don't combine multiple ideas in one task. Each brick should do one thing well.",
            style = "margin: 0 0 12px 0; color: #777; font-size: 13px;"
          ),
          tags$p(
            tags$strong("Think about dependencies:"), " Each task should ideally use output from the previous one. Avoid tasks that depend on multiple others.",
            style = "margin: 0 0 12px 0; color: #777; font-size: 13px;"
          ),
          tags$p(
            tags$em("Questions to ask yourself:"),
            tags$br(),
            "• Can this task be completed without the previous task?",
            tags$br(),
            "• Would you reuse this task in a different workflow?",
            tags$br(),
            "• Can someone else understand what this task does by its description?",
            style = "margin: 0; color: #999; font-size: 12px; font-style: italic;"
          )
        )
      ),
      tags$button(id = "close_info", style = "width: 100%; padding: 12px; background-color: #2196F3; color: white; border: none; border-radius: 5px; font-size: 16px; cursor: pointer;", "Close")
    )
  ),

  # Status bar
  tags$div(id = "status_bar", "Hover over a brick to see its details")
)

#------------------------------------------
# SERVER
#------------------------------------------
server <- function(input, output, session) {
  # Map brick numbers to symbols
  get_brick_symbol <- function(number) {
    symbols <- c("●", "▲", "■", "◆", "★")
    symbols[number]
  }

  pool_bricks <- reactiveVal(list(
    list(number = 1, type = "grey", text = "", id = "1"),
    list(number = 2, type = "grey", text = "", id = "2"),
    list(number = 3, type = "grey", text = "", id = "3"),
    list(number = 4, type = "grey", text = "", id = "4"),
    list(number = 5, type = "grey", text = "", id = "5")
  ))
  stack_bricks <- reactiveVal(list())
  editing_brick <- reactiveVal(NULL)

  # Semantic matching using GloVe embeddings
  detect_brick_type <- function(text) {
    if (nchar(text) == 0) return("grey")

    # Create embedding for user input using GloVe
    user_vec <- doc_embed(text)

    # Calculate cosine similarity with each category embedding
    similarities <- c(
      extraction = cosine_similarity(user_vec, category_embeddings$extraction),
      prompt = cosine_similarity(user_vec, category_embeddings$prompt),
      formatting = cosine_similarity(user_vec, category_embeddings$formatting)
    )

    # Return category with highest similarity
    # If all similarities are 0 (no words found in GloVe), default to prompt
    if (all(similarities == 0)) {
      return("prompt")
    }

    return(names(which.max(similarities)))
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

  # Render pool bricks - show bricks in pool and placeholders for bricks in stack
  observe({
    pb <- pool_bricks()
    sb <- stack_bricks()

    # Get all brick numbers that are in the stack
    stack_brick_numbers <- sapply(sb, function(b) b$number)

    # Create a list of 5 slots, some may be empty (placeholder)
    html <- paste0(
      sapply(1:5, function(slot_num) {
        # Find brick with this number in pool
        if (length(pb) == 0) {
          brick_idx <- integer(0)
        } else {
          brick_idx <- which(sapply(pb, function(b) b$number == slot_num))
        }

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

          symbol <- get_brick_symbol(brick$number)
          sprintf("<div class='draggable-brick %s' data-number='%d' data-tooltip='%s' data-symbol='%s'><span class='brick-symbol brick-symbol-%d'>%s</span></div>",
                  brick$type,
                  brick$number,
                  gsub("'", "&apos;", tooltip),
                  symbol,
                  brick$number,
                  symbol)
        } else if (slot_num %in% stack_brick_numbers) {
          # Brick is in tower - render placeholder (number shown via ::after)
          sprintf("<div class='brick-placeholder' data-number='%d'></div>", slot_num)
        } else {
          # Brick doesn't exist - render nothing
          ""
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

  # Handle brick dropped to pool (from anywhere in stack)
  observeEvent(input$dropped_to_pool, {
    brick_num <- input$dropped_to_pool$number

    # Find and remove the brick from stack by its number
    sb <- stack_bricks()
    if (length(sb) == 0) {
      brick_idx <- integer(0)
    } else {
      brick_idx <- which(sapply(sb, function(b) b$number == brick_num))
    }

    if (length(brick_idx) > 0) {
      removed_brick <- sb[[brick_idx]]

      # Remove from stack
      stack_bricks(sb[-brick_idx])

      # Add back to pool (each brick returns to its numbered slot)
      pool_bricks(c(pool_bricks(), list(removed_brick)))
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
          symbol <- get_brick_symbol(sb[[idx]]$number)

          sprintf("<div class='%s' data-number='%d' data-tooltip='%s' data-symbol='%s'><span class='brick-symbol brick-symbol-%d'>%s</span></div>",
                  classes,
                  sb[[idx]]$number,
                  gsub("'", "&apos;", tooltip),
                  symbol,
                  sb[[idx]]$number,
                  symbol)
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
