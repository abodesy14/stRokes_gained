# load libraries
{
  library(shiny)
  library(shinyWidgets)
  library(shinybusy)
  library(shinydashboard)
  library(DT)
  library(tidyverse)
  library(glue)
  library(gt)
  library(gtExtras)
  library(readxl)
  library(zoo)
  library(tidyr)
}

# expected strokes dataset
xStrokes <- read.csv("data/expected_strokes_dataset.csv") %>%
  select(-c(hdcp_4_exp:hdcp_20_exp))

# impute any gaps in data (thousands of distance/lie/handicap combinations)
xStrokes_filled <- xStrokes %>%
  group_by(start_surface) %>%
  arrange(ref_distance_value) %>%
  mutate(across(c(scratch_exp, pga_exp, avg_80_exp, avg_85_exp, avg_90_exp, avg_95_exp, avg_100_exp),
                ~ zoo::na.approx(.x, ref_distance_value, rule = 2))) %>%
  ungroup() %>%
  select(shot_code_yds, ref_distance_value, start_surface, high_level_desc, scratch_exp, pga_exp, 
         avg_80_exp, avg_85_exp, avg_90_exp, avg_95_exp, avg_100_exp) %>%
  arrange(start_surface, ref_distance_value)

# pivot data from wide to tall
xStrokes_tall <- xStrokes_filled %>%
  pivot_longer(
    cols = c(scratch_exp, pga_exp, avg_80_exp, avg_85_exp, avg_90_exp, avg_95_exp, avg_100_exp),
    names_to = "handicap",
    values_to = "expected_strokes"
  )

ui <- fluidPage(
  titlePanel("Golf Strokes Gained App"),
  theme = shinythemes::shinytheme("simplex"),
  
  # DataTable inherently makes you click Tab to submit value
  # using JS auto-blur to allow Enter to do the same
  tags$script(HTML("
    $(document).on('keydown', '.dataTable input', function(e) {
      if (e.key === 'Enter') {
        $(this).blur();
      }
    });
  ")),
  
  fluidRow(
    column(
      width = 4,
      fluidRow(
        column(
          width = 4,
          numericInput("num_rows", "Shots to Add:", value = 36, min = 1)
        ),
        column(
          width = 4,
          style = "padding-top: 25px;",
          actionButton("add_rows", "Add Shots", style = "width: 100%;")
        ),
        column(
          width = 4,
          style = "padding-top: 25px;",
          downloadButton("download_csv", "Download", style = "width: 100%;")
        )
      ),
      div(
        style = "margin-top: 10px;",
        radioButtons(
          "handicap_baseline", "Choose Handicap Baseline:",
          choices = c(
            "PGA" = "pga_exp",
            "Scratch" = "scratch_exp",
            "80" = "avg_80_exp",
            "85" = "avg_85_exp",
            "90" = "avg_90_exp",
            "95" = "avg_95_exp",
            "100" = "avg_100_exp"
          ),
          selected = "pga_exp",
          inline = TRUE
        )
      )
    ),
    
    column(
      width = 8,
      uiOutput("sg_kpi_boxes")
    )
  ),
  
  fluidRow(
    column(
      width = 4,
      tags$div(
        tags$hr(),
        tags$p(tags$b("App Usage:")),
        tags$p(
          "Enter in the starting location of each of your shots in the ",
          tags$b("Shot Start"),
          " column. If you holed out on a shot, indicate this by entering in any value into the ",
          tags$b("Ball in Hole"),
          " column, otherwise, leave it blank.",
          tags$br(), tags$br(),
          "Double click a cell to modify, and click 'Enter' or 'Tab' to submit a shot. The format for 'Shot Start' is yardage followed by a code:"
        ),
        tags$ul(
          tags$li("'t' = tee"),
          tags$li("'f' = fairway"),
          tags$li("'r' = rough"),
          tags$li("'dr' = deep rough"),
          tags$li("'s' = sand"),
          tags$li("'rec' = recovery"),
          tags$li("'g' = green")
        ),
        tags$p(
          "For example, 400t would be entered if teeing off on a 400 yard hole, 150f for a shot from 150 yards in fairway, and 30g for a 30 foot putt on the green."
        )
      )
    ),
    column(
      width = 8,
      DTOutput("sg_table")
    )
  )
)


# server
server <- function(input, output, session) {
  
  # holds user input table. render 36 shots upon app open
  initialize_table <- reactiveVal(data.frame(
    shot_code_yds = rep(NA_character_, 36),
    in_hole = rep(NA_character_, 36),
    strokes_gained = rep(NA_real_, 36),
    stringsAsFactors = FALSE
  ))
  
  # add rows/"shots" when button is clicked
  observeEvent(input$add_rows, {
    n <- input$num_rows
    
    new_rows <- data.frame(
      shot_code_yds = rep(NA_character_, n),
      in_hole = rep(NA_character_, n),
      strokes_gained = rep(NA_real_, n),
      stringsAsFactors = FALSE
    )
    
    updated_table <- rbind(initialize_table(), new_rows)
    initialize_table(updated_table)
  })
  

  # get shots recorded by user in table
  observeEvent(input$sg_table_cell_edit, {
    info <- input$sg_table_cell_edit
    edited_table <- initialize_table()
    edited_table[info$row, info$col] <- DT::coerceValue(info$value, edited_table[info$row, info$col])
    initialize_table(edited_table)
  })
  
  # join user inputted shots to expected strokes df
  joined_data <- reactive({
    user_df <- initialize_table()
    if (nrow(user_df) == 0) return(user_df)
    
    baseline_col <- input$handicap_baseline
    
    user_df %>%
      left_join(xStrokes_filled, by = "shot_code_yds") %>%
      mutate(
        baseline = .data[[baseline_col]],
        is_holed = !is.na(in_hole) & in_hole != "",
        strokes_gained = round(ifelse(is_holed, baseline - 1, baseline - lead(baseline, order_by = row_number()) - 1), 2)) %>%
      select(shot_code_yds, in_hole, strokes_gained, high_level_desc)
  })

  output$sg_table <- renderDT({
    joined_data() %>%
      select(shot_code_yds, in_hole, strokes_gained) %>%
      datatable(
      editable = list(target = "cell", disable = list(columns = c(3:ncol(joined_data())))),
      colnames = c(
        'Shot Start' = 'shot_code_yds',
        'Ball in Hole' = 'in_hole',
        'Strokes Gained' = 'strokes_gained'
      ),
      options = list(
        paging = FALSE,
        dom = 't',
        columnDefs = list(list(className = 'dt-center', targets = 1:3))),
      class = 'cell-border'
    ) %>%
    formatStyle(
      'Strokes Gained',
      # hulk color palette for strokes gained
      backgroundColor = styleInterval(c(-Inf, -0.5, -0.01, 0, 0.5, Inf),
                                      c("#762a83", "#af8dc3", "#e7d4e8", '#fffff', "#d9f0d3", "#7fbf7b", "#1b7837"))
    )
  })
  
  # kpi cards for each SG category
  output$sg_kpi_boxes <- renderUI({
    kpi_data <- joined_data()
    if (nrow(kpi_data) == 0) return(NULL)
    
    kpi_data <- kpi_data %>%
      filter(!is.na(strokes_gained)) %>%
      group_by(high_level_desc) %>%
      summarise(total_sg = round(sum(strokes_gained), 2), .groups = "drop")
    
    if (nrow(kpi_data) == 0) return(NULL)
    
    kpi_cards <- lapply(1:nrow(kpi_data), function(i) {
      desc <- kpi_data$high_level_desc[i]
      value <- kpi_data$total_sg[i]
      
      infoBox(
        title = desc,
        value = value,
        icon = icon("golf-ball-tee"),
        width = 2
      )
    })
    
    fluidRow(kpi_cards)
  })
  
  # write sg data to csv
  output$download_csv <- downloadHandler(
    filename = function() {
      paste0("strokes_gained_data_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(joined_data(), file, row.names = FALSE)
    }
  )
}
  
shinyApp(ui, server)