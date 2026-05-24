# checks
# each hole starts with "t" in it

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

#### DATA TESTS ####
# bad data will flow into this df
hole_starts_with_tee_shot_code <- sg_composite %>%
  filter(stroke == 1 & !grepl("t", start))

####################

### EXPECTED STROKES #### 
# data/expected_strokes/expected_strokes_dataset.csv"
xStrokes <- read.csv("/Users/adambeaudet/Github/stRokes_gained/data/expected_strokes/expected_strokes_dataset.csv") %>%
  select(-c(hdcp_4_exp:hdcp_20_exp))

dim_sg_cat <- xStrokes %>%
  select(shot_code_yds, description, high_level_desc, detailed_desc, sg_category_25)

# impute any gaps in data (thousands of distance/lie/handicap combinations)
xStrokes_filled <- xStrokes %>%
  group_by(start_surface) %>%
  arrange(ref_distance_value) %>%
  mutate(across(c(pga_exp, scratch_exp, avg_80_exp, avg_85_exp, avg_90_exp, avg_95_exp, avg_100_exp),
                ~ zoo::na.approx(.x, ref_distance_value, rule = 2))) %>%
  ungroup() %>%
  select(shot_code_yds, ref_distance_value, start_surface, pga_exp, scratch_exp, 
         avg_80_exp, avg_85_exp, avg_90_exp, avg_95_exp, avg_100_exp) %>%
  arrange(start_surface, ref_distance_value)

# pivot data from wide to tall
xStrokes_tall <- xStrokes_filled %>%
  pivot_longer(
    cols = c(scratch_exp, pga_exp, avg_80_exp, avg_85_exp, avg_90_exp, avg_95_exp, avg_100_exp),
    names_to = "handicap",
    values_to = "expected_strokes"
  )

### END EXPECTED STROKES ###

### READ IN SHOT DATA + DATA CLEANING ###
# avoid hardcoding before committing
# but if this is local exploration it's fine
setwd("/Users/adambeaudet/Github/stRokes_gained/data/shot_tracking")

# old format of logging strokes before using shiny app directly
# palmetto dunes 14 has the same hole index twice somehow
shot_history_old <- read_excel("/Users/adambeaudet/Github/stRokes_gained/data/shot_tracking/shot_tracking_file.xlsx", sheet = "rounds") %>%
  select(date:finish) %>%
  select(-hole_orientation) %>%
  rename(in_hole = finish) %>%
  mutate(src = "Old") %>%
  group_by(date, course) %>%
  mutate(
    is_tee_shot = grepl("t$", start),
    prev_in_hole = lag(in_hole, default = NA),
    starts_new_hole = is_tee_shot & (!is.na(prev_in_hole) | row_number() == 1),
    hole_index = cumsum(starts_new_hole)
  ) %>%
  ungroup() %>%
  select(-is_tee_shot, -prev_in_hole, -starts_new_hole)


round_file_paths <- list.files(
  "/Users/adambeaudet/Github/stRokes_gained/data/shot_tracking", 
  pattern = "\\.csv$",
  full.names = TRUE
)

# need like a dim course that stores city, state, logo, etc.

# hole index more-so important than physical hole. physical hole would be good for best/worst holes if same course played a lot
shot_history_new <- map_dfr(round_file_paths, function(path) {
  df <- read.csv(path)
  
  fname <- basename(path) |> str_remove("\\.csv$")
  date <- str_extract(fname, "\\d{4}-\\d{2}-\\d{2}")
  course <- str_remove(fname, "_\\d{4}-\\d{2}-\\d{2}$")
  
  df %>%
    mutate(course = course, date = as.Date(date)) %>%
    rename(start = shot_code_yds) %>%
    select(-c(strokes_gained, high_level_desc)) %>%
    arrange(course, date) %>%
    group_by(course, date) %>%
    mutate(
      prev_in_hole = lag(in_hole, default = NA),
      is_tee_shot = grepl("t$", start),
      starts_new_hole = is_tee_shot & (!is.na(prev_in_hole) | row_number() == 1),
      hole_index = cumsum(starts_new_hole)
    ) %>%
    group_by(course, date, hole_index) %>%
    mutate(
      stroke = row_number()
    ) %>%
    ungroup() %>%
    select(-c(is_tee_shot, prev_in_hole, starts_new_hole)) %>%
    mutate(src = "New") %>%
    mutate(across(where(is.character), ~na_if(., ""))) %>%
    filter(!is.na(start)) %>%
    # for ordering before rbind
    select(date, course, hole, par, stroke, club, start, in_hole, src, hole_index)
})

# combine two formats into one data frame
shot_history_all <- bind_rows(shot_history_old, shot_history_new)

shot_history_all <- shot_history_all %>%
  mutate(course_clean = str_to_title(str_replace_all(course, '_', ' '))) %>%
  relocate(course_clean, .after = course) %>%
  mutate(round_id = paste0(course_clean, " | ", date)) %>%
  relocate(round_id, .after = course_clean) %>%
  mutate(finish = ifelse(is.na(in_hole), lead(start), in_hole)) %>%
  fill(hole) %>% 
  fill(par) %>% 
  mutate(start_surface = ifelse(grepl('f', start), 'Fairway',
                                ifelse(grepl('g', start), 'Green',
                                       ifelse(grepl('t', start), 'Tee',
                                              ifelse(grepl('s', start), 'Sand',
                                                     ifelse(grepl('rec', start), 'Recovery',
                                                            ifelse(grepl('r', start), 'Rough', 'Other'))))))) %>%
  mutate(finish_surface = ifelse(grepl('f', finish), 'Fairway',
                                 ifelse(grepl('g', finish), 'Green',
                                        ifelse(grepl('t', finish), 'Tee',
                                               ifelse(grepl('s', finish), 'Sand',
                                                      ifelse(grepl('rec', finish), 'Recovery',
                                                             ifelse(grepl('r', finish), 'Rough', 'Other'))))))) %>%
  mutate(start_distance = str_extract_all(start, "\\d+")) %>%
  mutate(finish_distance = if_else(!is.na(in_hole), 0, as.numeric(str_extract(finish, "\\d+")))) %>%
  mutate(finish_footage = if_else(!grepl("g", finish), finish_distance * 3, finish_distance)) %>%
  group_by(round_id) %>%
  mutate(shot_number = row_number()) %>%
  ungroup() %>%
  select(-c(start_surface)) %>%
  mutate(penalty = if_else(lead(club) == "penalty" | club == "penalty", 1, 0))
# coalesce in_hole. 0 and 1


# join shot data with expected strokes data
sg_composite <- left_join(shot_history_all, xStrokes_filled, by = c("start" = "shot_code_yds"))
sg_composite <- left_join(sg_composite, dim_sg_cat, by = c("start" = "shot_code_yds"))

sg_composite <- sg_composite %>% 
  mutate(pga_sg = ifelse(!is.na(in_hole), pga_exp - 1, (pga_exp-lead(pga_exp)-1))) %>% 
  mutate(scratch_sg = ifelse(!is.na(in_hole), scratch_exp - 1, (scratch_exp-lead(scratch_exp)-1))) %>%
  mutate(avg_80_sg = ifelse(!is.na(in_hole), avg_80_exp - 1, (pga_exp-lead(avg_80_exp)-1))) %>% 
  mutate(avg_85_sg = ifelse(!is.na(in_hole), avg_85_exp - 1, (pga_exp-lead(avg_85_exp)-1))) %>% 
  mutate(avg_90_sg = ifelse(!is.na(in_hole), avg_90_exp - 1, (pga_exp-lead(avg_90_exp)-1))) %>% 
  mutate(avg_95_sg = ifelse(!is.na(in_hole), avg_95_exp - 1, (pga_exp-lead(avg_95_exp)-1))) %>% 
  mutate(avg_100_sg = ifelse(!is.na(in_hole), avg_100_exp - 1, (pga_exp-lead(avg_100_exp)-1))) %>% 
  mutate(club = if_else(club == "Driver", "d", club), club = tolower(club)) %>%
  mutate(drive_distance = ifelse(club == "d", as.numeric(start_distance) - as.numeric(finish_distance), NA_real_))

sg_composite$pga_sg <- round(as.numeric(sg_composite$pga_sg), digits = 2)
sg_composite$scratch_sg <- round(as.numeric(sg_composite$scratch_sg), digits = 2)
sg_composite$start_distance <- round(as.numeric(sg_composite$start_distance), digits = 0)

options(scipen = 999)

### END READ IN SHOT DATA + DATA CLEANING ###





# datagolf esque chart
score_to_par <- sg_composite %>%
  filter(!is.na(in_hole)) %>%
  mutate(score_to_par = stroke - par) %>%
  group_by(round_id) %>%
  summarise(score_to_par = sum(score_to_par))


sg_summarised <- sg_composite %>%
  arrange(date) %>%
  mutate(high_level_desc = coalesce(high_level_desc, "Unknown")) %>%
  bind_rows(sg_composite %>% mutate(high_level_desc = "TOTAL")) %>%
  group_by(round_id, high_level_desc) %>%
  summarise(
    pga_sg = sum(pga_sg),
    scratch_sg = sum(scratch_sg),
    strokes = n(),
    holes = sum(hole != lag(hole), na.rm = TRUE) + 1,
    date = first(date),
    .groups = "drop"
  ) %>%
  left_join(score_to_par, by = "round_id") %>%
  arrange(date) %>%
  group_by(high_level_desc) %>%
  mutate(
    round_index = row_number(),
    moving_avg = zoo::rollapply(scratch_sg, width = 20, FUN = mean, fill = NA, align = "right", partial = TRUE),
    course_name = sub(" \\| .*", "", round_id),
    course_name = gsub("_", " ", course_name),
    course_name = tools::toTitleCase(course_name),
    bar_color = case_when(
      high_level_desc == "TOTAL" & !holes %in% c(9, 18) ~ "rgba(100, 195, 220, 0.4)",
      scratch_sg < 0 ~ "rgba(224, 112, 112, 0.4)",
      TRUE           ~ "rgba(100, 180, 160, 0.4)"
    ),
    score_label = case_when(
      score_to_par == 0 ~ "E",
      score_to_par > 0  ~ paste0("+", score_to_par),
      TRUE              ~ as.character(score_to_par)
    )
  ) %>%
  ungroup()





ui <- fluidPage(
  
  titlePanel("Golf Strokes Gained App"),
  
  theme = shinythemes::shinytheme("flatly"),
  
  # JS for enter key
  tags$script(HTML("
    $(document).on('keydown', '.dataTable input', function(e) {
      if (e.key === 'Enter') {
        $(this).blur();
      }
    });
  ")),
  
  
  # global handicap selection
  fluidRow(
    column(
      width = 12,
      
      div(
        style = "
          background-color: #f8f9fa;
          padding: 12px 18px;
          border-radius: 10px;
          margin-bottom: 15px;
        ",
        
        radioButtons(
          "handicap_baseline",
          "Choose Handicap Baseline:",
          choices = c(
            "PGA" = "pga_exp",
            "Scratch" = "scratch_exp",
            "80" = "avg_80_exp",
            "85" = "avg_85_exp",
            "90" = "avg_90_exp",
            "95" = "avg_95_exp",
            "100" = "avg_100_exp"
          ),
          selected = "scratch_exp",
          inline = TRUE
        )
      )
    )
  ),
  
  
  # tabs
  tabsetPanel(
    
    # round/data entry
    tabPanel(
      "Round Entry",
      
      br(),
      
      fluidRow(
        
        column(
          width = 4,
          
          fluidRow(
            
            column(
              width = 4,
              numericInput(
                "num_rows",
                "Shots to Add:",
                value = 36,
                min = 1
              )
            ),
            
            column(
              width = 4,
              style = "padding-top: 25px;",
              actionButton(
                "add_rows",
                "Add Shots",
                style = "width: 100%;"
              )
            ),
            
            column(
              width = 4,
              style = "padding-top: 25px;",
              downloadButton(
                "download_csv",
                "Download",
                style = "width: 100%;"
              )
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
              tags$br(),
              tags$br(),
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
    ),
    
    
    # scorecards
    tabPanel(
      "Scorecards",
      
      br(),
      
      fluidRow(
        column(
          width = 12,
          
          selectInput(
            "selected_scorecard",
            "Select Round",
            choices = sg_composite %>%
              distinct(round_id, date) %>%
              arrange(desc(date)) %>%
              pull(round_id)
          ),
          
          gt_output("scorecards")
        )
      )
    ),
    
    
    # strokes gained breakout by category
    tabPanel(
      "SG Breakout",
      
      br(),
      
      gt_output("sg_breakout")
    ),
    
    
    # moving averages
    tabPanel(
      "Moving Avg",
      
      br(),
      
      selectInput(
        inputId = "strokes_gained_category",
        label = "SG Category",
        choices = c(
          "TOTAL",
          "Off the Tee",
          "Approach",
          "Around the Green",
          "Putting"
        ),
        selected = "TOTAL"
      ),
      
      plotlyOutput("moving_avg")
    ),
    
    # cumulative strokes gained by category
    tabPanel(
      "Cumulative",
      plotlyOutput("cumulative_sg")
    )
  )
)

# server
server <- function(input, output, session) {
  
  
  # shared reactive used by scorecards and sg_breakout
  hole_level_data <- reactive({
    
    selected_handicap <- switch(
      input$handicap_baseline,
      "pga_exp" = "pga_sg",
      "scratch_exp" = "scratch_sg",
      "avg_80_exp" = "avg_80_sg",
      "avg_85_exp" = "avg_85_sg",
      "avg_90_exp" = "avg_90_sg",
      "avg_95_exp" = "avg_95_sg",
      "avg_100_exp" = "avg_100_sg"
    )
    
    sg_composite %>%
      mutate(selected_handicap = .data[[selected_handicap]]) %>%
      mutate(gir = ifelse(par - stroke >= 2 & finish_surface == "Green", 1, 0)) %>%
      mutate(under_regulation = ifelse(par - stroke >= 3 & finish_surface == "Green", 1, 0)) %>%
      mutate(fir = ifelse(par >= 4 & stroke == 1 & finish_surface == 'Fairway', 1,
                          ifelse(par == 3, NA, 0))) %>%
      mutate(feet_of_putts_made = ifelse(start_surface == "Green" & !is.na(in_hole), start_distance, 0)) %>%
      group_by(round_id, date, hole_index, par) %>%
      summarise(
        score = max(stroke),
        score_to_par = max(stroke) - max(par),
        hole_distance = first(start_distance),
        
        sg_ott = if (all(is.na(selected_handicap[high_level_desc == 'Off the Tee']))) NA_real_
        else sum(selected_handicap[high_level_desc == 'Off the Tee'], na.rm = TRUE),
        
        sg_app = if (all(is.na(selected_handicap[high_level_desc == 'Approach']))) NA_real_
        else sum(selected_handicap[high_level_desc == 'Approach'], na.rm = TRUE),
        
        sg_arg = if (all(is.na(selected_handicap[high_level_desc == 'Around the Green']))) NA_real_
        else sum(selected_handicap[high_level_desc == 'Around the Green'], na.rm = TRUE),
        
        sg_putt = if (all(is.na(selected_handicap[high_level_desc == 'Putting']))) NA_real_
        else sum(selected_handicap[high_level_desc == 'Putting'], na.rm = TRUE),
        
        gir = max(gir),
        under_regulation = max(under_regulation),
        fir = max(fir),
        feet_of_putts_made = max(feet_of_putts_made),
        
        drive_distance = if (
          all(is.na(drive_distance)) ||
          any(penalty != 0, na.rm = TRUE)
        ) NA_real_
        else sum(drive_distance, na.rm = TRUE),
        
        hole = first(hole)
      ) %>%
      group_by(round_id) %>%
      arrange(hole_index, .by_group = TRUE) %>%
      mutate(
        hole_index = row_number(),
        running_score_to_par = cumsum(score_to_par)
      ) %>%
      mutate(score_label = case_when(
        score_to_par ==  0 ~ "Par",
        score_to_par ==  1 ~ "Bogey",
        score_to_par ==  2 ~ "Double Bogey",
        score_to_par ==  3 ~ "Triple Bogey",
        score_to_par ==  4 ~ "Quadruple Bogey",
        score_to_par == -1 ~ "Birdie",
        score_to_par == -2 ~ "Eagle",
        score_to_par == -3 ~ "Double Eagle",
        TRUE ~ NA_character_
      ))
  })
  
  
  initialize_table <- reactiveVal(data.frame(
    shot_code_yds = rep(NA_character_, 36),
    hole = rep(NA_character_, 36),
    par = rep(NA_character_, 36),
    club = rep(NA_character_, 36),
    in_hole = rep(NA_character_, 36),
    strokes_gained = rep(NA_real_, 36),
    stringsAsFactors = FALSE
  ))
  
  observeEvent(input$add_rows, {
    n <- input$num_rows
    new_rows <- data.frame(
      shot_code_yds = rep(NA_character_, n),
      hole = rep(NA_character_, n),
      par = rep(NA_character_, n),
      club = rep(NA_character_, n),
      in_hole = rep(NA_character_, n),
      strokes_gained = rep(NA_real_, n),
      stringsAsFactors = FALSE
    )
    initialize_table(rbind(initialize_table(), new_rows))
  })
  
  observeEvent(input$sg_table_cell_edit, {
    info <- input$sg_table_cell_edit
    edited_table <- initialize_table()
    edited_table[info$row, info$col] <- DT::coerceValue(info$value, edited_table[info$row, info$col])
    initialize_table(edited_table)
  })
  
  joined_data <- reactive({
    user_df <- initialize_table()
    if (nrow(user_df) == 0) return(user_df)
    
    baseline_col <- input$handicap_baseline
    
    user_df %>%
      left_join(xStrokes_filled, by = "shot_code_yds") %>%
      left_join(dim_sg_cat %>% select(shot_code_yds, high_level_desc), by = "shot_code_yds") %>%
      mutate(
        baseline = .data[[baseline_col]],
        is_holed = !is.na(in_hole) & in_hole != "",
        strokes_gained = round(ifelse(is_holed, baseline - 1, baseline - lead(baseline, order_by = row_number()) - 1), 2)
      ) %>%
      select(shot_code_yds, hole, par, club, in_hole, strokes_gained, high_level_desc)
  })
  
  
  
  # render once only
  table_rendered <- reactiveVal(FALSE)
  
  output$sg_table <- renderDT({
    table_rendered(TRUE)
    # read joined_data once on init. then isolate prevents re-render on cell edits
    # but renderDT still fires when add_rows button is clicked from initialize_table
    joined_data() %>%
      select(shot_code_yds, hole, par, club, in_hole, strokes_gained) %>%
      datatable(
        editable = list(target = "cell", disable = list(columns = c(6))),
        colnames = c(
          'Shot Start' = 'shot_code_yds',
          'Hole' = 'hole',
          'Par' = 'par',
          'Club' = 'club',
          'Ball in Hole' = 'in_hole',
          'Strokes Gained' = 'strokes_gained'
        ),
        options = list(
          paging = FALSE,
          dom = 't',
          columnDefs = list(list(className = 'dt-center', targets = 1:6))
        ),
        class = 'cell-border'
      ) %>%
      formatStyle(
        'Strokes Gained',
        backgroundColor = styleInterval(
          c(-1.0, -0.5, -0.2, -0.001, 0.001, 0.2, 0.5, 1.0),
          c("#3C3489", "#7F77DD", "#AFA9EC", "#E1D8F5", "#ffffff", "#9FE1CB", "#5DCAA5", "#1D9E75", "#085041")
        ),
        color = styleInterval(
          c(-0.5, -0.2, 0.2, 0.5),
          c("#fff", "#3C3489", "#1a1a1a", "#1a1a1a", "#fff")
        )
      )
  })
  
  proxy <- dataTableProxy("sg_table")
  
  # only will fire on cell edits, not row additions
  observeEvent(input$sg_table_cell_edit, {
    req(table_rendered())
    replaceData(
      proxy,
      joined_data() %>% select(shot_code_yds, hole, par, club, in_hole, strokes_gained),
      resetPaging = FALSE,
      rownames = FALSE
    )
  })
  
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
      infoBox(title = desc, value = value, icon = icon("golf-ball-tee"), width = 2)
    })
    
    fluidRow(kpi_cards)
  })
  
  output$download_csv <- downloadHandler(
    filename = function() paste0("strokes_gained_data_", Sys.Date(), ".csv"),
    content = function(file) write.csv(joined_data(), file, row.names = FALSE)
  )
  
  output$scorecards <- render_gt({
    
    hole_level_data <- reactive({
      
      selected_handicap <- switch(
        input$handicap_baseline,
        "pga_exp" = "pga_sg",
        "scratch_exp" = "scratch_sg",
        "avg_80_exp" = "avg_80_exp",
        "avg_85_exp" = "avg_85_exp",
        "avg_90_exp" = "avg_90_exp",
        "avg_95_exp" = "avg_95_exp",
        "avg_100_exp" = "avg_100_exp"
      )
      
      sg_composite %>%
        mutate(
          selected_handicap = .data[[selected_handicap]]
        ) %>%
        mutate(gir = ifelse(par - stroke >= 2 & finish_surface == "Green", 1, 0)) %>%
        mutate(under_regulation = ifelse(par - stroke >= 3 & finish_surface == "Green", 1, 0)) %>%
        mutate(fir = ifelse(par >= 4 & stroke == 1 & finish_surface == 'Fairway', 1,
                            ifelse(par == 3, NA, 0))) %>%
        mutate(feet_of_putts_made = ifelse(start_surface == "Green" & !is.na(in_hole), start_distance, 0)) %>%
        group_by(round_id, date, hole_index, par) %>%
        summarise(
          score = max(stroke), 
          score_to_par = max(stroke) - max(par), 
          hole_distance = first(start_distance),
          
          sg_ott = if (all(is.na(selected_handicap[high_level_desc == 'Off the Tee']))) NA_real_
          else sum(selected_handicap[high_level_desc == 'Off the Tee'], na.rm = TRUE), 
          
          sg_app = if (all(is.na(selected_handicap[high_level_desc == 'Approach']))) NA_real_
          else sum(selected_handicap[high_level_desc == 'Approach'], na.rm = TRUE), 
          
          sg_arg = if (all(is.na(selected_handicap[high_level_desc == 'Around the Green']))) NA_real_
          else sum(selected_handicap[high_level_desc == 'Around the Green'], na.rm = TRUE), 
          
          sg_putt = if (all(is.na(selected_handicap[high_level_desc == 'Putting']))) NA_real_
          else sum(selected_handicap[high_level_desc == 'Putting'], na.rm = TRUE),
          
          gir = max(gir),
          under_regulation = max(under_regulation),
          fir = max(fir),
          feet_of_putts_made = max(feet_of_putts_made),
          
          drive_distance = if (
            all(is.na(drive_distance)) ||
            any(penalty != 0, na.rm = TRUE)
          ) NA_real_
          else sum(drive_distance, na.rm = TRUE),
          
          hole = first(hole)
        ) %>%
        group_by(round_id) %>%
        arrange(hole_index, .by_group = TRUE) %>%
        mutate(
          hole_index = row_number(),
          running_score_to_par = cumsum(score_to_par)
        ) %>%
        mutate(score_label = case_when(
          score_to_par ==  0 ~ "Par",
          score_to_par ==  1 ~ "Bogey",
          score_to_par ==  2 ~ "Double Bogey",
          score_to_par ==  3 ~ "Triple Bogey",
          score_to_par ==  4 ~ "Quadruple Bogey",
          score_to_par == -1 ~ "Birdie",
          score_to_par == -2 ~ "Eagle",
          score_to_par == -3 ~ "Double Eagle",
          TRUE ~ NA_character_
        ))
      
    })
    
    
    selected_scorecard <- hole_level_data() %>%
      filter(round_id == input$selected_scorecard)
    
    build_scorecard_gt <- function(round_data) {
      
      stp_values <- round_data$score_to_par
      final_stp <- round_data$running_score_to_par[which.max(round_data$hole_index)]
      hole_order <- as.character(round_data$hole_index)
      
      # physical hole number as a display row
      hole_number_row <- tibble(
        Hole = "Physical Hole",
        !!!setNames(as.list(as.character(round_data$hole)), hole_order)
      )
      
      pivoted <- round_data %>%
        ungroup() %>%
        mutate(across(c(sg_ott, sg_app, sg_arg, sg_putt), ~ round(.x, 2))) %>%
        rename(
          "Par" = "par",
          "Hole" = "hole_index",
          "Score" = "score",
          "Yardage" = "hole_distance",
          "+/-" = "running_score_to_par",
          "GIR" = "gir",
          "FIR" = "fir",
          "Feet of Putts Made" = "feet_of_putts_made",
          "Drive Distance" = "drive_distance",
          "SG: OTT" = "sg_ott",
          "SG: APP" = "sg_app",
          "SG: ARG" = "sg_arg",
          "SG: PUTT" = "sg_putt"
        ) %>%
        select(Hole, Par, Score, Yardage, `+/-`, GIR, FIR, `Feet of Putts Made`, `Drive Distance`, `SG: OTT`, `SG: APP`, `SG: ARG`, `SG: PUTT`) %>%
        mutate(across(everything(), as.character)) %>%
        pivot_longer(
          cols = c(Par, Score, Yardage, `+/-`, GIR, FIR, `Feet of Putts Made`, `Drive Distance`, `SG: OTT`, `SG: APP`, `SG: ARG`, `SG: PUTT`),
          names_to = "metric",
          values_to = "value"
        ) %>%
        mutate(
          metric = factor(metric, levels = c("Par", "Score", "Yardage", "+/-", "GIR", "FIR", "Feet of Putts Made", "Drive Distance", "SG: OTT", "SG: APP", "SG: ARG", "SG: PUTT")),
          Hole = factor(Hole, levels = hole_order),
          value = as.numeric(value)
        ) %>%
        arrange(metric) %>%
        pivot_wider(names_from = Hole, values_from = value) %>%
        rename("Hole" = "metric") %>%
        mutate(Hole = as.character(Hole)) %>%
        select(Hole, all_of(hole_order), everything()) %>%
        mutate(
          Total = rowSums(across(where(is.numeric)), na.rm = TRUE),
          Total = ifelse(Hole == "+/-", final_stp, Total),
          Total = ifelse(Hole == "Drive Distance", round(mean(round_data$drive_distance, na.rm = TRUE), 1), Total)
        )
      
      hole_cols <- hole_order
      
      bind_rows(
        hole_number_row, 
        pivoted %>% mutate(across(where(is.numeric), as.character))) %>%
        gt() %>%
        tab_header(title = unique(round_data$round_id)) %>%
        fmt_integer(columns = c(all_of(hole_cols), "Total"), rows = Hole %in% c("Par", "Score", "Yardage", "+/-", "Feet of Putts Made", "Drive Distance")) %>%
        fmt_number(columns = c(all_of(hole_cols), "Total"), rows = Hole %in% c("SG: OTT", "SG: APP", "SG: ARG", "SG: PUTT"), decimals = 2) %>%
        fmt_missing(columns = all_of(c(hole_cols, "Total")), missing_text = "-") %>%
        text_transform(
          locations = cells_body(columns = all_of(hole_cols), rows = Hole == "GIR"),
          fn = function(x) dplyr::case_when(
            x == "1" ~ "<span style='background-color:#18a153; color:white; padding: 0px 5px; border-radius: 5px; font-weight:bold;'>✓</span>",
            x == "0" ~ "<span style='background-color:#d9485b; color:white; padding: 0px 5px; border-radius: 5px; font-weight:bold;'>✗</span>",
            TRUE      ~ "-"
          )
        ) %>%
        text_transform(
          locations = cells_body(columns = all_of(hole_cols), rows = Hole == "FIR"),
          fn = function(x) dplyr::case_when(
            x == "1" ~ "<span style='background-color:#18a153; color:white; padding: 0px 5px; border-radius: 5px; font-weight:bold;'>✓</span>",
            x == "0" ~ "<span style='background-color:#d9485b; color:white; padding: 0px 5px; border-radius: 5px; font-weight:bold;'>✗</span>",
            TRUE      ~ "-"
          )
        ) %>%
        tab_style(style = cell_text(weight = "bold"), locations = cells_body(rows = Hole == "Score")) %>%
        tab_style(
          style = cell_text(color = "gray50", size = "small"),
          locations = cells_body(rows = Hole %in% c("Physical Hole", "Par", "Yardage", "+/-", "GIR", "FIR", "Drive Distance", "Feet of Putts Made", "SG: OTT", "SG: APP", "SG: ARG", "SG: PUTT"))
        ) %>%
        tab_style(
          style = cell_fill(color = "#85cce8"),
          locations = cells_body(columns = all_of(hole_cols[!is.na(stp_values) & stp_values == -1]), rows = Hole == "Score")
        ) %>%
        tab_style(
          style = list(cell_fill(color = "#5b8db8"), cell_text(color = "white")),
          locations = cells_body(columns = all_of(hole_cols[!is.na(stp_values) & stp_values <= -2]), rows = Hole == "Score")
        ) %>%
        tab_style(
          style = cell_fill(color = "#f0c040"),
          locations = cells_body(columns = all_of(hole_cols[!is.na(stp_values) & stp_values == 1]), rows = Hole == "Score")
        ) %>%
        tab_style(
          style = list(cell_fill(color = "#e8622a"), cell_text(color = "white")),
          locations = cells_body(columns = all_of(hole_cols[!is.na(stp_values) & stp_values == 2]), rows = Hole == "Score")
        ) %>%
        tab_style(
          style = list(cell_fill(color = "#a0522d"), cell_text(color = "white")),
          locations = cells_body(columns = all_of(hole_cols[!is.na(stp_values) & stp_values >= 3]), rows = Hole == "Score")
        ) %>%
        cols_align(align = "center", columns = all_of(hole_cols)) %>%
        gt_theme_538(quiet = TRUE) %>%
        cols_width(
          Hole ~ px(180),
          Total ~ px(70)
        ) %>%
        cols_align(align = "center", columns = "Total") %>%
        fmt_number(columns = "Total", rows = Hole == "Drive Distance", decimals = 1)
    }

    build_scorecard_gt(selected_scorecard)

  })
  
  output$sg_breakout <- render_gt({
    
    sg_round_summary <- hole_level_data() %>%
      group_by(round_id, date) %>%
      summarise(
        OTT  = ifelse(all(is.na(sg_ott)),  NA_real_, sum(sg_ott,  na.rm = TRUE)),
        APP  = ifelse(all(is.na(sg_app)),  NA_real_, sum(sg_app,  na.rm = TRUE)),
        ARG  = ifelse(all(is.na(sg_arg)),  NA_real_, sum(sg_arg,  na.rm = TRUE)),
        PUTT = ifelse(all(is.na(sg_putt)), NA_real_, sum(sg_putt, na.rm = TRUE)),
        .groups = "drop"
      ) %>%
      mutate(
        ott_pctile  = percent_rank(OTT),
        app_pctile  = percent_rank(APP),
        arg_pctile  = percent_rank(ARG),
        putt_pctile = percent_rank(PUTT)
      ) %>%
      arrange(desc(date)) %>%
      select(-date)
    
    sg_round_summary %>%
      mutate(
        OTT_label  = ifelse(is.na(OTT),  "-", paste0(round(OTT,  2), " / ", round(ott_pctile  * 100, 1), "%tile")),
        APP_label  = ifelse(is.na(APP),  "-", paste0(round(APP,  2), " / ", round(app_pctile  * 100, 1), "%tile")),
        ARG_label  = ifelse(is.na(ARG),  "-", paste0(round(ARG,  2), " / ", round(arg_pctile  * 100, 1), "%tile")),
        PUTT_label = ifelse(is.na(PUTT), "-", paste0(round(PUTT, 2), " / ", round(putt_pctile * 100, 1), "%tile"))
      ) %>%
      select(round_id, OTT, APP, ARG, PUTT, OTT_label, APP_label, ARG_label, PUTT_label) %>%
      gt() %>%
      cols_label(
        round_id   = "Round",
        OTT_label  = "OTT",
        APP_label  = "APP",
        ARG_label  = "ARG",
        PUTT_label = "PUTT"
      ) %>%
      data_color(
        columns = c(OTT, APP, ARG, PUTT),
        fn = scales::col_numeric(
          palette = c("#d73027", "#fc8d59", "#fee08b", "#d9ef8b", "#91cf60", "#1a9850"),
          domain = NULL,
          na.color = "white"
        ),
        target_columns = c(OTT_label, APP_label, ARG_label, PUTT_label)
      ) %>%
      cols_hide(columns = c(OTT, APP, ARG, PUTT)) %>%
      cols_align(align = "center", columns = -round_id)
  })
  
  output$moving_avg <- renderPlotly({
  
    
    
    # year ranges
    year_ranges <- sg_summarised %>%
      filter(high_level_desc == input$strokes_gained_category) %>%
      group_by(year = format(date, "%Y")) %>%
      summarise(
        start_index = min(round_index),
        end_index = max(round_index),
        mid_index = (start_index + end_index) / 2,
        n_rounds = n(),
        .groups = "drop"
      )
    
    
    # year boundary lines
    year_boundaries <- year_ranges %>%
      arrange(start_index) %>%
      slice(-1) %>%
      mutate(line_pos = start_index - 0.5)
    
    
    
    # year labels
    year_labels <- year_ranges %>%
      filter(n_rounds >= 3)
    
    
    plot_ly() %>%
      add_bars(
        data = sg_summarised %>% dplyr::filter(high_level_desc == input$strokes_gained_category),
        x = ~round_index,
        y = ~scratch_sg,
        marker = list(color = ~bar_color),
        hovertemplate = paste(
          "Round: %{customdata}<br>",
          "Date: %{hovertext}<br>",
          "SG vs Scratch: %{y:.2f}<br>",
          "<extra></extra>"
        ),
        customdata = ~round_id,
        hovertext = ~format(date, "%b %d, %Y"),
        textposition = "none",
        name = "SG per Round"
      ) %>%
      add_text(
        data = sg_summarised %>% filter(high_level_desc == input$strokes_gained_category),
        x = ~round_index,
        y = ~ifelse(scratch_sg >= 0, scratch_sg + 0.1, scratch_sg - 0.1),
        text = ~strokes,
        textposition = ~ifelse(scratch_sg >= 0, "top center", "bottom center"),
        textfont = list(size = 9, color = "rgba(80, 80, 80, 0.8)"),
        hoverinfo = "skip",
        showlegend = FALSE
      ) %>%
      add_lines(
        data = sg_summarised %>% filter(high_level_desc == input$strokes_gained_category),
        x = ~round_index,
        y = ~moving_avg,
        line = list(color = "black", width = 2),
        hoverinfo = "skip",
        name = "Moving Avg"
      ) %>%
      layout(
        annotations = lapply(1:nrow(year_labels), function(i) {
          list(
            x = year_labels$mid_index[i],
            y = 1.03,
            yref = "paper",
            text = year_labels$year[i],
            showarrow = FALSE,
            xanchor = "center",
            yanchor = "bottom",
            font = list(size = 14, color = "gray")
          )
        }),
        shapes = lapply(year_boundaries$line_pos, function(x) {
          list(
            type = "line",
            x0 = x, x1 = x,
            y0 = 0, y1 = 1,
            yref = "paper",
            line = list(color = "gray", width = 1.5, dash = "dash")
          )
        }),
        xaxis = list(
          tickvals = sg_summarised %>% filter(high_level_desc == input$strokes_gained_category) %>% pull(round_index),
          ticktext = sg_summarised %>% filter(high_level_desc == input$strokes_gained_category) %>% pull(course_name),
          tickangle = -90,
          title = "",
          showgrid = FALSE,
          zeroline = FALSE
        ),
        yaxis = list(
          title = "Strokes Gained vs Scratch",
          showgrid = TRUE,
          gridcolor = "rgba(200,200,200,0.4)",
          zeroline = TRUE,
          zerolinecolor = "black",
          zerolinewidth = 1.5
        ),
        plot_bgcolor = "white",
        paper_bgcolor = "white",
        showlegend = FALSE,
        bargap = 0.2,
        margin = list(t = 60)
      )
    
  })
  
  
  output$cumulative_sg <- renderPlotly({
    
    sg_col <- switch(
      input$handicap_baseline,
      "pga_exp"    = "pga_sg",
      "scratch_exp" = "scratch_sg",
      "avg_80_exp" = "avg_80_sg",
      "avg_85_exp" = "avg_85_sg",
      "avg_90_exp" = "avg_90_sg",
      "avg_95_exp" = "avg_95_sg",
      "avg_100_exp" = "avg_100_sg"
    )
    
    cumulative_total <- sg_composite %>%
      arrange(date, hole, stroke) %>%
      mutate(shot_no = row_number()) %>%
      mutate(high_level_desc = 'TOTAL')
    
    cumulative_by_category <- sg_composite %>%
      group_by(high_level_desc) %>%
      arrange(date, hole, stroke) %>%
      mutate(shot_no = row_number())
    
    cumulative_binded <- bind_rows(cumulative_total, cumulative_by_category) %>%
      group_by(high_level_desc) %>%
      arrange(shot_no) %>%
      mutate(
        shot_sg = round(.data[[sg_col]], 2),
        cumulative_sg = round(cumsum(replace_na(.data[[sg_col]], 0)), 2)
      )
    
    cumulative <- ggplot(
      data = cumulative_binded,
      aes(
        x = shot_no,
        y = cumulative_sg,
        color = high_level_desc
      )
    ) +
      geom_line() +
      
      geom_point(
        aes(
          text = paste0(
            "Round: ", round_id,
            "<br>Hole: ", hole,
            "<br>Stroke: ", stroke,
            "<br>Start: ", start,
            "<br>Finish: ", finish,
            "<br>Shot SG: ", shot_sg,
            "<br>Cumulative SG: ", cumulative_sg
          )
        ),
        alpha = 0
      ) +
      
      facet_wrap(vars(high_level_desc), scales = "free") +
      
      geom_hline(
        yintercept = 0,
        color = "gray50"
      ) +
      
      theme(legend.position = "none")
    
    ggplotly(cumulative, tooltip = "text")
    
  })
  
  
}

shinyApp(ui, server)
