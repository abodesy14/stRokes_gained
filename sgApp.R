# checks
# each hole starts with "t" in it

# KPIs to add
# proximity
# birdies per 18
# FIR, GIR streaks 
# most played course
# best hole aggregate score of all times played it
# career +/- to par


# load libraries
{
  library(shiny)
  library(shinyWidgets)
  library(shinybusy)
  library(shinydashboard)
  library(DT)
  library(tidyverse)
  library(plotly)
  library(glue)
  library(gt)
  library(gtExtras)
  library(readxl)
  library(zoo)
  library(tidyr)
  library(ggthemes)
}



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
  mutate(avg_80_sg = ifelse(!is.na(in_hole), avg_80_exp - 1, (avg_80_exp-lead(avg_80_exp)-1))) %>% 
  mutate(avg_85_sg = ifelse(!is.na(in_hole), avg_85_exp - 1, (avg_85_exp-lead(avg_85_exp)-1))) %>% 
  mutate(avg_90_sg = ifelse(!is.na(in_hole), avg_90_exp - 1, (avg_90_exp-lead(avg_90_exp)-1))) %>% 
  mutate(avg_95_sg = ifelse(!is.na(in_hole), avg_95_exp - 1, (avg_95_exp-lead(avg_95_exp)-1))) %>% 
  mutate(avg_100_sg = ifelse(!is.na(in_hole), avg_100_exp - 1, (avg_100_exp-lead(avg_100_exp)-1))) %>% 
  mutate(club = if_else(club == "Driver", "d", club), club = tolower(club)) %>%
  mutate(drive_distance = ifelse(club == "d", as.numeric(start_distance) - as.numeric(finish_distance), NA_real_))

sg_composite$pga_sg <- round(as.numeric(sg_composite$pga_sg), digits = 2)
sg_composite$scratch_sg <- round(as.numeric(sg_composite$scratch_sg), digits = 2)
sg_composite$start_distance <- round(as.numeric(sg_composite$start_distance), digits = 0)

options(scipen = 999)

### END READ IN SHOT DATA + DATA CLEANING ###


#### DATA TESTS ####
# bad data will flow into this df
hole_starts_with_tee_shot_code <- sg_composite %>%
  filter(stroke == 1 & !grepl("t", start))


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
    pga_sg     = sum(pga_sg,     na.rm = TRUE),
    scratch_sg = sum(scratch_sg, na.rm = TRUE),
    avg_80_sg  = sum(avg_80_sg,  na.rm = TRUE),
    avg_85_sg  = sum(avg_85_sg,  na.rm = TRUE),
    avg_90_sg  = sum(avg_90_sg,  na.rm = TRUE),
    avg_95_sg  = sum(avg_95_sg,  na.rm = TRUE),
    avg_100_sg = sum(avg_100_sg, na.rm = TRUE),
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
  
  tags$style(HTML("

/* the whole card */
.info-box {
  min-height: 50px;      /* how tall the card is */
  margin-bottom: 10px;   /* space between rows of cards */
  border: 1px solid #e0e0e0;  /* border: thickness, style, color */
  border-radius: 8px;    /* rounds the corners. 0 = sharp, higher = more rounded */
  box-shadow: none;      /* removes the default shadow shinydashboard adds */
}

/* the colored icon square on the left */
.info-box-icon {
  height: 50px;          /* should match min-height above */
  line-height: 50px;     /* vertically centers the icon. should match height */
  width: 70px;           /* how wide the icon area is */
  font-size: 28px;       /* size of the icon itself */
}

/* the text area on the right of the icon */
.info-box-content {

  /* added: stack label + value vertically */
  display: flex;
  flex-direction: column;
  justify-content: center;

  padding-top: 4px;      /* space above the text */
  padding-bottom: 4px;   /* space below the text */
}

/* the big number in the middle */
.info-box-number {
  font-size: 22px;
  line-height: 1.1;      /* added */
}

/* the title text at the top */
.info-box-text {
  font-size: 13px;
  line-height: 1.1;      /* added */
  margin-bottom: 2px;    /* added */

  /* added: allows long labels to wrap */
  white-space: normal;
}

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
    ),
    
    tabPanel(
      "SG by Category",
      br(),
      plotOutput("sg_by_category", height = "500px")
    ),
    
    tabPanel(
      "Best and Worst Shots",
      br(),
      fluidRow(
        column(
          width = 8,
          selectInput(
            "top_bottom_round",
            "Select Round(s)",
            choices = sg_composite %>%
              distinct(round_id, date) %>%
              arrange(desc(date)) %>%
              pull(round_id),
            multiple = TRUE,
            selected = sg_composite %>%
              distinct(round_id, date) %>%
              arrange(desc(date)) %>%
              slice(1) %>%
              pull(round_id)
          )
        ),
        column(
          width = 2,
          numericInput(
            "top_bottom_n",
            "Top / Bottom N:",
            value = 5,
            min = 1,
            max = 20
          )
        )
      ),
      gt_output("best_and_worst_shots")
    ),
    tabPanel(
      "KPIs",
      br(),
      uiOutput("kpi_cards")
      ),
    
    tabPanel(
      "Data Dictionary",
      br(),
      gt_output("data_dict")
    ),
    
    tabPanel(
      "FAQ"
    )
  )
)

# server
server <- function(input, output, session) {
  
  sg_col <- reactive({
    switch(
      input$handicap_baseline,
      "pga_exp"     = "pga_sg",
      "scratch_exp" = "scratch_sg",
      "avg_80_exp"  = "avg_80_sg",
      "avg_85_exp"  = "avg_85_sg",
      "avg_90_exp"  = "avg_90_sg",
      "avg_95_exp"  = "avg_95_sg",
      "avg_100_exp" = "avg_100_sg"
    )
  })
  
  sg_col_label <- reactive({
    switch(
      input$handicap_baseline,
      "pga_exp"     = "PGA",
      "scratch_exp" = "Scratch",
      "avg_80_exp"  = "80 Avg",
      "avg_85_exp"  = "85 Avg",
      "avg_90_exp"  = "90 Avg",
      "avg_95_exp"  = "95 Avg",
      "avg_100_exp" = "100 Avg"
    )
  })
  
  
  # shared reactive used by scorecards and sg_breakout
  hole_level_data <- reactive({
    
    selected_handicap <- sg_col()
    
    sg_composite %>%
      mutate(selected_handicap = .data[[selected_handicap]]) %>%
      mutate(gir = ifelse(par - stroke >= 2 & finish_surface == "Green", 1, 0)) %>%
      mutate(under_regulation = ifelse(par - stroke >= 3 & finish_surface == "Green", 1, 0)) %>%
      mutate(fir = ifelse(par >= 4 & stroke == 1 & finish_surface == 'Fairway', 1,
                          ifelse(par == 3, NA, 0))) %>%
      mutate(feet_of_putts_made = ifelse(start_surface == "Green" & !is.na(in_hole), start_distance, 0)) %>%
      mutate(good_shot = ifelse(selected_handicap >= 0.5, 1, 0)) %>%
      mutate(bad_shot = ifelse(selected_handicap <= -0.5, 1, 0)) %>%
      group_by(round_id, date, hole_index, par) %>%
      summarise(
        score = max(stroke),
        score_to_par = max(stroke) - max(par),
        hole_distance = first(start_distance),
        good_shot_pct = mean(good_shot, na.rm = TRUE),
        bad_shot_pct = mean(bad_shot, na.rm = TRUE),
        
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
        drive_distance = if (all(is.na(drive_distance)) || any(penalty != 0, na.rm = TRUE)) NA_real_ else sum(drive_distance, na.rm = TRUE),
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
  
  output$kpi_cards <- renderUI({
    
    hld <- hole_level_data()
    
    scramble <- hld %>%
      ungroup() %>%
      filter(gir == 0) %>%
      summarise(opportunities = n(), successes = sum(score_to_par <= 0, na.rm = TRUE), rate = successes / opportunities)

    
    rounds_played <- hld %>% distinct(round_id) %>% nrow()
    gir_pct <- mean(hld$gir, na.rm = TRUE)
    fir_pct <- mean(hld$fir, na.rm = TRUE)
    good_shot_pct <- mean(hld$good_shot_pct, na.rm = TRUE)
    poor_shot_avoidance <- 1 - mean(hld$bad_shot_pct, na.rm = TRUE)
    par_or_better_pct <- mean(hld$score_to_par <= 0, na.rm = TRUE)
    num_eagles <- sum(hld$score_to_par == -2, na.rm = TRUE)
    avg_drive_dist <- mean(hld$drive_distance, na.rm = TRUE)
    
    hole_outs <- sg_composite %>%
      filter(start_surface != "Green" & !is.na(in_hole)) %>%
      nrow()
    
    longest_hole_out <- sg_composite %>%
      filter(start_surface != "Green" & !is.na(in_hole)) %>%
      summarise(longest_hole_out = ifelse(n() == 0, NA_real_, max(start_distance, na.rm = TRUE))) %>%
      pull(longest_hole_out)
    
    best_club <- sg_composite %>%
      filter(!is.na(club) & club != "penalty") %>%
      group_by(club) %>%
      summarise(avg_sg = mean(.data[[sg_col()]], na.rm = TRUE), n = n(), .groups = "drop") %>%
      filter(n >= 10) %>%   # minimum shot threshold to avoid noise
      slice_max(avg_sg, n = 1) %>%
      mutate(label = paste0(toupper(club), " (", round(avg_sg, 2), ")")) %>%
      pull(label)
    
    best_sg_category <- sg_composite %>%
      filter(high_level_desc != "Recovery") %>%
      group_by(sg_category_25) %>%
      summarise(avg_sg = mean(.data[[sg_col()]], na.rm = TRUE), n = n(), .groups = "drop") %>%
      filter(n >= 10) %>%   # minimum shot threshold to avoid noise
      slice_max(avg_sg, n = 1) %>%
      mutate(label = paste0(toupper(sg_category_25), " (", round(avg_sg, 2), ")")) %>%
      pull(label)
    
    
    # dry this up, course exists upstream already
    courses_played <- hld %>%
      ungroup() %>%
      distinct(round_id) %>%
      mutate(course = sub(" \\| .*", "", round_id)) %>%
      distinct(course) %>%
      nrow()
    
    holes_played <- sg_composite %>%
      distinct(round_id, hole_index) %>%
      nrow()
    
    shots_logged <- sg_composite %>%
      nrow()
    
    longest_putt <- sg_composite %>%
      filter(start_surface == "Green" & !is.na(in_hole)) %>%
      summarise(max(start_distance, na.rm = TRUE)) %>%
      pull()
    
    performance_by_par <- hld %>%
      group_by(par) %>%
      summarise(avg_score = mean(score, na.rm = TRUE), .groups = "drop")
    
    par_3_avg <- performance_by_par %>% filter(par == 3) %>% pull(avg_score) %>% round(2)
    par_4_avg <- performance_by_par %>% filter(par == 4) %>% pull(avg_score) %>% round(2)
    par_5_avg <- performance_by_par %>% filter(par == 5) %>% pull(avg_score) %>% round(2)
    
    # catch for if someone hasn't logged any data yet
    fmt_par_avg <- function(x) {
      ifelse(length(x) == 0 || is.nan(x), "-", as.character(round(x, 2)))
    }
    
    # order holes chronologically across all rounds
    hole_sequence <- hld %>%
      ungroup() %>%
      arrange(date, round_id, hole_index)
    
    # run length encoding to find consecutive streaks
    birdie_rle <- rle(hole_sequence$score_to_par == -1)
    par_rle     <- rle(hole_sequence$score_to_par == 0)
    
    max_consec_birdies <- ifelse(any(birdie_rle$values),max(birdie_rle$lengths[birdie_rle$values]), 0)
    max_consec_par <- ifelse(any(par_rle$values), max(par_rle$lengths[par_rle$values]), 0)
    
    
    round_scores_18 <- hld %>%
      group_by(round_id) %>%
      summarise(
        holes        = n(),
        score        = sum(score),
        score_to_par = sum(score_to_par),
        .groups = "drop"
      ) %>%
      filter(holes == 18)
    
    scoring_avg_18   <- ifelse(nrow(round_scores_18) == 0, NA_real_, mean(round_scores_18$score))
    low_round_18     <- ifelse(nrow(round_scores_18) == 0, NA_real_, min(round_scores_18$score_to_par))
    low_round_raw_18 <- ifelse(nrow(round_scores_18) == 0, NA_real_, round_scores_18$score[which.min(round_scores_18$score_to_par)])
    
    
    round_scores_9 <- hld %>%
      group_by(round_id) %>%
      mutate(nine = ifelse(hole_index <= 9, "Front", "Back")) %>%
      group_by(round_id, nine) %>%
      summarise(
        holes        = n(),
        score        = sum(score),
        score_to_par = sum(score_to_par),
        .groups = "drop"
      ) %>%
      filter(holes == 9)
    
    low_round_9     <- ifelse(nrow(round_scores_9) == 0, NA_real_, min(round_scores_9$score_to_par))
    low_round_raw_9 <- ifelse(nrow(round_scores_9) == 0, NA_real_, round_scores_9$score[which.min(round_scores_9$score_to_par)])
    
    sg_summary <- hld %>%
      group_by(round_id) %>%
      summarise(
        across(c(sg_ott, sg_app, sg_arg, sg_putt), ~ sum(.x, na.rm = TRUE)),
        .groups = "drop"
      ) %>%
      summarise(across(c(sg_ott, sg_app, sg_arg, sg_putt), ~ mean(.x, na.rm = TRUE)))
    
    fluidRow(
      style = "padding: 0 6px;",
      infoBox("Rounds Played", as.character(rounds_played), icon = icon("golf-ball-tee"), width = 2),
      infoBox("Courses Played", as.character(courses_played), icon = icon("location-dot"), width = 2),
      infoBox("Holes Played", as.character(holes_played), icon = icon("map"), width = 2),
      infoBox("Shots Logged", as.character(shots_logged), icon = icon("list-ol"), width = 2),
      infoBox("Scrambling", ifelse(is.nan(scramble$rate), "-", scales::percent(scramble$rate, accuracy = 1)), icon = icon("arrow-down-up-across-line"), width = 2),
      infoBox("GIR %", ifelse(is.nan(gir_pct), "-", scales::percent(gir_pct, accuracy = 1)), icon = icon("circle-check"), width = 2),
      infoBox("FIR %", ifelse(is.nan(fir_pct), "-", scales::percent(fir_pct, accuracy = 1)), icon = icon("road"), width = 2),
      infoBox("Scoring Avg", ifelse(is.na(scoring_avg_18), "-", as.character(round(scoring_avg_18, 1))), icon = icon("hashtag"), width = 2),
      infoBox("Low 18", ifelse(is.na(low_round_18), "-", paste0(low_round_raw_18, " (", ifelse(low_round_18 <= 0, as.character(low_round_18), paste0("+", low_round_18)), ")")),icon = icon("trophy"), width = 2),
      infoBox("Low 9", ifelse(is.na(low_round_9), "-", paste0(low_round_raw_9, " (", ifelse(low_round_9 <= 0, as.character(low_round_9), paste0("+", low_round_9)), ")")),icon = icon("trophy"), width = 2),
      infoBox("SG: OTT", as.character(round(sg_summary$sg_ott,  2)), icon = icon("hammer"), width = 2),
      infoBox("SG: APP", as.character(round(sg_summary$sg_app,  2)), icon = icon("bullseye"), width = 2),
      infoBox("SG: ARG", as.character(round(sg_summary$sg_arg,  2)), icon = icon("hand-sparkles"), width = 2),
      infoBox("SG: PUTT", as.character(round(sg_summary$sg_putt, 2)), icon = icon("flag"), width = 2),
      infoBox("Good Shot Rate",  ifelse(is.nan(good_shot_pct), "-", scales::percent(good_shot_pct, accuracy = 1)), icon = icon("star"), width = 2),
      infoBox("Poor Shot Avoidance",  ifelse(is.nan(poor_shot_avoidance), "-", scales::percent(poor_shot_avoidance, accuracy = 1)), icon = icon("poo"), width = 2),
      infoBox("Par or Better", ifelse(is.nan(par_or_better_pct), "-", scales::percent(par_or_better_pct, accuracy = 1)), icon = icon("thumbs-up"), width = 2),
      infoBox("Most Consecutive Birdies", as.character(max_consec_birdies), icon = icon("fire"),      width = 2),
      infoBox("Most Consecutive Pars",    as.character(max_consec_par),     icon = icon("arrow-trend-up"), width = 2),
      infoBox("Eagles", as.character(num_eagles), icon = icon("dove"), width = 2),
      infoBox("Driving Distance", ifelse(is.nan(avg_drive_dist), "-", round(avg_drive_dist, 1)), icon = icon("dumbbell"), width = 2),
      infoBox("Hole Outs", as.character(hole_outs), icon = icon("flag"), width = 2),
      infoBox("Longest Hole Out", as.character(longest_hole_out), icon = icon("ruler-horizontal"), width = 2), # add ifelse if none?
      infoBox("Longest Holed Putt", as.character(longest_putt), icon = icon("ruler-horizontal"), width = 2),
      infoBox("Par 3 Performance", fmt_par_avg(par_3_avg), icon = icon("3"), width = 2),
      infoBox("Par 4 Performance", fmt_par_avg(par_4_avg), icon = icon("4"), width = 2),
      infoBox("Par 5 Performance", fmt_par_avg(par_5_avg), icon = icon("5"), width = 2),
      infoBox("Best Club (SG/shot)", ifelse(length(best_club) == 0, "-", best_club), icon = icon("ranking-star"), width = 2),
      infoBox("Best SG Category (SG/shot)", ifelse(length(best_sg_category) == 0, "-", best_sg_category), icon = icon("ranking-star"), width = 2)
      )
  })
  
  output$data_dict <- render_gt({
    
    tibble(
      KPI = c(
        "Rounds Played",
        "Scrambling",
        "GIR %",
        "FIR %",
        "Scoring Avg",
        "Low Round",
        "Good Shot %",
        "Poor Shot Avoidance %",
        "Par or Better",
        "Consec. Birdies",
        "Consec. Par+",
        "Eagles",
        "Avg Drive",
        "Courses Played",
        "SG: OTT",
        "SG: APP",
        "SG: ARG",
        "SG: PUTT"
      ),
      Definition = c(
        "Total number of rounds logged in the app",
        "Percentage of holes where you missed the green in regulation but still made par or better",
        "Percentage of holes where the ball is on the green in regulation (2 or more shots remaining for par when reaching the green)",
        "Percentage of par 4 and par 5 tee shots that found the fairway (Par 3's excluded)",
        "Average 18-hole score",
        "Lowest 18-hole score",
        "Fraction of shots that gained at least 0.5 strokes against the selected handicap baseline",
        "1 minus the fraction of shots that lost at least 0.5 strokes against the selected handicap baseline",
        "Percentage of holes with a score of par or better",
        "Longest streak of consecutive birdies",
        "Longest streak of consecutive pars",
        "Total number of eagles",
        "Average driving distance in yards (excluding penalty shots). Disclaimer: this is calculated as hole distance - distance remaining after tee shot in which driver was used. So, take for example a short Par 4 in which you are pin-high after your tee shot, 30 yards left of the pin. It will calculate your drive as hole distance - 30, even though technically your drive distance was the length of the hole, just left.",
        "Number of unique courses played",
        "Strokes Gained: Off the Tee. Measures tee shot performance relative to the selected handicap baseline.",
        "Strokes Gained: Approach. Measures approach shot performance relative to the selected handicap baseline.",
        "Strokes Gained: Around the Green. Measures short game performance relative to the selected handicap baseline.",
        "Strokes Gained: Putting. Measures putting performance relative to the selected handicap baseline."
      )
    ) %>%
      gt() %>%
      tab_header(title = "KPI Definitions") %>%
      cols_width(KPI ~ px(160), Definition ~ px(600)) %>%
      tab_style(
        style = cell_text(weight = "bold"),
        locations = cells_body(columns = KPI)
      ) %>%
      gt_theme_538(quiet = TRUE)
  })
  
  
  
  output$download_csv <- downloadHandler(
    filename = function() paste0("strokes_gained_data_", Sys.Date(), ".csv"),
    content = function(file) write.csv(joined_data(), file, row.names = FALSE)
  )
  
  output$scorecards <- render_gt({
    
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
    
    
    
    plot_data <- sg_summarised %>%
      filter(high_level_desc == input$strokes_gained_category) %>%
      mutate(
        selected_sg = .data[[sg_col()]],
        moving_avg  = zoo::rollapply(selected_sg, width = 20, FUN = mean, fill = NA, align = "right", partial = TRUE),
        bar_color   = case_when(
          high_level_desc == "TOTAL" & !holes %in% c(9, 18) ~ "rgba(100, 195, 220, 0.4)",
          selected_sg < 0 ~ "rgba(224, 112, 112, 0.4)",
          TRUE            ~ "rgba(100, 180, 160, 0.4)"
        )
      )
    
    year_ranges <- plot_data %>%
      group_by(year = format(date, "%Y")) %>%
      summarise(
        start_index = min(round_index),
        end_index   = max(round_index),
        mid_index   = (start_index + end_index) / 2,
        n_rounds    = n(),
        .groups = "drop"
      )
    
    year_boundaries <- year_ranges %>%
      arrange(start_index) %>%
      slice(-1) %>%
      mutate(line_pos = start_index - 0.5)
    
    year_labels <- year_ranges %>%
      filter(n_rounds >= 3)
    
    plot_ly() %>%
      add_bars(
        data = plot_data,
        x = ~round_index,
        y = ~selected_sg,
        marker = list(color = ~bar_color),
        hovertemplate = paste(
          "Round: %{customdata}<br>",
          "Date: %{hovertext}<br>",
          paste0("SG vs ", sg_col_label(), ": %{y:.2f}<br>"),
          "<extra></extra>"
        ),
        customdata = ~round_id,
        hovertext = ~format(date, "%b %d, %Y"),
        textposition = "none",
        name = "SG per Round"
      ) %>%
      add_text(
        data = plot_data,
        x = ~round_index,
        y = ~ifelse(selected_sg >= 0, selected_sg + 0.1, selected_sg - 0.1),
        text = ~strokes,
        textposition = ~ifelse(selected_sg >= 0, "top center", "bottom center"),
        textfont = list(size = 9, color = "rgba(80, 80, 80, 0.8)"),
        hoverinfo = "skip",
        showlegend = FALSE
      ) %>%
      add_lines(
        data = plot_data,
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
          tickvals  = plot_data$round_index,
          ticktext  = plot_data$course_name,
          tickangle = -90,
          title     = "",
          showgrid  = FALSE,
          zeroline  = FALSE
        ),
        yaxis = list(
          title     = paste0("Strokes Gained vs ", sg_col_label()),
          showgrid  = TRUE,
          gridcolor = "rgba(200,200,200,0.4)",
          zeroline  = TRUE,
          zerolinecolor = "black",
          zerolinewidth = 1.5
        ),
        plot_bgcolor  = "white",
        paper_bgcolor = "white",
        showlegend    = FALSE,
        bargap        = 0.2,
        margin        = list(t = 60)
      )
  })
  
  
  output$cumulative_sg <- renderPlotly({
    
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
        shot_sg = round(.data[[sg_col()]], 2),
        cumulative_sg = round(cumsum(replace_na(.data[[sg_col()]], 0)), 2)
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
  
  output$sg_by_category <- renderPlot({
    
    incremental_25_yds_performance <- sg_composite %>%
      filter(!grepl("Recovery", sg_category_25, ignore.case = TRUE)) %>%
      group_by(sg_category_25) %>%
      summarise(
        sg_total    = sum(.data[[sg_col()]], na.rm = TRUE),
        shots       = n(),
        sg_per_shot = sg_total / shots,
        .groups = "drop"
      ) %>%
      filter(shots > 1) %>%
      mutate(category_type = case_when(
        grepl("Approach",    sg_category_25, ignore.case = TRUE) ~ "APP",
        grepl("Putting",     sg_category_25, ignore.case = TRUE) ~ "PUTT",
        grepl("Off the Tee", sg_category_25, ignore.case = TRUE) ~ "OTT",
        grepl("Around",      sg_category_25, ignore.case = TRUE) ~ "ARG",
        TRUE ~ "Other"
      )) %>%
      mutate(
        category_type  = factor(category_type, levels = c("OTT", "APP", "ARG", "PUTT")),
        first_num      = as.numeric(str_extract(sg_category_25, "\\d+")),
        sg_category_25 = factor(sg_category_25, levels = sg_category_25[order(category_type, first_num)])
      )
    
    ggplot(
      data = incremental_25_yds_performance,
      aes(
        x    = sg_category_25,
        y    = sg_per_shot,
        fill = sg_per_shot > 0
      )
    ) +
      geom_col() +
      geom_text(
        aes(
          y = ifelse(
            sg_per_shot >= 0,
            sg_per_shot + 0.005,
            sg_per_shot - 0.005
          ),
          label = shots
        ),
        angle = 90,
        fontface = "bold",
        size = 3.5,
        color = "gray20"
      ) + 
      
      facet_grid(
        ~ category_type,
        scales = "free_x",
        space = "free_x"
      ) + 
      
      scale_fill_manual(
        values = c(
          "TRUE" = "#5DCAA5",
          "FALSE" = "#e07070"
        ),
        guide = "none"
      ) +
      
      labs(
        x     = "",
        y     = paste0("SG Per Shot vs ", sg_col_label()),
        title = paste0(
          "Strokes Gained per Shot (",
          sg_col_label(),
          " Baseline)"
        )
      ) +
      
      theme_minimal() +
      theme_fivethirtyeight() +
      theme(
        axis.text.x   = element_text(angle = 45, hjust = 1, size = 7),
        strip.text    = element_text(face = "bold"),
        panel.spacing.x = unit(1.2, "lines")
      ) 
  })
  
  
  output$best_and_worst_shots <- render_gt({
    
    n <- input$top_bottom_n
    
    sg_composite %>%
      filter(round_id %in% input$top_bottom_round) %>%
      mutate(
        sg_val      = .data[[sg_col()]],
        top_rank    = min_rank(desc(sg_val)),
        bottom_rank = row_number(sg_val) # break ties. a lot of ties with penalty shots being -1
      ) %>%
      mutate(shot_verbiage = case_when(
          
        start_surface == "Green" & !is.na(in_hole) ~
            paste0("Holed ", start_distance, " ft putt"),
          
          start_surface != "Green" & !is.na(in_hole) ~
            paste0("Holed out from ", start_distance, " yards in the ", start_surface),
        
          penalty == 1 & start_distance == finish_distance & start_surface == finish_surface ~
            "Out of Bounds Penalty",
        
          penalty == 1 & (start_distance != finish_distance | start_surface != finish_surface) ~
            "Penalty Drop",
        
          start_surface == "Green" & finish_surface == "Green" & is.na(in_hole) ~
            paste0("Missed putt from ", start_distance, " ft to ", finish_distance, " ft"),
          
          start_surface == "Tee" & finish_surface != "Green" ~
            paste0("Teeshot with ", club, " from ", start_distance, " yards to ", finish_distance, " yards", " in the ", finish_surface),
          
          start_surface == "Tee" & finish_surface == "Green" ~
            paste0("Teeshot with ", club, " from ", start_distance, " yards to ", finish_distance, " ft"),
      
          finish_surface == "Green" & start_surface != "Green" ~
            paste0(club, " from ", start_distance, " yards in the ", start_surface, " to ", finish_distance, " ft"),

          start_surface != "Green" & finish_surface != "Green" ~
            paste0(club, " from ", start_distance, " yards in the ", start_surface, " to ", finish_distance, " yards", " in the ", finish_surface),
          
        TRUE ~ "TBD"
        )
      ) %>%
      filter(top_rank <= n | bottom_rank <= n) %>%
      mutate(
        shot_type  = case_when(
          top_rank    <= n ~ paste0("Top ", n),
          bottom_rank <= n ~ paste0("Bottom ", n)
        ),
        shot_order = ifelse(shot_type == paste0("Top ", n), 1, 2),
        sort_sg    = case_when(
          shot_type == paste0("Top ", n)    ~ -sg_val,
          shot_type == paste0("Bottom ", n) ~  sg_val
        )
      ) %>%
      arrange(shot_order, sort_sg) %>%
      select(round_id, shot_type, hole, par, stroke, club, start, finish, sg_val, shot_verbiage) %>%
      gt(groupname_col = "shot_type") %>%
      fmt_number(columns = sg_val, decimals = 2) %>%
      cols_label(
        round_id = "Round",
        hole     = "Hole",
        par      = "Par",
        stroke   = "Stroke",
        club     = "Club",
        start    = "Start",
        finish   = "Finish",
        sg_val   = "SG"
      ) %>%
      data_color(
        columns = sg_val,
        fn = scales::col_numeric(
          palette = c("#d73027", "#fdae61", "#ffffbf", "#a6d96a", "#1a9850"),
          domain  = NULL
        )
      ) %>%
      tab_header(title = paste0("Best & Worst Shots")) %>%
      gt_theme_538(quiet = TRUE)
  })
  
  
}

shinyApp(ui, server)
