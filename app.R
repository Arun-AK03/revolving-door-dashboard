###############################################
# LIBRARIES & COMMON HELPERS
###############################################

library(shiny)
library(shinydashboard)
library(plotly)
library(dplyr)
library(readr)
library(ggplot2)
library(lubridate)
library(scales)
library(tidyr)
library(leaflet)
library(ozmaps)


# --- file paths ---
sankey_path       <- "Data/sankey_data_final.csv"
timeline_path     <- "Data/AIHW/AIHW_Client.csv"
client_group_path <- "Data/AIHW/AIHW_Client_Group.csv"
reasons_path      <- "Data/AIHW/AIHW_Reasons.csv"
core_act_path     <- "Data/Table_1_3/Table1_3_Core_Activities.csv"
HOG_path          <- "Data/Table_1_3/Table1_3_HOG.csv"
Total_path        <- "Data/Table_1_3/Table1_3_Total.csv"
Age_group_path    <- "Data/Table_1_3/Table_1_3Age_group.csv"
Indigenous_path   <- "Data/Table_1_3/Table_1_3_Indigenous_status.csv"
Sex_path          <- "Data/Table_1_3/Table_1_3_Sex.csv"
ret_client2_path  <- "Data/AIHW-HOU-339(CLIENT_2).csv"
ret_client1_path  <- "Data/AIHW-HOU-339(I_CLIENT_1).csv"
intervention_path <- "Data/intervention_sim_data.csv"



table_coreactivities<-read.csv(core_act_path,sep=",")
table_hog<-read.csv(HOG_path,sep=",")
table_Total<-read.csv(Total_path,sep=",")
table_agegroup<-read.csv(Age_group_path,sep=",")



table_agegroup[] <- lapply(table_agegroup, function(x) {
  if (is.character(x)) gsub("\\?", "-", x) else x
})

table_Indistatus<-read.csv(Indigenous_path,sep=",")
table_sex<-read.csv(Sex_path ,sep=",")

safe_read_csv <- function(path) {
  if (!file.exists(path)) stop(paste("FILE NOT FOUND:", path))
  read_csv(path, show_col_types = FALSE)
}

parse_client_group_date <- function(x) {
  x <- trimws(as.character(x))
  x <- sub("^([A-Za-z]{3})-(\\d{2})$", "\\1 20\\2", x)
  x <- sub("^([A-Za-z]+)-(\\d{4})$", "\\1 \\2", x)
  as.Date(suppressWarnings(parse_date_time(x, orders = c("b Y","B Y","Y-m"))))
}

###############################################
# DATA: SANKey (OVERVIEW KPIs + main Sankey)
###############################################

sankey_raw <- safe_read_csv(sankey_path)
state_cols <- c("NSW","Vic","Qld","WA","SA","Tas","ACT","NT","National")


sankey_long <- sankey_raw %>%
  rename(National = `National(a)`) %>%
  mutate(
    Year     = iconv(Year,     from = "", to = "UTF-8", sub = ""),
    Category = iconv(Category, from = "", to = "UTF-8", sub = "")
  ) %>%
  mutate(
    Year     = as.character(trimws(Year)),
    Category = trimws(Category)
  ) %>%
  pivot_longer(
    cols      = all_of(state_cols),
    names_to  = "state",
    values_to = "raw_value"
  ) %>%
  mutate(
    value = suppressWarnings(
      as.numeric(gsub("[^0-9.\\-]", "", as.character(raw_value)))
    )
  ) %>%
  filter(!is.na(value)) %>%
  group_by(Year, state, Category) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop")


sankey_wide <- sankey_long %>%
  pivot_wider(
    names_from  = Category,
    values_from = value
  ) %>%
  rename(
    year            = Year,
    returned        = `Clients who return to homelessness after achieving housing`,
    percent_returned = `Percentage of clients who return to homelessness after achieving housing`,
    housed          = `Clients who remainined to housed after achieving housing`,
    total_shs       = `Total SHS Client`
  ) %>%
  mutate(
    returned        = as.numeric(returned),
    percent_returned = as.numeric(percent_returned),
    housed          = as.numeric(housed),
    total_shs       = as.numeric(total_shs),
    state           = as.character(state),
    year            = as.character(year),
    year = ifelse(
      grepl("^\\d{6}$", year),
      paste0(substr(year, 1, 4), "-", substr(year, 5, 6)),
      year
    ),
    client_number   = total_shs,
    portion         = percent_returned
  )


sankey_df <- sankey_wide


###############################################
# DATA: TIMELINE
###############################################

timeline_df <- safe_read_csv(timeline_path) %>%
  rename(
    month_year = `Month and Year`,
    state_NSW  = NSW,
    state_Vic  = Vic,
    state_Qld  = Qld,
    state_WA   = WA,
    state_SA   = SA,
    state_Tas  = Tas,
    state_ACT  = ACT,
    state_NT   = NT,
    national   = National
  ) %>%
  mutate(
    month_year = trimws(month_year),
    date       = as.Date(parse_date_time(month_year, orders = c("b Y","B Y","Y-m"))),
    across(
      c(national, state_NSW, state_Vic, state_Qld, state_WA, state_SA,
        state_Tas, state_ACT, state_NT),
      ~ as.numeric(gsub(",", "", .x))
    )
  ) %>%
  arrange(date)

if (all(is.na(timeline_df$date))) {
  timeline_df <- timeline_df %>%
    mutate(month_year_ord = factor(month_year, levels = unique(month_year)))
} else {
  order_df <- timeline_df %>% distinct(month_year, date) %>% arrange(date)
  timeline_df <- timeline_df %>%
    left_join(order_df, by = c("month_year","date")) %>%
    mutate(month_year_ord = factor(month_year, levels = order_df$month_year))
}

###############################################
# DATA: CLIENT GROUP (STACKED BAR)
###############################################

client_group_df <- safe_read_csv(client_group_path) %>%
  rename(
    month_year   = `Month and Year`,
    month        = Month,
    year         = Year,
    sex          = Sex,
    client_group = `Client Group`,
    state_NSW    = NSW,
    state_Vic    = Vic,
    state_Qld    = Qld,
    state_WA     = WA,
    state_SA     = SA,
    state_Tas    = Tas,
    state_ACT    = ACT,
    state_NT     = NT,
    national     = National
  ) %>%
  mutate(
    month_year   = trimws(month_year),
    date         = parse_client_group_date(month_year),
    sex          = trimws(as.character(sex)),
    client_group = trimws(as.character(client_group)),
    across(c(state_NSW, state_Vic, state_Qld, state_WA, state_SA,
             state_Tas, state_ACT, state_NT, national),
           ~ as.numeric(gsub(",", "", .x)))
  ) %>%
  filter(!is.na(client_group), !is.na(sex))

###############################################
# DATA: REASONS (ALLUVIAL TAB)
###############################################

reasons_raw <- safe_read_csv(reasons_path) %>%
  rename(
    month_year = `Month and Year`,
    reason     = `Reason for seeking assistance`,
    group      = Group,
    national   = National
  ) %>%
  mutate(
    month_year = trimws(month_year),
    date       = parse_client_group_date(month_year),
    reason     = trimws(reason),
    group      = trimws(group),
    national   = as.numeric(gsub(",", "", national))
  ) %>%
  filter(!is.na(date), !is.na(national), national > 0)

valid_groups <- c("Accommodation","Financial","Interpersonal","Health","Other")


reasons_df <- reasons_raw %>%
  filter(group %in% valid_groups) %>%
  mutate(
    fin_year = ifelse(
      month(date) >= 7,
      paste0(year(date), "-", substr(year(date) + 1, 3, 4)),
      paste0(year(date) - 1, "-", substr(year(date), 3, 4))
    )
  )

latest_year <- sankey_df$year[which.max(nchar(sankey_df$year))]  
if (length(latest_year) == 0) latest_year <- sort(unique(sankey_df$year))[1]

###############################################
# DATA: Table_total (Maps)
###############################################
year_map <- sort(unique(table_Total$Year))


core_activities_long <- table_coreactivities %>%
  dplyr::rename(core_activity = 2) %>%   
  dplyr::mutate(
    Year_chr = as.character(Year)
  ) %>%
  tidyr::pivot_longer(
    cols = c(NSW, Vic., Qld, SA, WA, Tas., NT, ACT),
    names_to  = "state_col",
    values_to = "value"
  ) %>%
  dplyr::mutate(
    state_name = dplyr::recode(
      state_col,
      "NSW"  = "New South Wales",
      "Vic." = "Victoria",
      "Qld"  = "Queensland",
      "SA"   = "South Australia",
      "WA"   = "Western Australia",
      "Tas." = "Tasmania",
      "ACT"  = "Australian Capital Territory",
      "NT"   = "Northern Territory"
    ),
    value = as.numeric(gsub(",", "", value))
  )


hog_long <- table_hog %>%
  dplyr::rename(hog_group = 2) %>%
  dplyr::mutate(
    Year_chr = as.character(Year),
    dplyr::across(c(NSW, Vic., Qld, SA, WA, Tas., NT, ACT), as.character)
  ) %>%
  tidyr::pivot_longer(
    cols = c(NSW, Vic., Qld, SA, WA, Tas., NT, ACT),
    names_to  = "state_col",
    values_to = "value"
  ) %>%
  dplyr::mutate(
    state_name = dplyr::recode(
      state_col,
      "NSW"  = "New South Wales",
      "Vic." = "Victoria",
      "Qld"  = "Queensland",
      "SA"   = "South Australia",
      "WA"   = "Western Australia",
      "Tas." = "Tasmania",
      "ACT"  = "Australian Capital Territory",
      "NT"   = "Northern Territory"
    ),
    value = as.numeric(gsub(",", "", value))
  )



agegroup_long <- table_agegroup %>%
  dplyr::rename(age_group = 2) %>%
  dplyr::mutate(
    Year_chr = as.character(Year),
    dplyr::across(c(NSW, Vic., Qld, SA, WA, Tas., NT, ACT), as.character)
  ) %>%
  tidyr::pivot_longer(
    cols = c(NSW, Vic., Qld, SA, WA, Tas., NT, ACT),
    names_to  = "state_col",
    values_to = "value"
  ) %>%
  dplyr::mutate(
    state_name = dplyr::recode(
      state_col,
      "NSW"  = "New South Wales",
      "Vic." = "Victoria",
      "Qld"  = "Queensland",
      "SA"   = "South Australia",
      "WA"   = "Western Australia",
      "Tas." = "Tasmania",
      "ACT"  = "Australian Capital Territory",
      "NT"   = "Northern Territory"
    ),
    value = as.numeric(gsub(",", "", value))
  )

indistatus_long <- table_Indistatus %>%
  dplyr::rename(indig_status = 2) %>%
  dplyr::mutate(
    Year_chr = as.character(Year),
    dplyr::across(c(NSW, Vic., Qld, SA, WA, Tas., NT, ACT), as.character)
  ) %>%
  tidyr::pivot_longer(
    cols = c(NSW, Vic., Qld, SA, WA, Tas., NT, ACT),
    names_to  = "state_col",
    values_to = "value"
  ) %>%
  dplyr::mutate(
    state_name = dplyr::recode(
      state_col,
      "NSW"  = "New South Wales",
      "Vic." = "Victoria",
      "Qld"  = "Queensland",
      "SA"   = "South Australia",
      "WA"   = "Western Australia",
      "Tas." = "Tasmania",
      "ACT"  = "Australian Capital Territory",
      "NT"   = "Northern Territory"
    ),
    value = as.numeric(gsub(",", "", value))
  )


sex_long <- table_sex %>%
  dplyr::rename(sex_cat = 2) %>%
  dplyr::mutate(
    Year_chr = as.character(Year),
    dplyr::across(c(NSW, Vic., Qld, SA, WA, Tas., NT, ACT), as.character)
  ) %>%
  tidyr::pivot_longer(
    cols = c(NSW, Vic., Qld, SA, WA, Tas., NT, ACT),
    names_to  = "state_col",
    values_to = "value"
  ) %>%
  dplyr::mutate(
    state_name = dplyr::recode(
      state_col,
      "NSW"  = "New South Wales",
      "Vic." = "Victoria",
      "Qld"  = "Queensland",
      "SA"   = "South Australia",
      "WA"   = "Western Australia",
      "Tas." = "Tasmania",
      "ACT"  = "Australian Capital Territory",
      "NT"   = "Northern Territory"
    ),
    value = as.numeric(gsub(",", "", value))
  )

###############################################
# DATA: Risk Calculator Slide 3
###############################################

states <- c("NSW", "Vic", "Qld", "WA", "SA", "Tas", "ACT", "NT")


age_raw <- read.csv(ret_client1_path,sep=",",stringsAsFactors = FALSE)
cohort_raw <- read.csv(ret_client2_path,sep=",",stringsAsFactors = FALSE)


age_feat <- age_raw %>%
  filter(
    Data.type == "Proportion of clients that avoided homelessness", 
    Sex %in% c("Males", "Females")
  ) %>%
  mutate(
    across(
      c(all_of(states), National.a.),
      ~ na_if(.x, "n.p.")
    )) %>%
  mutate(
    across(
      all_of(states),
      ~ dplyr::coalesce(.x, National.a.)
    ))%>%
  pivot_longer(
    cols      = all_of(states),
    names_to  = "State",
    values_to = "prop_housed"
  ) %>%
  mutate(
    prop_housed = suppressWarnings(
      as.numeric(gsub("[^0-9.]", "", prop_housed))
    ),
    prop_return = 100 - prop_housed
  ) %>%
  filter(Age.group != "Total") %>%
  select(Year, Sex, Age.group, State, prop_return)


cohort_feat <- cohort_raw %>%
  filter(
    Data.type == "Proportion of clients that avoided homelessness", 
    Sex %in% c("Males", "Females")
  ) %>%
  mutate(
    across(
      c(all_of(states), National.a.),
      ~ na_if(.x, "n.p.")
    )) %>%
  mutate(
    across(
      all_of(states),
      ~ dplyr::coalesce(.x, National.a.)
    ))%>%
  pivot_longer(
    cols      = all_of(states),
    names_to  = "State",
    values_to = "prop_housed"
  ) %>%
  mutate(
    prop_housed = suppressWarnings(
      as.numeric(gsub("[^0-9.]", "", prop_housed))
    ),
    prop_return = 100 - prop_housed
  ) %>%
  select(Year, Sex, NHHA.priority.cohort, State, prop_return)


logit     <- function(p) log(p / (1 - p))
inv_logit <- function(x) 1 / (1 + exp(-x))

age_model_data <- age_feat %>%
  mutate(
    p       = prop_return / 100,
    logit_p = logit(p),
    Year_num = as.numeric(substr(Year, 1, 4)),
    Sex       = factor(Sex),
    Age.group = factor(Age.group),
    State     = factor(State)
  )

age_glm <- lm(
  logit_p ~ Sex + Age.group + State + Year_num,
  data = age_model_data
)


cohort_model_data <- cohort_feat %>%
  mutate(
    p       = prop_return / 100,
    logit_p = logit(p),
    Year_num = as.numeric(substr(Year, 1, 4)),
    Sex          = factor(Sex),
    Cohort.group = factor(NHHA.priority.cohort),
    State        = factor(State)
  )

cohort_glm <- lm(
  logit_p ~ Sex + Cohort.group + State + Year_num,
  data = cohort_model_data
)

risk_year_choices   <- sort(unique(age_feat$Year))
risk_state_choices  <- states
risk_sex_choices    <- c("Males", "Females")
risk_age_choices    <- levels(age_model_data$Age.group)
risk_cohort_choices <- levels(cohort_model_data$Cohort.group)


age_coef_df <- data.frame(
  term = rownames(coef(summary(age_glm))),
  as.data.frame(coef(summary(age_glm))),
  row.names = NULL
)
names(age_coef_df) <- c("term","Estimate","Std.Error","t.value","p.value")
age_coef_df$abs_t <- abs(age_coef_df$t.value)

cohort_coef_df <- data.frame(
  term = rownames(coef(summary(cohort_glm))),
  as.data.frame(coef(summary(cohort_glm))),
  row.names = NULL
)
names(cohort_coef_df) <- c("term","Estimate","Std.Error","t.value","p.value")
cohort_coef_df$abs_t <- abs(cohort_coef_df$t.value)

###############################################
# DATA: Intervention Simulator Slide 3
###############################################
intervention_df <- read.csv(intervention_path, stringsAsFactors = FALSE)

intervention_model <- lm(Return.ratio ~ ShortTerm_Accomdation_Coverage + ShortTerm_Financial_Assistance_Coverage +
    Share_Family_Domestic_Violence, data = intervention_df
)



###############################################
# UI
###############################################

ui <- dashboardPage(
  dashboardHeader(title = "The Revolving Door"),
  
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",   
      menuItem(" Overview", tabName = "overview", icon = icon("home")),
      menuItem(" Pathways & Demographics", tabName = "alluvial", icon = icon("project-diagram")),
      menuItem(" Risk & Response", tabName = "riskresp", icon = icon("lightbulb"))
    ),
    
    br(),
    
    # Year Selector for Sankey + Alluvial+KPI
    conditionalPanel(
      condition = "input.tabs == 'overview' || input.tabs == 'alluvial'",
      selectInput(
        "yearSelect",
        "Select Year (Sankey/Alluvial/KPI):",
        choices  = if (length(unique(na.omit(sankey_wide$year))) > 0)
          sort(unique(na.omit(sankey_wide$year))) else "",
        selected = if (length(unique(na.omit(sankey_wide$year))) > 0)
          max(unique(na.omit(sankey_wide$year))) else ""
      )
    ),
    
    # Territory selector
    conditionalPanel(
      condition = "input.tabs == 'overview'",
      selectInput(
        "territorySelect",
        "Select Territory (Sankey / Timeline / Stacked Bar):",
        choices  = c("National","NSW","Vic","Qld","WA","SA","Tas","ACT","NT"),
        selected = "National"
      )
    ),
    
    # Year for Australia Demographic Maps 
    conditionalPanel(
      condition = "input.tabs == 'alluvial'",
      selectInput(
        "Yearformap",
        "Select Year (Australia Demographic Maps):",
        choices  = sort(unique(table_Total$Year)),
        selected = year_map
      )
    )),
  
  dashboardBody(
    tags$head(tags$style(HTML("
      .small-box .inner h3 {
        font-size: 44px !important;
        font-weight: 800 !important;
        margin: 0 0 8px 0 !important;
        line-height: 1.0 !important;
      }
      .small-box .inner p {
        font-size: 18px !important;
        margin: 0 !important;
        font-weight: 600 !important;
      }
      .small-box { min-height: 130px !important; }
    "))),
    
    tabItems(
      
      #################### OVERVIEW TAB ####################
      tabItem(
        tabName = "overview",
        
        fluidRow(
          valueBoxOutput("twelveMonth",  width = 3),
          valueBoxOutput("medianReturn", width = 3),
          valueBoxOutput("percentDV",    width = 3),
          valueBoxOutput("housedCohort", width = 3)
        ),
        
        fluidRow(
          box(width = 6, plotlyOutput("sankeyPlot",  height = "500px")),
          box(width = 6, plotOutput("stackedBars", height = "500px"))
        ),
        
        fluidRow(
          box(width = 12, plotOutput("timelinePlot", height = "450px"))
        )
      ),
      
      ################ PATHWAYS / ALLUVIAL TAB #############
      tabItem(
        tabName = "alluvial",
        
        fluidRow(
          box(
            width = 12,
            title = "Reason → Assistance Group → Outcome",
            status = "primary",
            solidHeader = TRUE,
            plotlyOutput("alluvial_plot", height = "600px")
          ),
          ################ PATHWAYS / Maps TAB ############# 
          
          fluidRow(
            box(
              width = 12,
              title = "Australian Map",
              status = "primary",
              solidHeader = TRUE,
              leafletOutput("map", height = 600)
            )
          ),
          ################ PATHWAYS / KPI Maps TAB ############# 
          fluidRow(
            box(
              width = 12,
              title = "Sub Indices Map",
              status = "primary",
              solidHeader = TRUE,
              tableOutput("sub_indices_summary"),
              br(),
              fluidRow(
                column(
                  width = 3,
                  actionButton("sub_prev", "Previous")
                ),
                column(
                  width = 6,
                  div(
                    style = "text-align:center; font-weight:bold;",
                    textOutput("sub_graph_title")
                  )
                ),
                column(
                  width = 3,
                  div(style = "text-align:right;",
                      actionButton("sub_next", "Next")
                  )
                )
              ),
              br(),
              plotOutput("sub_index_plot", height = "400px")
            )
          )
        )
      ),
      
      
      
      #################### Risk & Resposnse TAB ####################
      tabItem(
        tabName = "riskresp",
        fluidRow(
          box(
            width = 6,
            title = "Intervention Simulator",
            status = "primary",
            solidHeader = TRUE,
            
            p("Choose a state and change coverage of short-term supports to see the modelled impact on returns to homelessness."),
            
            selectInput(
              "sim_state",
              "Territory",
              choices  = unique(intervention_df$State),
              selected = "Vic"
            ),
            
            sliderInput(
              "sim_accom_mult",
              "Short-term accommodation coverage (× current level)",
              min = 0.5, max = 1.5, value = 1, step = 0.05
            ),
            
            sliderInput(
              "sim_fin_mult",
              "Short-term financial assistance coverage (× current level)",
              min = 0.5, max = 1.5, value = 1, step = 0.05
            ),
            
            actionButton("sim_run", "Run intervention scenario", icon = icon("play")),
            br(), br(),
            
            htmlOutput("sim_summary"),
            tableOutput("sim_table")
          ),
          
          box(
            width = 6,
            title = "Risk Calculator & Model Summaries",
            status = "warning",
            solidHeader = TRUE,
            
            tabsetPanel(
              id = "rc_view",
              type = "hidden",
              selected = "calc", 
              

              tabPanel(
                title = "Risk calculator",
                value = "calc", 
                h4("Client profile", align = "left"),
                
                fluidRow(
                  column(
                    width = 8,
                    selectInput("rc_state",  "Territory",
                                choices = risk_state_choices, selected = "Vic"),
                    selectInput("rc_sex",    "Gender",
                                choices = risk_sex_choices,   selected = "Females"),
                    selectInput("rc_age",    "Age group",
                                choices = risk_age_choices),
                    selectInput("rc_cohort", "Cohort group",
                                choices = risk_cohort_choices),
                    
                    br(),
                    actionButton("rc_run", "Calculate risk", icon = icon("play"))
                  ),
                  
                  column(
                    width = 4,
                    div(
                      style = "margin-top: 40px; text-align: center; border-left: 1px solid #eee; padding-left: 10px;",
                      div(
                        style = "font-size: 12px; font-weight: 600; text-transform: uppercase; margin-bottom: 4px;",
                        "Predicted 12-month return risk"
                      ),
                      div(
                        style = "font-size: 26px; font-weight: 800;",
                        textOutput("rc_risk_pct")
                      ),
                      div(
                        style = "font-size: 11px; color: #666; margin-top: 2px;",
                        "(2024 scenario)"
                      )
                    )
                  )
                ),
                
                hr(),
                
                h4("Narrative conclusion", align = "left"),
                htmlOutput("rc_conclusion_text")
              ),
              

              tabPanel(
                title = "age",
                value = "age",
                
                h4("Age model summary", align = "left"),
                verbatimTextOutput("rc_age_summary")
              ),
              

              tabPanel(
                title = "cohort",
                value = "cohort",
                
                h4("Cohort model summary", align = "left"),
                verbatimTextOutput("rc_cohort_summary")
              )
            ),
            
            br(),

            fluidRow(
              column(
                width = 12,
                div(
                  style = "text-align:right;",
                  actionButton("rc_show_calc",   "Calculator"),
                  actionButton("rc_show_age",    "Age model"),
                  actionButton("rc_show_cohort", "Cohort model")
                )
              )
            )
          )
        )
      )
      #################### ######## ####################
    )
  )
)  


###############################################
# SERVER
###############################################

server <- function(input, output, session) {
  
# ================== OVERVIEW REACTIVES ==================
  sankey_row <- reactive({
    req(input$yearSelect, input$territorySelect)
    
    row <- sankey_wide %>%
      filter(year == input$yearSelect & state == input$territorySelect)
    
    if (nrow(row) == 0) {
      year_alt <- gsub("\u2013|\u2014", "-", input$yearSelect)
      row <- sankey_wide %>%
        filter(gsub("\u2013|\u2014", "-", year) == year_alt &
                 state == input$territorySelect)
    }
    
    row
  })
  

  filteredData <- reactive({
    row <- sankey_row()
    
    client_number <- if ("total_shs" %in% names(row)) row$total_shs else NA_real_
    returned <- if ("returned" %in% names(row)) row$returned else NA_real_
    housed <- if ("housed" %in% names(row)) row$housed else NA_real_
    portion <- if ("percent_returned" %in% names(row)) row$percent_returned else NA_real_
    
    tibble::tibble(
      client_number = client_number,
      returned = returned,
      housed = housed,
      portion = portion
    )
  })
  
  output$twelveMonth <- renderValueBox({
    data <- filteredData()
    avg_return <- NA_real_
    if (!is.na(data$returned) && !is.na(data$housed) && data$housed > 0) {
      avg_return <- (data$returned / data$housed) * 100
    } else if (!is.na(data$portion)) {
      avg_return <- data$portion
    }
    avg_return <- if (is.nan(avg_return)) NA else avg_return
    valueBox(
      ifelse(is.na(avg_return), "N/A", paste0(round(avg_return, 1), "%")),
      "12 Month Return",
      color = "light-blue",
      icon = icon("calendar")
    )
  })
  
  output$medianReturn <- renderValueBox({
    data <- filteredData()
    med_return <- NA_real_
    if (!is.na(data$returned) && !is.na(data$housed) && data$housed > 0) {
      med_return <- (data$returned / data$housed) * 100
    } else if (!is.na(data$portion)) {
      med_return <- data$portion
    }
    med_return <- if (is.nan(med_return)) NA_real_ else med_return
    valueBox(
      ifelse(is.na(med_return), "N/A", paste0(round(med_return, 1), "%")),
      "Median 12-month Return %",
      color = "yellow",
      icon = icon("clock")
    )
  })
  
  output$percentDV <- renderValueBox({
    data <- filteredData()
    pct_dv <- NA_real_
    if (!is.na(data$portion)) {
      pct_dv <- data$portion
    } else if (!is.na(data$returned) && !is.na(data$client_number) && data$client_number > 0) {
      pct_dv <- (data$returned / data$client_number) * 100
    }
    valueBox(
      ifelse(is.na(pct_dv), "N/A", paste0(round(pct_dv, 1), "%")),
      "Average portion of clients",
      color = "red",
      icon = icon("venus-mars")
    )
  })
  
  output$housedCohort <- renderValueBox({
    data <- filteredData()
    total_housed <- data$housed
    valueBox(
      ifelse(is.na(total_housed) || total_housed == 0, "0", format(total_housed, big.mark = ",")),
      "Housed Cohort",
      color = "green",
      icon = icon("users")
    )
  })
  
# Sankey Housed vs Returned
output$sankeyPlot <- renderPlotly({
  row <- sankey_row()
  # Ensure numeric
  total_housed   <- suppressWarnings(as.numeric(row$housed))
  total_returned <- suppressWarnings(as.numeric(row$returned))
  
  if (is.na(total_housed) && is.na(total_returned)) {
    validate(need(FALSE, "No Housed/Returned values for selected Year/State"))
  }
  total_housed   <- ifelse(is.na(total_housed), 0, total_housed)
  total_returned <- ifelse(is.na(total_returned), 0, total_returned)
  still_housed   <- total_housed - total_returned
  still_housed   <- ifelse(is.na(still_housed) || still_housed < 0, 0, still_housed)
  
  nodes <- data.frame(name = c("Housed","Still Housed","Returned"))
  links <- data.frame(
    source = c(0, 0),
    target = c(1, 2),
    value  = c(still_housed, total_returned)
  )
  validate(need(sum(links$value, na.rm = TRUE) > 0, "No flow values for selected Year/State"))
  
  plot_ly(
    type = "sankey",
    orientation = "h",
    node = list(
      label     = nodes$name,
      color     = c("#2E86AB","#28B463","#E74C3C"),
      pad       = 25,
      thickness = 25,
      line      = list(color = "black", width = 0.6)
    ),
    link = list(
      source = links$source,
      target = links$target,
      value  = links$value,
      color  = c("rgba(46,134,171,0.4)","rgba(231,76,60,0.4)")
    )
  ) %>%
    layout(
      title = list(
        text = paste("Housing Flow in", input$yearSelect, "-", input$territorySelect),
        x    = 0.5,
        font = list(size = 16, color = "#333333")
      ),
      font = list(size = 13, color = "#2C3E50")
    )%>%
    config(displayModeBar = FALSE) 
})
  
  # Timeline Graph
  territoryData <- reactive({
    req(input$territorySelect)
    
    df <- timeline_df %>%
      filter(tolower(trimws(`Age group`)) == "all ages")
    
    y_col <- switch(input$territorySelect,
                    "National" = "national",
                    "NSW"      = "state_NSW",
                    "Vic"      = "state_Vic",
                    "Qld"      = "state_Qld",
                    "WA"       = "state_WA",
                    "SA"       = "state_SA",
                    "Tas"      = "state_Tas",
                    "ACT"      = "state_ACT",
                    "NT"       = "state_NT")
    
    df %>%
      select(month_year, month_year_ord, date, clients = all_of(y_col)) %>%
      mutate(clients = as.numeric(clients)) %>%
      filter(!is.na(clients) & is.finite(clients))
  })
  
  output$timelinePlot <- renderPlot({
    df <- territoryData()
    validate(need(nrow(df) > 0, "No timeline data available"))
    
    if (!all(is.na(df$date))) {
      df <- df %>% arrange(date)
      ggplot(df, aes(x = date, y = clients, group = 1)) +
        geom_line(size = 1.1) +
        geom_point(size = 2) +
        geom_smooth(method = "loess", se = FALSE, linetype = "dashed") +
        scale_x_date(date_labels = "%b %Y") +
        labs(
          title = paste("Return Timeline -", input$territorySelect),
          x = "Month and Year",
          y = "Number of Clients"
        ) +
        theme_minimal(base_size = 14) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    } else {
      df <- df %>% arrange(month_year_ord)
      ggplot(df, aes(x = month_year_ord, y = clients, group = 1)) +
        geom_line(size = 1.1) +
        geom_point(size = 2) +
        labs(
          title = paste("Return Timeline -", input$territorySelect),
          x = "Month and Year",
          y = "Number of Clients"
        ) +
        theme_minimal(base_size = 14) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
  })
  
  # Stacked bar Graph
  stackedData <- reactive({
    req(input$territorySelect)
    
    y_col <- switch(input$territorySelect,
                    "National" = "national",
                    "NSW"      = "state_NSW",
                    "Vic"      = "state_Vic",
                    "Qld"      = "state_Qld",
                    "WA"       = "state_WA",
                    "SA"       = "state_SA",
                    "Tas"      = "state_Tas",
                    "ACT"      = "state_ACT",
                    "NT"       = "state_NT")
    
    dat_sb <- client_group_df %>%
      filter(!is.na(date)) %>%
      select(date, client_group, sex, value = all_of(y_col)) %>%
      filter(!is.na(value))
    
    dat_sb <- dat_sb %>%
      mutate(
        reason = case_when(
          grepl("problematic drug or alcohol", client_group, ignore.case = TRUE) ~
            "problematic drug or alcohol issues",
          grepl("financially assisted with payments for short term/emergency",
                client_group, ignore.case = TRUE) ~
            "financially assisted with payments for short term/emergency",
          grepl("accommodated in short[- ]?term/emergency accommodation",
                client_group, ignore.case = TRUE) ~
            "in short-term/emergency accommodation",
          grepl("current mental health issue", client_group, ignore.case = TRUE) ~
            "mental health issue",
          grepl("Indigenous", client_group, ignore.case = TRUE) ~
            "indigenous clients",
          grepl("experienced family and domestic violence",
                client_group, ignore.case = TRUE) ~
            "experienced family and domestic violence",
          grepl("at risk of homelessness", client_group, ignore.case = TRUE) ~
            "at risk of homelessness",
          grepl("who are homeless", client_group, ignore.case = TRUE) ~
            "homeless",
          TRUE ~ client_group
        )
      )
    
    reason_levels <- c(
      "problematic drug or alcohol issues",
      "financially assisted with payments for short term/emergency",
      "in short-term/emergency accommodation",
      "mental health issue",
      "indigenous clients",
      "experienced family and domestic violence",
      "at risk of homelessness",
      "homeless"
    )
    
    dat_sb <- dat_sb %>%
      filter(reason %in% reason_levels) %>%
      group_by(reason, sex) %>%
      mutate(latest_date = max(date)) %>%
      filter(date == latest_date) %>%
      summarise(value = sum(value, na.rm = TRUE), .groups = "drop")
    
    dat_sb$reason <- factor(dat_sb$reason,
                            levels = reason_levels[reason_levels %in% dat_sb$reason])
    dat_sb
  })
  
  output$stackedBars <- renderPlot({
    dat_sb <- stackedData()
    validate(need(nrow(dat_sb) > 0,
                  "No Client Group data available."))
    
    dat_sb_plot <- dat_sb %>% mutate(value_1000 = value / 1000)
    
    order_df <- dat_sb_plot %>%
      group_by(reason) %>%
      summarise(total = sum(value_1000, na.rm = TRUE), .groups = "drop")
    
    reason_order <- order_df %>% arrange(total) %>% pull(reason)
    
    dat_sb_plot <- dat_sb_plot %>%
      mutate(
        reason = factor(reason, levels = reason_order),
        sex    = factor(sex, levels = c("Total","Male","Female"))
      )
    
    ggplot(dat_sb_plot, aes(x = value_1000, y = reason, fill = sex)) +
      geom_col(alpha = 0.9) +
      geom_text(
        aes(label = round(value_1000, 1)),
        position = position_stack(vjust = 0.5),
        size = 3,
        color = "white"
      ) +
      scale_x_continuous(labels = label_number(accuracy = 1)) +
      scale_fill_manual(
        values = c("Female" = "#e41a1c",
                   "Male"   = "#377eb8",
                   "Total"  = "#4daf4a"),
        breaks = c("Total","Male","Female")
      ) +
      labs(
        title = paste0("Monthly Clients by Reason (", input$territorySelect, ")"),
        x     = "Number of clients (thousands)",
        y     = "Client Group (Reason)"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        panel.grid.major.y = element_blank(),
        panel.grid.minor   = element_blank(),
        axis.text.x        = element_text(size = 10),
        axis.text.y        = element_text(size = 10)
      )
  })
  
  # ================== ALLUVIAL TAB ==================
  
  clean_reasons <- reactive({
    reasons_df %>%
      mutate(
        reason_clean = trimws(tolower(reason)),
        group_clean  = trimws(tolower(group))
      ) %>%
      filter(
        !grepl("total", reason_clean),
        !grepl("total", group_clean),
        !(reason_clean == "total clients"),
        !(group_clean  == "total clients"),
        !(reason_clean == "other" & group_clean == "other")
      ) %>%
      select(-reason_clean, -group_clean)
  })
  
  alluvial_data <- reactive({
    req(input$yearSelect)
    
    fy <- input$yearSelect           
    df <- clean_reasons() %>%
      filter(fin_year == fy)
    
    validate(need(nrow(df) > 0, "No alluvial data for selected year"))
    
    df <- df %>% mutate(Count = national)
    
 
    sank_row <- sankey_df %>% filter(year == fy)
    validate(need(nrow(sank_row) > 0, "No summary data for selected year"))
    
    total_clients  <- sank_row$client_number[1]
    total_housed   <- sank_row$housed[1]
    total_returned <- sank_row$returned[1]
    
    p_housed   <- ifelse(total_clients > 0, total_housed   / total_clients, 0.8)
    p_returned <- ifelse(total_clients > 0, total_returned / total_clients, 0.2)
    
    df <- df %>%
      mutate(
        OutcomeHoused   = round(Count * p_housed),
        OutcomeReturned = round(Count * p_returned)
      )
    

    links_rg <- df %>%
      transmute(source = reason, target = group, value = Count)
    
    links_go <- bind_rows(
      df %>% transmute(source = group, target = "Housed",   value = OutcomeHoused),
      df %>% transmute(source = group, target = "Returned", value = OutcomeReturned)
    )
    
    links <- bind_rows(links_rg, links_go) %>%
      filter(value > 0)
    
    nodes <- data.frame(name = unique(c(links$source, links$target)))
    links$IDsource <- match(links$source, nodes$name) - 1
    links$IDtarget <- match(links$target, nodes$name) - 1
    

    links <- links[links$IDsource != links$IDtarget, ]
    
    list(nodes = nodes, links = links)
  })
  
  output$alluvial_plot <- renderPlotly({
    sank <- alluvial_data()
    id_housed   <- which(sank$nodes$name == "Housed")   - 1
    id_returned <- which(sank$nodes$name == "Returned") - 1
    
 
    link_cols <- ifelse(
      sank$links$IDtarget == id_housed,
      "rgba(46, 204, 113, 0.8)",          
      ifelse(
        sank$links$IDtarget == id_returned,
        "rgba(231, 76, 60, 0.8)",         
        "rgba(160, 160, 160, 0.7)"        
      )
    )
    
    plot_ly(
      type = "sankey",
      arrangement = "snap",
      node = list(
        label     = sank$nodes$name,
        pad       = 20,
        thickness = 15,
        line      = list(color = "rgba(0,0,0,0.1)", width = 0.3)
      ),
      link = list(
        source = sank$links$IDsource,
        target = sank$links$IDtarget,
        value  = sank$links$value,
        color  = link_cols
      )
    ) %>%
      layout(
        title = paste0("Reason → Assistance Group → Outcome (", input$yearSelect, ")")
      ) %>%
      config(displayModeBar = FALSE)  
  })
  
  # ================== Australian Heatmaps ==================

  aus_states <- ozmaps::ozmap_states
  
  map_data <- reactive({
    req(input$Yearformap)
    

    totals_row <- table_Total %>%
      dplyr::filter(Year == input$Yearformap)   
    
    validate(need(nrow(totals_row) == 1,
                  "No totals found for the selected year"))

    state_totals <- totals_row %>%
      dplyr::transmute(
        `New South Wales`              = NSW,
        Victoria                       = Vic.,
        Queensland                     = Qld,
        `South Australia`              = SA,
        `Western Australia`            = WA,
        Tasmania                       = Tas.,
        `Australian Capital Territory` = ACT,
        `Northern Territory`           = NT
      ) %>%
      tidyr::pivot_longer(
        cols      = dplyr::everything(),
        names_to  = "NAME",
        values_to = "total"
      )

    aus_states %>%
      dplyr::left_join(state_totals, by = "NAME")
  })
  
  selected_state <- reactiveVal(NULL)
  
  observeEvent(input$map_shape_click, {
    click <- input$map_shape_click
    if (is.null(click$id)) return()
    selected_state(click$id)
  })
  
  
  output$map <- renderLeaflet({
    df <- map_data()
    
    leaflet(df) %>%
      addTiles() %>%
      addPolygons(
        fillColor    = "lightblue",
        color        = "black",
        weight       = 1,
        fillOpacity  = 0.5,
        layerId      = ~NAME,   
        label = ~paste0(
          NAME,
          " | Year: ", input$Yearformap,
          " | Total: ",
          ifelse(
            is.na(total),
            "No data",
            format(total, big.mark = ",")
          )
        ),
        highlightOptions = highlightOptions(
          weight = 3,
          color  = "red",
          bringToFront = TRUE
        )
      )
  })
  ##############         KPI Maps    ###############
  output$sub_indices_summary <- renderTable({
    df <- map_data()              
    st <- selected_state()
    yr <- input$Yearformap
    
    if (is.null(st)) {
      return(data.frame(
        State = "Click a state on the map",
        Year = "",
        Total = ""
      ))
    }
    
    row <- df[df$NAME == st, ]
    tot <- row$total[1]
    total_str <- ifelse(is.na(tot), "No data", format(tot, big.mark = ","))
    

    data.frame(
      State = st,
      Year = yr,
      Total = total_str,
      check.names = FALSE
    )
  })
  

  current_sub_plot <- reactiveVal(1)
  
  observeEvent(input$sub_next, {
    i <- current_sub_plot() + 1
    if (i > 5) i <- 1
    current_sub_plot(i)
  })
  
  observeEvent(input$sub_prev, {
    i <- current_sub_plot() - 1
    if (i < 1) i <- 5
    current_sub_plot(i)
  })
  
  output$sub_graph_title <- renderText({
    i <- current_sub_plot()
    switch(
      as.character(i),
      "1" = "Core Activity Needs",
      "2" = "Homeless Operational Groups (HOG)",
      "3" = "Age Groups",
      "4" = "Indigenous Status",
      "5" = "Sex",
      ""
    )
  })
  
  output$sub_index_plot <- renderPlot({
    st <- selected_state()
    yr <- input$Yearformap
    
    validate(
      need(!is.null(st), "Click a state on the map to see sub-index graphs")
    )
    
    i <- current_sub_plot()
    
    # 1) Core Activities
    if (i == 1) {
      df_core <- core_activities_long %>%
        dplyr::filter(
          state_name == st,
          Year_chr   == as.character(yr)
        )
      
      validate(
        need(nrow(df_core) > 0,
             paste("No Core Activities data for", st, "in year", yr))
      )
      
      df_core <- df_core %>%
        dplyr::mutate(
          core_activity = factor(
            core_activity,
            levels = core_activity[order(value, decreasing = TRUE)]
          )
        )
      
      ggplot(df_core, aes(x = core_activity, y = value)) +
        geom_col(aes(fill = core_activity)) +
        geom_text(
          aes(label = format(value, big.mark = ",")),
          hjust = -0.1,
          size  = 3
        ) +
        coord_flip() +
        expand_limits(y = max(df_core$value, na.rm = TRUE) * 1.1) +
        labs(
          title = paste0("Core activity needs in ", st, " (", yr, ")"),
          x     = "Core activity need for assistance",
          y     = "Number of clients"
        ) +
        theme_minimal(base_size = 13) +
        theme(axis.text.y = element_text(hjust = 1),
              legend.position = "none")
      
      # 2) HOG
    } else if (i == 2) {
      df_hog <- hog_long %>%
        dplyr::filter(
          state_name == st,
          Year_chr   == as.character(yr)
        )
      
      validate(
        need(nrow(df_hog) > 0,
             paste("No HOG data for", st, "in year", yr))
      )
      
      df_hog <- df_hog %>%
        dplyr::mutate(
          hog_group = factor(
            hog_group,
            levels = hog_group[order(value, decreasing = TRUE)]
          )
        )
      
      ggplot(df_hog, aes(x = hog_group, y = value)) +
        geom_col(aes(fill = hog_group)) +
        geom_text(
          aes(label = format(value, big.mark = ",")),
          hjust = -0.1,
          size  = 3
        ) +
        coord_flip() +
        expand_limits(y = max(df_hog$value, na.rm = TRUE) * 1.1) +
        labs(
          title = paste0("Homeless Operational Groups (HOG) in ", st, " (", yr, ")"),
          x     = "Homeless operational group",
          y     = "Number of clients"
        ) +
        theme_minimal(base_size = 13) +
        theme(axis.text.y = element_text(hjust = 1),
              legend.position = "none")
      
      # 3) Age Group
    } else if (i == 3) {
      df_age <- agegroup_long %>%
        dplyr::filter(
          state_name == st,
          Year_chr   == as.character(yr)
        )
      
      validate(
        need(nrow(df_age) > 0,
             paste("No Age Group data for", st, "in year", yr))
      )
      
      df_age <- df_age %>%
        dplyr::mutate(
          age_group = factor(
            age_group,
            levels = age_group[order(value, decreasing = TRUE)]
          )
        )
      
      ggplot(df_age, aes(x = age_group, y = value)) +
        geom_col(aes(fill = age_group)) +
        geom_text(
          aes(label = format(value, big.mark = ",")),
          hjust = -0.1,
          size  = 3
        ) +
        expand_limits(y = max(df_age$value, na.rm = TRUE) * 1.1) +
        labs(
          title = paste0("Age group distribution in ", st, " (", yr, ")"),
          x     = "Age groups (years)",
          y     = "Number of clients"
        ) +
        theme_minimal(base_size = 13) +
        theme(axis.text.y = element_text(hjust = 1),
              legend.position = "none")
      
      # 4) Indigenous Status
    } else if (i == 4) {
      df_indig <- indistatus_long %>%
        dplyr::filter(
          state_name == st,
          Year_chr   == as.character(yr)
        )
      
      validate(
        need(nrow(df_indig) > 0,
             paste("No Indigenous status data for", st, "in year", yr))
      )
      
      df_indig <- df_indig %>%
        dplyr::mutate(
          indig_status = factor(
            indig_status,
            levels = indig_status[order(value, decreasing = TRUE)]
          )
        )
      
      ggplot(df_indig, aes(x = indig_status, y = value)) +
        geom_col(aes(fill = indig_status)) +
        geom_text(
          aes(label = format(value, big.mark = ",")),
          hjust = -0.1,
          size  = 3
        ) +
        expand_limits(y = max(df_indig$value, na.rm = TRUE) * 1.1) +
        labs(
          title = paste0("Indigenous status distribution in ", st, " (", yr, ")"),
          x     = "Indigenous status",
          y     = "Number of clients"
        ) +
        theme_minimal(base_size = 13) +
        theme(axis.text.y = element_text(hjust = 1),
              legend.position = "none")
      
      # 5) Sex
    } else if (i == 5) {
      df_sex <- sex_long %>%
        dplyr::filter(
          state_name == st,
          Year_chr   == as.character(yr)
        )
      
      validate(
        need(nrow(df_sex) > 0,
             paste("No Sex data for", st, "in year", yr))
      )
      
      df_sex <- df_sex %>%
        dplyr::mutate(
          sex_cat = factor(
            sex_cat,
            levels = sex_cat[order(value, decreasing = TRUE)]
          )
        )
      
      ggplot(df_sex, aes(x = sex_cat, y = value)) +
        geom_col(aes(fill = sex_cat)) +
        geom_text(
          aes(label = format(value, big.mark = ",")),
          hjust = -0.1,
          size  = 3
        ) +
        expand_limits(y = max(df_sex$value, na.rm = TRUE) * 1.1) +
        labs(
          title = paste0("Sex distribution in ", st, " (", yr, ")"),
          x     = "Sex",
          y     = "Number of clients"
        ) +
        theme_minimal(base_size = 13) +
        theme(
          axis.text.y     = element_text(hjust = 1),
          legend.position = "none"
        )
    }
  })
  #####################Slide 3#########################
  
  observeEvent(input$rc_show_calc, {
    updateTabsetPanel(session, "rc_view", selected = "calc")
  })
  
  observeEvent(input$rc_show_age, {
    updateTabsetPanel(session, "rc_view", selected = "age")
  })
  
  observeEvent(input$rc_show_cohort, {
    updateTabsetPanel(session, "rc_view", selected = "cohort")
  })
  
  rc_pred_prob <- eventReactive(input$rc_run, {
    req(input$rc_state, input$rc_sex, input$rc_age, input$rc_cohort)
    

    year_num <- 2024
    

    new_age <- data.frame(
      Year_num  = year_num,
      Sex       = factor(input$rc_sex,
                         levels = levels(age_model_data$Sex)),
      Age.group = factor(input$rc_age,
                         levels = levels(age_model_data$Age.group)),
      State     = factor(input$rc_state,
                         levels = levels(age_model_data$State))
    )
    

    new_cohort <- data.frame(
      Year_num     = year_num,
      Sex          = factor(input$rc_sex,
                            levels = levels(cohort_model_data$Sex)),
      Cohort.group = factor(input$rc_cohort,
                            levels = levels(cohort_model_data$Cohort.group)),
      State        = factor(input$rc_state,
                            levels = levels(cohort_model_data$State))
    )
    

    if (any(is.na(new_age)) || any(is.na(new_cohort))) return(NA_real_)
    
    logit_age    <- predict(age_glm,    newdata = new_age)
    logit_cohort <- predict(cohort_glm, newdata = new_cohort)
    
    as.numeric(inv_logit((logit_age + logit_cohort) / 2))  
  
  })
  

  output$rc_risk_pct <- renderText({
    p <- rc_pred_prob()
    if (is.na(p)) return("No prediction")
    paste0(round(p * 100, 1), "%")
  })
  

  output$rc_top_drivers <- renderTable({
    age_imp <- age_coef_df %>%
      dplyr::filter(grepl("Age.group|State|Sex", term)) %>%
      dplyr::arrange(dplyr::desc(abs_t)) %>%
      dplyr::mutate(Model = "Age model") %>%
      dplyr::select(Model, term, Estimate, t.value, p.value, abs_t)
    
    cohort_imp <- cohort_coef_df %>%
      dplyr::filter(grepl("Cohort.group|State|Sex", term)) %>%
      dplyr::arrange(dplyr::desc(abs_t)) %>%
      dplyr::mutate(Model = "Cohort model") %>%
      dplyr::select(Model, term, Estimate, t.value, p.value, abs_t)
    
    dplyr::bind_rows(age_imp, cohort_imp) %>%
      dplyr::arrange(dplyr::desc(abs_t)) %>%
      head(6) %>%
      dplyr::rename(
        Term      = term,
        `t value` = t.value,
        `p value` = p.value,
        `|t|`     = abs_t
      )
  })
  
  # model summaries
  output$rc_age_summary    <- renderPrint(summary(age_glm))
  output$rc_cohort_summary <- renderPrint(summary(cohort_glm))
  
  # conclusion text 
  output$rc_conclusion_text <- renderUI({
    p <- rc_pred_prob()
    if (is.na(p)) {
      HTML("<p>Select a client profile in the Risk Calculator and click <strong>Calculate risk</strong> to see the estimated probability of returning to homelessness.</p>")
    } else {
      base <- round(p * 100, 1)
      HTML(paste0(
        "<p>For a <strong>", input$rc_sex, "</strong> aged <strong>",
        input$rc_age, "</strong> in <strong>", input$rc_state,
        "</strong> and cohort <strong>", input$rc_cohort,
        "</strong>, the modelled 12-month risk of returning to homelessness ",
        "in a 2024 scenario is <strong>", base, "%</strong>.</p>",
        "<p>This helps identify client profiles at highest risk in the ",
        "<strong>revolving door</strong> and informs where policy and ",
        "practice interventions could be prioritised.</p>"
      ))
    }
  })
  
  ########################################
  # INTERVENTION SIMULATOR (Slide 3)
  ########################################
  
  sim_result <- eventReactive(input$sim_run, {
    req(input$sim_state)
    

    base <- intervention_df[intervention_df$State == input$sim_state, ]
    if (nrow(base) == 0) return(NULL)
    base <- base[1, ]
    

    base_dat <- data.frame(
      ShortTerm_Accomdation_Coverage =
        base$ShortTerm_Accomdation_Coverage,
      ShortTerm_Financial_Assistance_Coverage =
        base$ShortTerm_Financial_Assistance_Coverage,
      Share_Family_Domestic_Violence =
        base$Share_Family_Domestic_Violence
    )
    

    new_dat <- base_dat
    new_dat$ShortTerm_Accomdation_Coverage <-
      base$ShortTerm_Accomdation_Coverage * input$sim_accom_mult
    new_dat$ShortTerm_Financial_Assistance_Coverage <-
      base$ShortTerm_Financial_Assistance_Coverage * input$sim_fin_mult
    

    base_ratio <- as.numeric(predict(intervention_model, newdata = base_dat))
    new_ratio  <- as.numeric(predict(intervention_model, newdata = new_dat))
  
    base_ratio <- max(min(base_ratio, 30), 0)
    new_ratio  <- max(min(new_ratio, 30), 0)
    
    n_housed      <- base$N_housed
    base_returns  <- n_housed * base_ratio / 100
    new_returns   <- n_housed * new_ratio  / 100
    avoided       <- base_returns - new_returns
    
    list(
      state         = base$State,
      year          = base$Year,
      n_housed      = n_housed,
      base_ratio    = base_ratio,
      new_ratio     = new_ratio,
      base_returns  = base_returns,
      new_returns   = new_returns,
      avoided       = avoided,
      base_accom    = base$ShortTerm_Accomdation_Coverage,
      base_fin      = base$ShortTerm_Financial_Assistance_Coverage,
      new_accom     = new_dat$ShortTerm_Accomdation_Coverage,
      new_fin       = new_dat$ShortTerm_Financial_Assistance_Coverage
    )
  })
  
  # Narrative summary
  output$sim_summary <- renderUI({
    res <- sim_result()
    if (is.null(res)) {
      return(HTML("<p>Select a state, adjust coverage, and click <strong>Run intervention scenario</strong>.</p>"))
    }
    
    HTML(paste0(
      "<p>For <strong>", res$state, "</strong> in <strong>", res$year,
      "</strong>, the model estimates a baseline 12-month return rate of ",
      "<strong>", round(res$base_ratio, 1), "%</strong> among ",
      format(round(res$n_housed), big.mark = ","), " housed clients.</p>",
      "<p>With the selected intervention, coverage of short-term accommodation changes from ",
      scales::percent(res$base_accom, accuracy = 0.1), " to ",
      scales::percent(res$new_accom, accuracy = 0.1),
      ", and financial assistance coverage from ",
      scales::percent(res$base_fin, accuracy = 0.1), " to ",
      scales::percent(res$new_fin, accuracy = 0.1), ".</p>",
      "<p>The modelled return rate changes to <strong>",
      round(res$new_ratio, 1), "%</strong>, which corresponds to around ",
      "<strong>", format(round(res$avoided), big.mark = ","),
      " fewer clients</strong> returning to homelessness over 12 months.</p>"
    ))
  })
  

  output$sim_table <- renderTable({
    res <- sim_result()
    if (is.null(res)) return(NULL)
    
    data.frame(
      Metric = c("Return rate (%)",
                 "Clients returning (approx.)"),
      Baseline = c(
        round(res$base_ratio, 1),
        format(round(res$base_returns), big.mark = ",")
      ),
      `After intervention` = c(
        round(res$new_ratio, 1),
        format(round(res$new_returns), big.mark = ",")
      ),
      check.names = FALSE
    )
  })
  
}

###############################################
# RUN APP
###############################################

shinyApp(ui, server)
