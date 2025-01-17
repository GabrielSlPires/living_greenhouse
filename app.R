#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


packages <- c("shiny",
              "shinydashboard",
              "ggplot2",
              "dplyr",
              "lubridate",
              "googledrive",
              "plantecophys")

install.packages(setdiff(packages, rownames(installed.packages())))

rm(packages)


require(shiny)
require(shinydashboard)
require(ggplot2)
require(dplyr)
require(lubridate)

max_min_norm <- function(x, by = 100) ((x - min(x))/(max(x) - min(x)))*by

#open data that it is already fixed

source("scripts/01 - data_wrangling.R", local = TRUE) #download and update data

#tryCatch({
#    raw <- data.table::fread("data/pneumatron_fixed.csv")
#  }, error = function(e) {
#    source("scripts/01 - data_wrangling.R", local = environment()) #download and update data
#    raw <- data.table::fread("data/pneumatron_fixed.csv")
#  })

colunas <- c('id',
             'step_min',
             'pad',
             'pf',
             'pi',
             'ad_mol',
             'ad_ul',
             'temp1_f',
             'temp1_i',
             'c',
             'temp2_f',
             'temp2_i',
             'humid1_f',
             'humid1_i',
             'humid2_f',
             'humid2_i',
             'atm_pres2_f',
             'atm_pres2_i',
             'datetime')

days_table <- raw %>% 
  dplyr::mutate(date = lubridate::date(lubridate::ymd_hms(datetime))) %>% 
  dplyr::select(date, id) %>% 
  unique()

pi_s <- 1.5
pf_s <- 150
reservoir <- 2.6

p_atm <- 101.3
Vr <- reservoir*10^-6
R <- 8.3144621
temp <- 293.15

data_wissen <- raw %>% 
  dplyr::filter(log_line %in% c(pi_s*2, pf_s*2)) %>% 
  dplyr::group_by(id, step_min15) %>% 
  dplyr::summarise(
    pf = pressure[which(log_line == pf_s*2)],
    pi = pressure[which(log_line == pi_s*2)],
    ad_mol = ((pf - pi)*100*Vr)/(R*temp),
    ad_ul = (ad_mol*R*temp/(p_atm*100))*1000*1000*1000,
    temp1_f = temp1[which(log_line == pf_s*2)],
    temp1_i = temp1[which(log_line == pi_s*2)],
    #I just replicate your code. If temp1_i don't make sense to be used we could use an average of temp1_i and temp1_f
    c = (pf - pi)/(R*temp1_i),
    #Do we need thins info? If so, do we need the initial and final? Or could we use an average?
    temp2_f = temp2[which(log_line == pf_s*2)],
    temp2_i = temp2[which(log_line == pi_s*2)],
    humid1_f = humid1[which(log_line == pf_s*2)],
    humid1_i = humid1[which(log_line == pi_s*2)],
    humid2_f = humid2[which(log_line == pf_s*2)],
    humid2_i = humid2[which(log_line == pi_s*2)],
    atm_pres2_f = atm_pres2[which(log_line == pf_s*2)],
    atm_pres2_i = atm_pres2[which(log_line == pi_s*2)],
    datetime = datetime,
    .groups = "drop") %>%
  dplyr::filter(ad_ul > 0) %>% 
  dplyr::filter(ad_ul < mean(ad_ul)*1.8) %>% 
  dplyr::mutate(vpd = plantecophys::RHtoVPD(humid2_f, temp1_f)) %>% 
  dplyr::group_by(id) %>% 
  dplyr::mutate(pad = max_min_norm(ad_ul)) %>% 
  dplyr::rename(step_min = step_min15)

#escolher os dois eixos que apareceram
#unir lista nomeada de legenda interativamente


sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Lange Nacht der Wissenschaft",
             tabName = "wissen",
             badgeLabel = "new",
             icon = icon("seedling") #filter-list
    ),
    menuItem("Data Filter",
             tabName = "filter",
             icon = icon("filter") #filter-list
    ),
    menuItem("Chart",
             tabName = "chart",
             icon = icon("chart-bar")
    ),
    menuItem("Parameters",
             icon = icon("th"),
             tabName = "parameters",
             badgeColor = "green")
  ),
  menuItem("Source code",
           icon = icon("file-code"),
           href = "https://github.com/GabrielSlPires/living_greenhouse")
)

wissen_sensors <- c("Temperature",
                    "Humid",
                    "VPD")

wissen <- tabItem(tabName = "wissen",
                  h3("Lange Nacht der Wissenschaft"),
                  fluidRow(
                    box(
                      width = 12,
                      column(width = 2,
                             actionButton("update_database", "Update")
                      ),
                      column(width = 2,
                             sliderInput("hour_average",
                                         "Hour Average:",
                                         min = 1,
                                         max = 4,
                                         step = 0.5,
                                         value = 1),
                      ),
                      column(width = 4,
                             checkboxGroupInput("wissen_checkbox",
                                                "Select Sensor(s):",
                                                choices = wissen_sensors,
                                                selected = wissen_sensors,
                                                inline = TRUE),
                      ),
                      column(width = 4,
                             selectInput("wissen_second_axis",
                                         "Select Second Axis:",
                                         choices = wissen_sensors,
                                         selected = "Temperature"),
                      ),
                    ),
                    column(width = 6,
                           box(
                             title = "Sleepy Plant",
                             width = NULL,
                             solidHeader = TRUE,
                             status = "warning",
                             plotOutput("wissen_sleepy_plant_plot")
                           )
                    ),
                    column(width = 6,
                           box(
                             title = "Control Plant",
                             width = NULL,
                             solidHeader = TRUE,
                             status = "success",
                             plotOutput("wissen_control_plant_plot")
                           )
                    ),
                  )
          ) #end item

body <- dashboardBody(
  tabItems(
    wissen,
    tabItem(tabName = "filter",
            h3("Filter Your Data"),
            fluidRow(
                     box(
                       width = 12,
                       #p("Use this filter to corretly select your experiment data:"),
                       column(width = 6,
                             dateRangeInput('filter_dateRange',
                                            label = 'Date range:',
                                            start = min(raw$datetime),
                                            end = max(raw$datetime)
                             ),
                       ),
                       column(width = 6,
                              uiOutput(outputId = "checkbox_pneumatron_id_filter"),
                       ),
                     ),
              column(width = 6,
                     box(
                       title = "Filtered Data",
                       width = NULL,
                       solidHeader = TRUE,
                       status = "primary",
                       plotOutput("filtered_data")
                     )
              ),
              column(width = 6,
                     box(
                       title = "Entire Data",
                       width = NULL,
                       solidHeader = TRUE,
                       status = "warning",
                       plotOutput("entire_data")
                     )
              ),
            ),
    ), #end item
    tabItem(tabName = "chart",
            fluidRow(
              column(width = 12,
                     box(title = "Data Parameters",
                         solidHeader = TRUE,
                         collapsible = TRUE,
                         status = "primary",
                         width = 12,
                         column(width = 6,
                                ), #end column
                         ), #end box
                     ) #end column
              ), #end row
            fluidRow(
              column(width = 6,
                     #create a device option
                     box(title = "Plot Parameters",
                         solidHeader = TRUE,
                         collapsible = TRUE,
                         status = "primary",
                         width = 12,
                         column(width = 6,
                                selectInput("scat_x",
                                            label = "select x-axis:",
                                            choices = colunas,
                                            selected = "step_min"),
                                selectInput("scat_y",
                                            label = "select y-axis:",
                                            choices = colunas,
                                            selected = "pad"),
                         ), #end column
                         column(width = 6,
                                dateRangeInput('dateRange',
                                               label = 'Date range input: yyyy-mm-dd',
                                               start = min(raw$datetime),
                                               end = max(raw$datetime)
                                ),
                                selectInput("color_by",
                                            label = "Color by:",
                                            choices = colunas,
                                            selected = "id"),
                                selectInput("sep_by",
                                            label = "Separate by:",
                                            choices = colunas,
                                            selected = "id"),
                                selectInput("sep_by2",
                                            label = "Separate by git explain:",
                                            choices = colunas,
                                            selected = "id")
                         ), #end column
                         column(width = 12,
                                infoBox("PAD",
                                        "Percentage of air discharged",
                                        icon = icon("tree-deciduous")
                                )
                         ) #end column
                     ) #end box
              ), #end column
              column(width = 6,
                     box(width = 12,
                         title = "Scatter Plot",
                         solidHeader = TRUE,
                         textOutput("colunas"),
                         plotOutput("scatter", height = "500px")
                     ) #end box
              ), #end column
            ), #end row
    ), #end item
    tabItem(tabName = "parameters",
            h2("Change Stutfs"),
            fluidRow(
              box(title = "Pneumatron",
                  width = 6,
                  collapsible = TRUE,
                  p("Define the time where you would have your initial (pi_s) and final (pf_s) pressures."),
                  p("Time desired is usually 1.5 (initial pressure) and 150 (final pressure) seconds."),
                  numericInput("initial_pressure",
                               label = "Initial Pressure",
                               value = 1.5),
                  numericInput("final_pressure",
                               label = "Final Pressure",
                               value = 150),
                  p("Define your tubing volume (in mL)"),
                  numericInput("tubing",
                               label = "Tubing Volume",
                               value = 2.6)
              ), #end box
              box(title = "Ambient Parameters",
                  witdh = 6,
                  p("Define atmospheric pressure (in kPa)"),
                  numericInput("atm_pressure",
                               label = "Atmospheric Pressure",
                               value = 101.3),
                  numericInput("temp",
                               label = "Temperature (K)",
                               value = 293.15)
              ) #end box
            ) # end row
    ) #end item
  ) #end items
)


# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(title = "Talking Garden"),
  sidebar,
  body
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  observeEvent(input$update_database, {
    #source("scripts/01 - data_wrangling.R", local = environment()) #download and update data
    #raw <- data.table::fread("data/pneumatron_fixed.csv")
    source("scripts/01 - data_wrangling.R", local = TRUE) #download and update data
  })
  
  df <- reactive( {
    pi_s <- input$initial_pressure
    pf_s <- input$final_pressure
    reservoir <- input$tubing
    
    p_atm <- input$atm_pressure
    Vr <- reservoir*10^-6
    R <- 8.3144621
    temp <- input$temp

    
    #Generate PAD
    #This code creates the same result as your loop. I think in that way it is easier to spot errors or implament changes.
    #If you have any doubts about it, let me know
    data <- raw %>% 
      dplyr::filter(
             as.Date(datetime) >= input$dateRange[1],
             as.Date(datetime) <= input$dateRange[2],
             log_line %in% c(pi_s*2, pf_s*2)) %>% 
      dplyr::group_by(id, step_min15) %>% 
      dplyr::summarise(
        pf = pressure[which(log_line == pf_s*2)],
        pi = pressure[which(log_line == pi_s*2)],
        ad_mol = ((pf - pi)*100*Vr)/(R*temp),
        ad_ul = (ad_mol*R*temp/(p_atm*100))*1000*1000*1000,
        temp1_f = temp1[which(log_line == pf_s*2)],
        temp1_i = temp1[which(log_line == pi_s*2)],
        #I just replicate your code. If temp1_i don't make sense to be used we could use an average of temp1_i and temp1_f
        c = (pf - pi)/(R*temp1_i),
        #Do we need thins info? If so, do we need the initial and final? Or could we use an average?
        temp2_f = temp2[which(log_line == pf_s*2)],
        temp2_i = temp2[which(log_line == pi_s*2)],
        humid1_f = humid1[which(log_line == pf_s*2)],
        humid1_i = humid1[which(log_line == pi_s*2)],
        humid2_f = humid2[which(log_line == pf_s*2)],
        humid2_i = humid2[which(log_line == pi_s*2)],
        atm_pres2_f = atm_pres2[which(log_line == pf_s*2)],
        atm_pres2_i = atm_pres2[which(log_line == pi_s*2)],
        datetime = datetime,
        .groups = "drop") %>%
      dplyr::filter(ad_ul > 0) %>% 
      dplyr::filter(ad_ul < mean(ad_ul)*1.8) %>% 
      dplyr::mutate(vpd = plantecophys::RHtoVPD(humid2_f, temp1_f)) %>% 
      dplyr::group_by(id) %>% 
      dplyr::mutate(pad = max_min_norm(ad_ul)) %>% 
      dplyr::rename(step_min = step_min15)
  })
  
  
  output$checkbox_pneumatron_id_filter <- renderUI({
    choice <-  sort(unique(raw$id)) #unique(data[data$cyl %in% input$select1, "gear"])
    checkboxGroupInput("pneumatron_id_filter",
                       "Select Pneumatron Device(s):",
                       choices = choice,
                       selected = choice,
                       inline = TRUE)
  })
  outputOptions(output, "checkbox_pneumatron_id_filter", suspendWhenHidden = FALSE)
  
  
  output$scatter <- renderPlot({
    selected_xvar = input$scat_x
    selected_yvar = input$scat_y
    
    req(selected_xvar, selected_yvar)
    ggplot(df(),
           aes_string(x = selected_xvar,
                      y = selected_yvar)) +
      geom_point() +
      facet_wrap(~id)
  })
  
  data_wissen_reactive <- reactive({
    hour_avg <- paste(input$hour_average, "hour")

    data_wissen %>% 
      dplyr::group_by(hour = cut(datetime, breaks = hour_avg), id) %>% 
      dplyr::summarise(across(where(is.numeric), mean), .groups = "drop") %>% 
      dplyr::group_by(id) %>% 
      dplyr::mutate(temp = max_min_norm(temp1_f),
             humid = max_min_norm(humid2_f),
             vpd = max_min_norm(vpd),
             
             temp_max = max(temp1_f),
             humid_max = max(humid2_f),
             vpd_max = max(vpd),
             
             temp_min = min(temp1_f),
             humid_min = min(humid2_f),
             vpd_min = min(vpd),
             
             pad = max_min_norm(ad_ul),
             hour = as.POSIXct(hour),
             hour_shade = as.numeric(format(hour, "%H")) >= 21 | as.numeric(format(hour, "%H")) <= 6)
  })
    
  output$wissen_control_plant_plot <- renderPlot({

    env_sensors <- list()
    env_colors <- c("Gas Discharge" = "black")
    
    #8 is control
    #Checkbox to select lines
    if ("Temperature" %in% input$wissen_checkbox) {
      env_sensors <- list(env_sensors,
                          geom_line(aes(y = temp, color = "Temperature"), size = 1.5))
      env_colors <- c(env_colors, "Temperature" = "red")
    }
    if ("Humid" %in% input$wissen_checkbox) {
      env_sensors <- list(env_sensors,
                          geom_line(aes(y = humid, color = "Humid"), size = 1.5))
      env_colors <- c(env_colors, "Humid" = "blue")
    }
    if ("VPD" %in% input$wissen_checkbox) {
      env_sensors <- list(env_sensors,
                          geom_line(aes(y = vpd, color = "VPD"), size = 1.5))
      env_colors <- c(env_colors, "VPD" = "orange")
    }
    
    #To select second axis
    if ("Temperature" == input$wissen_second_axis) {
      corretion_max <- data_wissen_reactive() %>% 
        dplyr::filter(id == 8) %>% 
        dplyr::select(temp_max) %>% 
        max()
      corretion_min <- data_wissen_reactive() %>% 
        dplyr::filter(id == 8) %>% 
        dplyr::select(temp_min) %>% 
        min()
    }
    if ("Humid" == input$wissen_second_axis) {
      corretion_max <- data_wissen_reactive() %>% 
        dplyr::filter(id == 8) %>% 
        dplyr::select(humid_max) %>% 
        max()
      corretion_min <- data_wissen_reactive() %>% 
        dplyr::filter(id == 8) %>% 
        dplyr::select(humid_min) %>% 
        min()
    }
    if ("VPD" == input$wissen_second_axis) {
      corretion_max <- data_wissen_reactive() %>% 
        dplyr::filter(id == 8) %>% 
        dplyr::select(vpd_max) %>% 
        max()
      corretion_min <- data_wissen_reactive() %>% 
        dplyr::filter(id == 8) %>% 
        dplyr::select(vpd_min) %>% 
        min()
    }
    second_axis <- sec_axis(~./100*(corretion_max-corretion_min) + corretion_min,
                            name = input$wissen_second_axis)
    
    data_wissen_reactive() %>% 
      dplyr::filter(id == 8) %>% 
      dplyr::mutate(vpd = humid) %>% #we don't have humid for this plant
      ggplot(aes(hour, group = id)) +
      #geom_rect(aes(xmin = hour, xmax = lead(hour), ymin = -Inf, ymax = Inf,
      #              fill = hour_shade)) +
      geom_line(aes(y = pad, color = "Gas Discharge"), size = 1.5) +
      env_sensors +
      scale_y_continuous("PAD (%)", sec.axis = second_axis) +
      scale_fill_manual(values = c("white", "grey90")) +
      scale_color_manual("", values = env_colors) +
      theme_classic() +
      guides(fill = "none") +
      theme(legend.position = "top") +
      xlab("")
  })

  output$wissen_sleepy_plant_plot <- renderPlot({
    
    env_sensors <- list()
    env_colors <- c("Gas Discharge" = "black")
    
    #8 is control
    #Checkbox to select lines
    if ("Temperature" %in% input$wissen_checkbox) {
      env_sensors <- list(env_sensors,
                          geom_line(aes(y = temp, color = "Temperature"), size = 1.5))
      env_colors <- c(env_colors, "Temperature" = "red")
    }
    if ("Humid" %in% input$wissen_checkbox) {
      env_sensors <- list(env_sensors,
                          geom_line(aes(y = humid, color = "Humid"), size = 1.5))
      env_colors <- c(env_colors, "Humid" = "blue")
    }
    if ("VPD" %in% input$wissen_checkbox) {
      env_sensors <- list(env_sensors,
                          geom_line(aes(y = vpd, color = "VPD"), size = 1.5))
      env_colors <- c(env_colors, "VPD" = "orange")
    }
    
    #To select second axis
    if ("Temperature" == input$wissen_second_axis) {
      corretion_max <- data_wissen_reactive() %>% 
        dplyr::filter(id == 7) %>% 
        dplyr::select(temp_max) %>% 
        max()
      corretion_min <- data_wissen_reactive() %>% 
        dplyr::filter(id == 7) %>% 
        dplyr::select(temp_min) %>% 
        min()
    }
    if ("Humid" == input$wissen_second_axis) {
      corretion_max <- data_wissen_reactive() %>% 
        dplyr::filter(id == 7) %>% 
        dplyr::select(humid_max) %>% 
        max()
      corretion_min <- data_wissen_reactive() %>% 
        dplyr::filter(id == 7) %>% 
        dplyr::select(humid_min) %>% 
        min()
    }
    if ("VPD" == input$wissen_second_axis) {
      corretion_max <- data_wissen_reactive() %>% 
        dplyr::filter(id == 7) %>% 
        dplyr::select(vpd_max) %>% 
        max()
      corretion_min <- data_wissen_reactive() %>% 
        dplyr::filter(id == 7) %>% 
        dplyr::select(vpd_min) %>% 
        min()
    }
    second_axis <- sec_axis(~./100*(corretion_max-corretion_min) + corretion_min,
                            name = input$wissen_second_axis)
    
    data_wissen_reactive() %>% 
      dplyr::filter(id == 7) %>% 
      #dplyr::mutate(hour_shade = ifelse(hour > "2022-07-15 12:00:0000", TRUE, hour_shade)) %>% 
      ggplot(aes(hour, group = id)) +
      #geom_rect(aes(xmin = hour, xmax = lead(hour), ymin = -Inf, ymax = Inf,
      #              fill = hour_shade)) +
      geom_line(aes(y = pad, color = "Gas Discharge"), size = 1.5) +
      env_sensors +
      scale_y_continuous("PAD (%)", sec.axis = second_axis) +
      scale_fill_manual(values = c("white", "grey90")) +
      scale_color_manual("", values = env_colors) +
      theme_classic() +
      guides(fill = "none") +
      theme(legend.position = "top") +
      xlab("")
  })

  output$entire_data <- renderPlot({
    initial <- input$filter_dateRange[1]
    final <- input$filter_dateRange[2]
    
    id_levels <- sort(unique(days_table$id))
    
    data_shader <- list(
      geom_rect(aes(xmin = initial,
                    xmax = final,
                    ymin = 0,
                    ymax = factor(max(id), levels = id_levels)),
                color = "black",
                fill = "grey50",
                alpha = 0.01))
    
    ggplot(days_table %>% 
             dplyr::mutate(filtered = if_else(id %in% input$pneumatron_id_filter,
                                       "Selected",
                                       "Removed")),
           aes(x = date,
               y = factor(id,
                          levels = id_levels),
               group = id,
               color = filtered)) +
      geom_line(size = 2) +
      data_shader +
      scale_color_manual(values = c("Selected" = "black",
                                    "Removed" = "red")) + 
      ylab("Pneumatron Device (ID)") +
      xlab("") +
      theme_classic() +
      theme(legend.position = "top",
            legend.title = element_blank())
  })
  
  output$filtered_data <- renderPlot({
    initial <- input$filter_dateRange[1]
    final <- input$filter_dateRange[2]
    id_levels <- sort(unique(days_table$id))
    
    ggplot(days_table %>% 
             dplyr::filter(between(date, initial, final),
                    id %in% input$pneumatron_id_filter),
           aes(x = date,
               y = factor(id, levels = id_levels),
               group = id)) +
      geom_line(size = 2) +
      ylab("Pneumatron Device (ID)") +
      xlab("") +
      theme_classic()
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
