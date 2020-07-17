#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(readxl)
library(shinythemes)
library(tidyverse)

# test_dta <- file.path("data", "test_dta.xlsx")
# dta <- read_excel(test_dta)
dta<- read.csv("https://raw.githubusercontent.com/AAPIData/raw_data/master/data_factsheets/test_dta.csv")
# topic_lookup <- file.path("data", "topic_lookup.xlsx")
# topic_lookup_dta <- read_excel(topic_lookup)
topic_lookup_dta <- read.csv("https://raw.githubusercontent.com/AAPIData/raw_data/master/data_factsheets/topic_lookup.csv")
#dta<- dta %>% arrange(group)
topics <- topic_lookup_dta %>% distinct(topic_group) %>% pull()
# state_population<- dta %>% select(top_states, starts_with("state_"), tot_state_pop) %>% colnames()

# Define UI for application that draws a histogram
shinyUI(
  fluidPage(
    tags$head(includeScript("google-analytics.js")),
    theme = shinytheme("slate"),
    tags$div(
      class = "header", checked = NA,
      tags$img(src = "aapidata.png", style = "display: block; width:250px; min-width: 200px max; margin-left: auto; margin-right: auto; margin-top:30px;")
    ),
    fluidRow(
      column(3, offset= 1,
             titlePanel("Community Facts"),
            # sidebarPanel(
               selectizeInput(
                 "Group",
                 "Select Groups (up to 3)",
                 choices = dta$group, multiple = TRUE, options = list(
                   maxItems = 3,
                   placeholder = "i.e. Vietnamese, Asian, US Average",
                   onInitialize = I('function() { this.setValue(""); }')
                 )
               ),
               # selectizeInput('x1', 'Another Input Option', choices = list(
               #   `South Asian` = c(`Indian` = 'Indian', `Pakistani` = 'Pakistani', `Bangladeshi`= 'Bangladeshi'),
               #   `East Asian` = c(`Chinese` = 'Chinese', `Japanese` = 'Japanese', `Korean`= 'Korean'),
               #   `General` = c(`Asian American`= 'Asian American',`USA Population` = "USA Popuation")), multiple = TRUE,options = list(maxItems = 3, placeholder = 'i.e. Chinese')),
               
               # selectInput('Group', 'Which Group?', dta$group, multiple=TRUE, selectize=FALSE),
               checkboxGroupInput(
                 "checkGroup", label = h3("Select Estimates"),
                 choices = topics,
                 selected = 1
               ),
               
               # conditionalPanel(condition="input.Group != null",
               #                  actionButton("submit", "Show Table")
               #                  ),
               conditionalPanel(
                 condition = "input.Group != null & input.checkGroup.length > 0",
                 downloadButton("report", "Download as PDF")
               ),
            actionButton(inputId='ab1', label="Back to AAPI Data", 
                                icon = icon("th"), 
                                onclick ="window.open('http://aapidata.com', '_blank')")
             #)   
      ),
    column(8,
           mainPanel(
             tags$style(type="text/css",
                        ".shiny-output-error { visibility: hidden; }",
                        ".shiny-output-error:before { visibility: hidden; }"
             ),
             
             # img(src='aapidata-logo-standard.png', align = "center"),
             h2(textOutput("selected_var")),
             fluidRow(
               column(
                 12, align = "left",
                 conditionalPanel(
                   "$.inArray('Total Population', input.checkGroup) > -1",
                   #         "$.inArray('Total Population', input.checkGroup) > -1 && input.submit == 1",
                   h4("Total Population"),
                   tableOutput("total_pop")
                 )
               )
             ),
             fluidRow(
               column(
                 12, align = "left",
                 conditionalPanel(
                   "$.inArray('Population Growth', input.checkGroup) > -1 ",
                   h4("Population Growth"),
                   tableOutput("pop_growth")
                 )
               )
             ),
             fluidRow(
               column(
                 12, align = "left",
                 conditionalPanel(
                   "$.inArray('Age Distribution', input.checkGroup) > -1 ",
                   h4("Age Distribution"),
                   tableOutput("age_distribution")
                 )
               )
             ),
             fluidRow(
               column(
                 12, align = "left",
                 conditionalPanel(
                   "$.inArray('Top States', input.checkGroup) > -1",
                   h4("Top States by Resident Population"),
                   tableOutput("top_states")
                 )
               )
             ),
             fluidRow(
               column(
                 12, align = "left",
                 conditionalPanel(
                   "$.inArray('Education', input.checkGroup) > -1 ",
                   h4("Education"),
                   tableOutput("education")
                 )
               )
             ),
             fluidRow(
               column(
                 12, align = "left",
                 conditionalPanel(
                   "$.inArray('Income and Poverty', input.checkGroup) > -1",
                   h4("Income & Poverty"),
                   tableOutput("income_pov")
                 )
               )
             ),
             fluidRow(
               column(
                 12, align = "left",
                 conditionalPanel(
                   "$.inArray('Political Participation', input.checkGroup) > -1",
                   h4("Political Participation"),
                   tableOutput("political")
                 )
               )
             ),
             fluidRow(
               column(
                 12, align = "left",
                 conditionalPanel(
                   "$.inArray('Language', input.checkGroup) > -1",
                   h4("Language"),
                   tableOutput("language")
                 )
               )
             ),
             fluidRow(
               column(
                 12, align = "left",
                 conditionalPanel(
                   "$.inArray('Nativity', input.checkGroup) > -1 ",
                   h4("Nativity"),
                   tableOutput("nativity")
                 )
               )
             ),
             fluidRow(
               column(
                 12, align = "left",
                 conditionalPanel(
                   "$.inArray('Health Insurance', input.checkGroup) > -1",
                   h4("Health Insurance"),
                   tableOutput("healthinsurance")
                 )
               )
             ),
             fluidRow(
               column(
                 12, align = "left",
                 conditionalPanel(
                   "$.inArray('Homeownership', input.checkGroup) > -1 ",
                   h4("Homeownership"),
                   tableOutput("homeownership")
                 )
               )
             ),
             tags$hr(),
             h4("Sources"),
             tableOutput("sources"),
             tags$footer("A Project of", tags$a(href = "http://www.aapidata.com", "AAPI Data"))
           )  
           )
    # Sidebar with a slider input for number of bins
#    sidebarLayout(
      

      # Show a plot of the generated distribution
      
#    )
  )
)
)