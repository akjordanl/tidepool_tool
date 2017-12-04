library(shiny)
if (!require('lubridate')) install.packages('lubridate'); library('lubridate')
if (!require('dplyr')) install.packages('dplyr'); library('dplyr')
if (!require('parsedate')) install.packages('parsedate'); library('parsedate')
if (!require('jsonlite')) install.packages('jsonlite'); library('jsonlite')
if (!require('httr')) install.packages('httr'); library('httr')
if (!require('DT')) install.packages('DT'); library('DT')
if (!require('rhandsontable')) install.packages('rhandsontable'); library('rhandsontable')

ui <- fluidPage(
  
  # Application title
  titlePanel("Tidepool Dr. Levy Daily Report"),
  p("v.Dev"),
  
  ## Inputs row at top
  br(),
  absolutePanel(width = 1500,
    fixedRow(
      column(width = 3,
             
             textInput("Username_input",
                "Tidepool username",
                value = "",
                placeholder = ""),
             
             passwordInput("Password_input",
                    "Tidepool password",
                    value = "",
                    placeholder = "")
             ),
      
      column(width = 2,
            
              dateInput("Date_input",
                       "Select Date",
                       value = with_tz(Sys.time(), tz = "America/New_York")-days(1)),
                       
             textInput("Timezone_input",
                "Select Timezone",
                value = "America/New_York",
                placeholder = "America/New_York")
             ),
      
      column(width = 1,
              actionButton("Run_report_action",
                   "Run report")),
  
    br(),
  
      fixedRow(rHandsontableOutput("mytable")),
      br(),
      fixedRow(
        column(width = 4, rHandsontableOutput("morning")),
        column(width = 4, rHandsontableOutput("afternoon")),
        column(width = 4, rHandsontableOutput("evening"))
      )
      
    )
  )
)
