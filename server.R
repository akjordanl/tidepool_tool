###**************LOAD NEEDED PACKAGES**********##

library(shiny)
if (!require('lubridate')) install.packages('lubridate'); library('lubridate')
if (!require('dplyr')) install.packages('dplyr'); library('dplyr')
if (!require('parsedate')) install.packages('parsedate'); library('parsedate')
if (!require('jsonlite')) install.packages('jsonlite'); library('jsonlite')
if (!require('httr')) install.packages('httr'); library('httr')
if (!require('DT')) install.packages('DT'); library('DT')
if (!require('rhandsontable')) install.packages('rhandsontable'); library('rhandsontable')


###**************START SERVER**********##


server <- function(input, output) {
  
  observeEvent(input$Run_report_action, { 
  
  
    ui_year <- year(input$Date_input)
    ui_month <- month(input$Date_input)
    ui_day <- day(input$Date_input)
    ui_target_tz <- input$Timezone_input
    
    options("digits.secs" = 3)
    
    anchor_day <- ISOdate(ui_year, ui_month, ui_day, tz = ui_target_tz)
    query_anchor_day <- ISOdatetime(ui_year, ui_month, ui_day, 00, 00 ,00.001, tz = ui_target_tz)
    
    data_start_query <- with_tz(query_anchor_day - days(1), "ZULU")
    data_start_query <- paste(
      paste(substr(data_start_query,1,10),substr(data_start_query,12,23), sep = "T"),
      "Z", sep = "")
    
    data_end_query <- with_tz(query_anchor_day + days(1), "ZULU")
    data_end_query <- paste(
      paste(substr(data_end_query,1,10),substr(data_end_query,12,23), sep = "T"),
      "Z", sep = "")
    
    messages_start_query <- with_tz(query_anchor_day - days(1), "ZULU")
    messages_start_query  <- paste(
      paste(substr(messages_start_query,1,10),substr(messages_start_query,12,23), sep = "T"),
      "Z", sep = "")
    
    messages_end_query <- with_tz(query_anchor_day + days(1), "ZULU")
    messages_end_query  <- paste(
      paste(substr(messages_end_query,1,10),substr(messages_end_query,12,23), sep = "T"),
      "Z", sep = "")
    
    ##************************************** GET DATA FROM TIDEPOOL API *****************************##
    ##install.packages("httr")
    ##library(httr)
    
    ## Set username and password
    my_username <- paste(input$Username_input, input$Password_input, sep = ":")
    
    my_password = input$Password_input
    
    ## Get tokens
    url <- "https://api.tidepool.org/auth/login"
    
    tp_response<-httr::POST(url, authenticate(my_username, my_password, type = "basic"), verbose())
    
    tp_session_token <- tp_response[["headers"]][["x-tidepool-session-token"]]
    
    tp_userid <- content(tp_response)[["userid"]]
    
    
    ## Get tidepool data
    url_with_userid <- paste("https://api.tidepool.org/data/",tp_userid, sep = "")
    
    tp_api_data <- httr::GET(url_with_userid,
                             add_headers("x-tidepool-session-token" = tp_session_token, "Content-Type" = "application/json"),
                             query = list("startDate" = data_start_query, "endDate" = data_end_query),
                             progress()
    )
    
    ## Get tidepool messages
    url_messages_with_userid <- paste("https://api.tidepool.org/message/notes/",tp_userid, sep = "")
    
    tp_api_messages <- httr::GET(url_messages_with_userid,
                                 add_headers("x-tidepool-session-token" = tp_session_token, "Content-Type" = "application/json"),
                                 query = list("starttime" = messages_start_query, "endtime" = messages_end_query), 
                                 progress()
    )
    
    
    ## Decode, flatten, and load data
    ##install.packages(jsonlite)
    ##library(jsonlite)
    
    ## Data
    tp_data_flat <- flatten(fromJSON(content(tp_api_data, "text")))
    ## Messages
    tp_messages_content <- fromJSON(content(tp_api_messages, "text"))
    tp_messages_flat <- data.frame(tp_messages_content)
    
    
    ##************************************** MANIPULATE DATA ***********************************##
    
    ##install packages
    ##install.packages("lubridate")
    ##install.packages("dplyr")
    ##install.packages("parsedate")
    
    ##load packages
    ##library(lubridate)
    ##library(dplyr)
    ##library(parsedate)
    
    ## Put dates into target timezone as new variable
    ## Pull in user input date of interest
    udvar_targettz <- ui_target_tz
    ## Code target timezone for Tidepool data file
    tp_data_flat$time_targtz <- with_tz(parse_date_time(tp_data_flat$time, "YmdHMS"), udvar_targettz)
    ## Code target timezone for Tidepool message file
    tp_messages_flat$timestamp_targtz <- with_tz(parse_date_time(ymd_hms(tp_messages_flat$messages.timestamp), "YmdHMS"), udvar_targettz)
    
    
    ## Select desired subset of data
    ## Tidepool data file
    tp_data_flat_doi <- dplyr::filter(tp_data_flat, day(tp_data_flat$time_targtz) == day(anchor_day))
    
    
    ## Tidepool messages file
    tp_messages_flat$messages.user <- unlist(tp_messages_flat$messages.user)
    tp_messages_flat_doi <- dplyr::filter(tp_messages_flat, day(tp_messages_flat$timestamp_targtz) == day(anchor_day))
    
    
    ## Create master_hour grouping variable
    ## Tidepool data file
    tp_data_flat_doi$master_hour <- tp_data_flat_doi %>%
      dplyr::transmute(master_hour = hour(tp_data_flat_doi$time_targtz))
    ## Tidepool message file
    tp_messages_flat_doi$master_hour <- tp_messages_flat_doi %>%
      dplyr::transmute(master_hour = hour(tp_messages_flat_doi$timestamp_targtz))
    
    ## Modify variables up front as needed for next set of operations
    ## Tidepool data file
    tp_data_flat_doi$master_hour <- unlist(tp_data_flat_doi$master_hour, recursive = TRUE, use.names = FALSE)
    tp_data_flat_doi$value <- as.numeric(tp_data_flat_doi$value)
    tp_data_flat_doicarbInput <- unlist(tp_data_flat_doi$carbInput, recursive = TRUE, use.names = FALSE)
    tp_data_flat_doi$carbInput <- as.numeric(tp_data_flat_doi$carbInput)
    tp_data_flat_doi$normal <- as.numeric(tp_data_flat_doi$normal)
    tp_data_flat_doi$payload.carb_bolus_units_delivered <- as.numeric(tp_data_flat_doi$payload.carb_bolus_units_delivered)
    tp_data_flat_doi$payload.corr_units_delivered <- as.numeric(tp_data_flat_doi$payload.corr_units_delivered)
    tp_data_flat_doi$duration <- as.numeric(tp_data_flat_doi$duration)
    tp_data_flat_doi$payload.event <- unlist(tp_data_flat_doi$payload.event, recursive = TRUE, use.names = FALSE)
    ## Tidepool message file
    tp_messages_flat_doi$master_hour <- unlist(tp_messages_flat_doi$master_hour, recursive = TRUE, use.names = FALSE)
    
    ## Calculate finger pricks
    hourly_smbg <- tp_data_flat_doi %>%
      dplyr::filter(type == 'smbg') %>%
      dplyr::select(master_hour, value) %>%
      dplyr::group_by(master_hour) %>%
      dplyr::summarise(Finger_Prick_Avg = round(18.01559 * mean(value),0),
                       Finger_Prick_Min = round(18.01559 * min(value),0),
                       Finger_Prick_Max = round(18.01559 * max(value),0))
    
    ## Calculate constant glucose monitor readings - mean, min, max - and plot by hour grouping
    hourly_cbg <- tp_data_flat_doi %>%
      dplyr::filter(type == 'cbg') %>%
      dplyr::select(master_hour, value) %>%
      dplyr::group_by(master_hour) %>%
      dplyr::summarise(CGM_Avg = round(18.01559 * mean(value),0),
                       CGM_Min = round(18.01559 * min(value),0),
                       CGM_Max = round(18.01559 * max(value),0))
    
    ## Calculate carb inputs
    hourly_carbs <- tp_data_flat_doi %>%
      dplyr::filter(is.na(carbInput) == 'FALSE') %>%
      dplyr::select(master_hour, carbInput) %>%
      dplyr::group_by(master_hour) %>%
      dplyr::summarise(Meal_Carbs = round(sum(carbInput),2))
    
    ##Add row for correction carbs
    
    hourly_correction_carbs <- data.frame(master_hour = 1, Correction_Carbs = "")
    
    
    ## Calculate total bolus
    hourly_bolus_total <- tp_data_flat_doi %>%
      dplyr::filter(type == 'bolus') %>%
      dplyr::select(master_hour, normal) %>%
      dplyr::group_by(master_hour) %>%
      dplyr::summarise(Total_Bolus = round(sum(normal),2))
    
    ## Calculate carb bolus
    hourly_bolus_carbs <- tp_data_flat_doi %>%
      dplyr::filter(type == 'wizard') %>%
      dplyr::select(master_hour, payload.carb_bolus_units_delivered) %>%
      dplyr::group_by(master_hour) %>%
      dplyr::summarise(Carb_Bolus = round(sum(payload.carb_bolus_units_delivered),2))
    
    ## Calculate correction bolus
    hourly_bolus_correction <- tp_data_flat_doi %>%
      dplyr::filter(type == 'wizard') %>%
      dplyr::select(master_hour, payload.corr_units_delivered) %>%
      dplyr::group_by(master_hour) %>%
      dplyr::summarise(Correction_Bolus = round(sum(payload.corr_units_delivered),2))
    
    ## Calculate basal adjustment - first create ifelse escape if data doesn't exist in that particular day
    ifelse("percent" %in% colnames(tp_data_flat_doi) == FALSE,
           
           hourly_basal_adjustment <- data.frame(master_hour = 1, Basal_Adjustment_Percent = "", Basal_Adjustment_Duration_Hours = ""),
           
           hourly_basal_adjustment <- tp_data_flat_doi %>%
             dplyr::filter(type == 'basal' & deliveryType == 'temp') %>%
             dplyr::select(master_hour, percent, duration) %>%
             dplyr::transmute(master_hour = master_hour, Basal_Adjustment_Percent = 100 * round(percent,2), Basal_Adjustment_Duration_Minutes = round(as.numeric(duration) / (1000*60),0))
    )
    
    ## Flag pod deactivation
    ifelse("payload.event" %in% colnames(tp_data_flat_doi) == FALSE,
           
           hourly_pod_deactivation <- data.frame(master_hour = 1, Pump_Deactivation = ""),
           
           hourly_pod_deactivation <- tp_data_flat_doi %>%
             dplyr::filter(type == 'deviceEvent' & payload.event == 'pod_deactivation') %>%
             dplyr::transmute(master_hour = master_hour, Pump_Deactivation = ifelse(is.na(payload.event) == TRUE, "", "***"))
    )
    
   
    ##Add row for activity
    
    hourly_activity <- data.frame(master_hour = 1, Activity = "")
    
     ## Gather messages, blipnotes
    ifelse( "messages.messagetext" %in% colnames(tp_messages_flat_doi) == FALSE,
            
            hourly_notes <- data.frame(master_hour = 1, timestamp_targtz = ISOdatetime(ui_year, ui_month, ui_day, 00, 00 ,00.000, tz = ui_target_tz), messages.messagetext = ""),
            
            hourly_notes <- tp_messages_flat_doi %>%
              dplyr::filter(is.na(messages.messagetext) == 'FALSE') %>%
              dplyr::select(master_hour, timestamp_targtz, messages.messagetext) %>%
              dplyr::arrange(master_hour)
    )
    
    ## Group morning, afternoon, Night notes in different tables
    hourly_notes_morning <-  hourly_notes %>%
      dplyr::filter(master_hour >=0 & master_hour <12) %>%
      dplyr::arrange(timestamp_targtz)%>%
      dplyr::transmute(note_time = paste(hour(timestamp_targtz),sprintf("%02d",minute(timestamp_targtz)), sep=":"), morning_notes = messages.messagetext)
    
    hourly_notes_afternoon <-  hourly_notes %>%
      dplyr::filter(master_hour >=12 & master_hour <19) %>%
      dplyr::arrange(timestamp_targtz)%>%
      dplyr::transmute(note_time = paste(hour(timestamp_targtz),sprintf("%02d",minute(timestamp_targtz)), sep=":"), afternoon_notes = messages.messagetext)
    
    hourly_notes_evening <-  hourly_notes %>%
      dplyr::filter(master_hour >=19) %>%
      dplyr::arrange(timestamp_targtz)%>%
      dplyr::transmute(note_time = paste(hour(timestamp_targtz),sprintf("%02d",minute(timestamp_targtz)), sep=":"), evening_notes = messages.messagetext)
    
    ## Join up datasets
    ## Part 1: all the numeric data
    ## Create hours backbone to serve as leftmost join column
    master_hour <- c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23)
    master_hour <- data.frame(master_hour)
    ## Join up all the other datasets created above
    joined_data_1 <- dplyr::left_join(master_hour, hourly_smbg, by="master_hour")
    joined_data_2 <- dplyr::left_join(joined_data_1,hourly_cbg, by="master_hour")
    joined_data_3 <- dplyr::left_join(joined_data_2,hourly_carbs, by="master_hour")
    joined_data_4 <- dplyr::left_join(joined_data_3,hourly_correction_carbs, by="master_hour")
    joined_data_5 <- dplyr::left_join(joined_data_4,hourly_bolus_total, by="master_hour")
    joined_data_6 <- dplyr::left_join(joined_data_5,hourly_bolus_carbs, by="master_hour")
    joined_data_7 <- dplyr::left_join(joined_data_6,hourly_bolus_correction, by="master_hour")
    joined_data_8 <- dplyr::left_join(joined_data_7,hourly_basal_adjustment, by="master_hour")
    joined_data_9 <- dplyr::left_join(joined_data_8,hourly_pod_deactivation, by="master_hour")
    joined_data_10 <- dplyr::left_join(joined_data_9,hourly_activity, by="master_hour")
    joined_data_F <- t(joined_data_10)
    
    ##colnames(joined_data_F) <- joined_data_F[1,]
    colnames(joined_data_F) <- c("12am","1am","2am","3am","4am","5am","6am","7am","8am","9am","10am","11am","12pm","1pm","2pm","3pm","4pm","5pm","6pm","7pm","8pm","9pm","10pm","11pm")
    joined_data_F <- joined_data_F[-1,]
    
    
    ##************************************** END DATA MANIPULATION *******************************************##
    
    output$mytable <- renderRHandsontable(rhandsontable(joined_data_F, width = 1500, rowHeaderWidth = 250))
                                          
    
    output$morning <- renderRHandsontable(rhandsontable(hourly_notes_morning, overflow = 'visible') %>% 
                                           hot_col(col = "note_time", colwidths = 5) %>%
                                           hot_col(col = "morning_notes", colWidths = 350)) 
    
    output$afternoon <- renderRHandsontable(rhandsontable(hourly_notes_afternoon, overflow = 'visible') %>%
                                           hot_col(col = "note_time", colwidths = 5) %>%
                                           hot_col(col = "afternoon_notes", colWidths = 350)) 
    
    output$evening <- renderRHandsontable(rhandsontable(hourly_notes_evening, overflow = 'visible') %>%
                                           hot_col(col = "note_time", colwidths = 5) %>%
                                           hot_col(col = "evening_notes", colWidths = 350)) 
    
  })
}