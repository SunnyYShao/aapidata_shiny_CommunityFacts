#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(data.table)
library(tidyverse)
library(readxl)

# test_dta <- file.path("data", "test_dta.xlsx") 
# dta<- read_excel(test_dta)
dta<- read.csv("https://raw.githubusercontent.com/AAPIData/raw_data/master/data_factsheets/test_dta.csv")
# source_dta <- file.path("data", "source_information.xlsx") 
# source<- read_excel(source_dta)
source <- read.csv("https://raw.githubusercontent.com/AAPIData/raw_data/master/data_factsheets/source_information.csv")
# topic_lookup <-file.path("data", "topic_lookup.xlsx") 
# topic_lookup_dta <- read_excel(topic_lookup)
topic_lookup_dta <- read.csv("https://raw.githubusercontent.com/AAPIData/raw_data/master/data_factsheets/topic_lookup.csv")

topic_lookup_dta<- topic_lookup_dta %>% rename(Estimate= "var_name")

shinyServer(function(input, output) {
  
# Getting what topics are selected so I can subset list
input_checkgroup<- reactive({
  topic_lookup_dta %>% filter(topic_group %in% input$checkGroup)%>% pull(Estimate)
})  

# Getting the var_names of any varaiables under the category "Top States"
input_topstates<- reactive({
  topic_lookup_dta %>% filter(topic_group  =="Top States")%>% pull(Estimate)
})
  

dta_show <- reactive({
  
    # dta<- dta %>% 
    #   filter(dta$group %in% input$Group) %>% # Take full data, then filter the rows (ethnic groups ) down to only those selected
    #   gather(Estimate, value, -group) # then gather the columns except for the ethnic group one such that we have long dta
  dta <- dta %>% 
    filter(dta$group %in% input$Group) %>%
    gather(Estimate, value, -group)
  
    percent_estimates<-dta %>%
      filter(str_detect(Estimate, 'pct_')) %>% pull(Estimate) %>% unique()

    states_estimates <- dta %>%
      filter(str_detect(Estimate, 'state_')) %>% pull(Estimate) %>% unique()
    
    selected_estimates_show<- input_checkgroup()
    top_states<-input_topstates()  
    
    
    dta<- dta %>% left_join(topic_lookup_dta) %>% 
      filter(Estimate %in% selected_estimates_show) %>% 
      filter(topic_group !="Top States") %>% 
      nest(-topic_group)
      


    # dta %>%  mutate(value = case_when(
    #   Estimate == "hhincome" ~ scales::dollar(round(as.numeric(value),0)),
    #   Estimate == "pop" ~ scales::comma(round(as.numeric(value),0)),
    #   Estimate %in% percent_estimates ~ paste(round(as.numeric(value)*100,1),"%",sep = ""))) %>%
    #   spread(group, value) %>% 
    #   filter(Estimate %in% selected_estimates_show) %>%
    #   filter(!Estimate %in% top_states) %>% 
    #   left_join(topic_lookup_dta) %>% 
    #   select(-Estimate, - topic_group) %>%
    #   select(label, everything())
    # dtafinal<- dta2 %>% mutate(output = map2(data,topic_group, generate_table))
    generate_table <- function(df,topic_group){
      if(topic_group == "Total Population"){
        df %>% mutate(value = case_when(
          Estimate == "pop" ~ scales::comma(round(as.numeric(value),0)),
          Estimate == "pop2" ~ scales::comma(round(as.numeric(value),0)))) %>%
          spread(group, value) %>% 
          select(-Estimate) %>%
          select(label, everything())
      } else if(topic_group == "Population Growth"){
        df %>% mutate(value = case_when(
          Estimate %in% percent_estimates & !is.na(value) ~ paste(round(as.numeric(value)*100,1),"%",sep = "")))  %>%
          spread(group, value) %>% 
          select(-Estimate) %>%
          select(label, everything())
      } else if(topic_group == "Age Distribution"){
        df %>% mutate(value = case_when(
          Estimate %in% percent_estimates & !is.na(value) ~ paste(round(as.numeric(value)*100,1),"%",sep = "")))  %>%
          spread(group, value) %>% 
          select(-Estimate) %>%
          select(label, everything())
      } else if(topic_group == "Education"){
        df %>% mutate(value = case_when(
          Estimate %in% percent_estimates & !is.na(value) ~ paste(round(as.numeric(value)*100,1),"%",sep = "")))  %>%
          spread(group, value) %>% 
          select(-Estimate) %>%
          select(label, everything())
      } else if (topic_group == "Income and Poverty"){
        df %>% mutate(value = case_when(
          Estimate %in% percent_estimates ~ paste(round(as.numeric(value)*100,1),"%",sep = "")))  %>%
          spread(group, value) %>% 
          select(-Estimate) %>%
          select(label, everything())
      }  else if(topic_group == "Political Participation"){
        df %>% mutate(value = case_when(
          Estimate %in% percent_estimates & !is.na(value) ~ paste(round(as.numeric(value)*100,1),"%",sep = ""),
          TRUE ~ value))  %>%
          spread(group, value) %>% 
          select(-Estimate) %>%
          select(label, everything())
      } else if(topic_group == "Language"){
        df %>% mutate(value = case_when(
          Estimate %in% percent_estimates & !is.na(value) ~ paste(round(as.numeric(value)*100,1),"%",sep = ""),
          Estimate == "common_lang" ~ value ))  %>%
          spread(group, value) %>% 
          select(-Estimate) %>%
          select(label, everything())
      } else if(topic_group == "Nativity"){
        df %>% mutate(value = case_when(
          Estimate %in% percent_estimates & !is.na(value) ~ paste(round(as.numeric(value)*100,1),"%",sep = "")))  %>%
          spread(group, value) %>% 
          select(-Estimate) %>%
          select(label, everything())
      }else if(topic_group == "Health Insurance"){
        df %>% mutate(value = case_when(
          Estimate %in% percent_estimates & !is.na(value) ~ paste(round(as.numeric(value)*100,1),"%",sep = "")))  %>%
          spread(group, value) %>% 
          select(-Estimate) %>%
          select(label, everything())
      }else if(topic_group == "Homeownership"){
        df %>% mutate(value = case_when(
          Estimate %in% percent_estimates & !is.na(value) ~ paste(round(as.numeric(value)*100,1),"%",sep = "")))  %>%
          spread(group, value) %>% 
          select(-Estimate) %>%
          select(label, everything())
      }
      
      # } else if(topic_group == "Top States"){
      #   df %>%
      #    # filter(dta$group %in% input$Group) %>%
      #     select(group, top_states, state_1, state_2, state_3, state_4,state_5,tot_state_pop) %>% 
      #     separate(top_states ,sep = "\r\n", into = c("State1_name", "State2_name", "State3_name", "State4_name", "State5_name")) %>%
      #     mutate(`1` = paste(State1_name," (",scales::comma(state_1),")",sep = ""),
      #            `2` = paste(State2_name," (",scales::comma(state_2),")",sep = ""),
      #            `3` = paste(State3_name," (",scales::comma(state_3),")",sep = ""),
      #            `4` = paste(State4_name," (",scales::comma(state_4),")",sep = ""),
      #            `5` = paste(State5_name," (",scales::comma(state_5),")",sep = ""))%>%
      #     select(group, `1`,`2`,`3`,`4`,`5`, tot_state_pop) %>%
      #     rename(`Total population in States` = tot_state_pop) %>%
      #     gather(`Top States`, value, -group) %>%
      #     spread(group, value)
      # }
    } 
    
    dta %>% mutate(output = map2(data,topic_group, generate_table))
    
    
  })
  
runstate<- reactive({
if("Top States" %in% input$checkGroup){
  runstate<-T
  }else{
  runstate<-F
  }
 runstate    
})
  
top_states_show <- reactive({
  dta %>%
    filter(dta$group %in% input$Group) %>%
    select(group, top_states, state_1, state_2, state_3, state_4,state_5,tot_state_pop) %>% 
    separate(top_states ,sep = ",", into = c("State1_name", "State2_name", "State3_name", "State4_name", "State5_name")) %>%
    mutate(`1` = paste(State1_name," (",scales::comma(state_1),")",sep = ""),
           `2` = paste(State2_name," (",scales::comma(state_2),")",sep = ""),
           `3` = paste(State3_name," (",scales::comma(state_3),")",sep = ""),
           `4` = paste(State4_name," (",scales::comma(state_4),")",sep = ""),
           `5` = paste(State5_name," (",scales::comma(state_5),")",sep = ""),
           tot_state_pop = scales::comma(round(as.numeric(tot_state_pop),0) )) %>%
    select(group, `1`,`2`,`3`,`4`,`5`, tot_state_pop) %>%
    rename(`Total population in States` = tot_state_pop) %>%
    gather(`Top States`, value, -group) %>%
    spread(group, value)
   
  
})  
  source_show<- reactive({
    source %>% filter(Estimate %in% input$checkGroup)
  })
  
#  observeEvent(input$submit, {
    
    output$total_pop <-renderTable({
      dta_showme <- dta_show()
      # dta_showme <- dta_showme$output[1]
      # dta_showme <- data.frame(dta_showme) %>% rename(` `= label)
      dta_showme %>% 
        filter(topic_group == "Total Population") %>% pull(output) %>% data.frame(check.names = F)%>% rename(` `= label)
    },striped = TRUE, bordered = TRUE, na=" ", 
    hover = TRUE, spacing = 'l',  
    width = '100%')
    
    output$pop_growth <-renderTable({
      dta_showme <- dta_show()
      # dta_showme <- dta_showme$output[2]
      # dta_showme <- data.frame(dta_showme) %>% rename(` `= label)
      dta_showme %>% 
        filter(topic_group == "Population Growth") %>% pull(output) %>% data.frame(check.names = F)%>% rename(` `= label)
    },striped = TRUE, bordered = TRUE,  
    hover = TRUE, spacing = 'l', na=" ",      
    width = '100%')
    
    output$age_distribution <-renderTable({
      dta_showme <- dta_show()
      # dta_showme <- dta_showme$output[2]
      # dta_showme <- data.frame(dta_showme) %>% rename(` `= label)
      dta_showme %>% 
        filter(topic_group == "Age Distribution") %>% pull(output) %>% data.frame(check.names = F)%>% rename(` `= label)
    },striped = TRUE, bordered = TRUE,  
    hover = TRUE, spacing = 'l', na=" ",      
    width = '100%')
    
    output$education <-renderTable({
      dta_showme <- dta_show()
      # dta_showme <- dta_showme$output[3]
      # dta_showme <- data.frame(dta_showme) %>% rename(` `= label)
      dta_showme %>% 
        filter(topic_group == "Education") %>% pull(output) %>% data.frame(check.names = F)%>% rename(` `= label)
    },striped = TRUE, bordered = TRUE,  na=" ",     
    hover = TRUE, spacing = 'l',  
    width = '100%')

    output$income_pov <-renderTable({
      dta_showme <- dta_show()
      # dta_showme <- dta_showme$output[4]
      # dta_showme <- data.frame(dta_showme) %>% rename(` `= label)
      dta_showme %>% 
        filter(topic_group == "Income and Poverty") %>% pull(output) %>% data.frame(check.names = F)%>% rename(` `= label)
    },striped = TRUE, bordered = TRUE, na=" ",      
    hover = TRUE, spacing = 'l',  
    width = '100%')
    
    output$political <-renderTable({
      dta_showme <- dta_show()
      # dta_showme <- dta_showme$output[4]
      # dta_showme <- data.frame(dta_showme) %>% rename(` `= label)
      dta_showme %>% 
        filter(topic_group == "Political Participation") %>% pull(output) %>% data.frame(check.names = F)%>% rename(` `= label)
    },striped = TRUE, bordered = TRUE,  
    hover = TRUE, spacing = 'l', na=" ",   
    width = '100%')
    
    output$language <-renderTable({
      dta_showme <- dta_show()
      # dta_showme <- dta_showme$output[4]
      # dta_showme <- data.frame(dta_showme) %>% rename(` `= label)
      dta_showme %>% 
        filter(topic_group == "Language") %>% pull(output) %>% data.frame(check.names = F)%>% rename(` `= label)
    },striped = TRUE, bordered = TRUE, na=" ",    
    hover = TRUE, spacing = 'l',  
    width = '100%')
    
    output$nativity <-renderTable({
      dta_showme <- dta_show()
      # dta_showme <- dta_showme$output[4]
      # dta_showme <- data.frame(dta_showme) %>% rename(` `= label)
      dta_showme %>% 
        filter(topic_group == "Nativity") %>% pull(output) %>% data.frame(check.names = F)%>% rename(` `= label)
    },striped = TRUE, bordered = TRUE,  
    hover = TRUE, spacing = 'l',  
    width = '100%')

    output$selected_var <- renderText({ 
      paste(input$Group,collapse=", ")
    })
    
    
    output$healthinsurance <-renderTable({
      dta_showme <- dta_show()
      # dta_showme <- dta_showme$output[4]
      # dta_showme <- data.frame(dta_showme) %>% rename(` `= label)
      dta_showme %>% 
        filter(topic_group == "Health Insurance") %>% pull(output) %>% data.frame(check.names = F)%>% rename(` `= label)
    },striped = TRUE, bordered = TRUE,  
    hover = TRUE, spacing = 'l', na=" ",    
    width = '100%')
    
    output$homeownership <-renderTable({
      dta_showme <- dta_show()
      # dta_showme <- dta_showme$output[4]
      # dta_showme <- data.frame(dta_showme) %>% rename(` `= label)
      dta_showme %>% 
        filter(topic_group == "Homeownership") %>% pull(output) %>% data.frame(check.names = F)%>% rename(` `= label)
    },striped = TRUE, bordered = TRUE,  na=" ",     
    hover = TRUE, spacing = 'l',  
    width = '100%')
                
    output$mytable2 <-renderTable({
      dta_showme <- dta_show()
      dta_showme <- dta_showme$output[2]
    },striped = TRUE, bordered = TRUE, na=" ",      
    hover = TRUE, spacing = 'l',  
    width = '100%')   
     
    output$sources <- renderTable({
      source_showme <- source_show()
      source_showme
    },striped = TRUE, bordered = TRUE, na=" ",  
    hover = TRUE, spacing = 'xs',  
    width = '100%')  
    
    output$top_states <- renderTable({
      topstates_showme <- top_states_show()
      #topstates_showme <- topstates_showme %>% rename(` `= label)
    },striped = TRUE, bordered = TRUE,
    hover = TRUE, spacing = 'l',  
    width = '100%')  
    
    
  #})
  
  
  
output$report <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "report.pdf",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      # tempReport <- file.path(tempdir(), "report.Rmd")
      # file.copy("report.Rmd", tempReport, overwrite = TRUE)
      src <- normalizePath('report.Rmd')
      src2 <- normalizePath('aapidata.png') #NEW 
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'report.Rmd')
      file.copy(src2, 'aapidata.png')
      
      
      my.dt <- dta_show()
      my.source<- source_show()
      my.states <-top_states_show()
      runstate<- runstate()
      params <- list(Group = input$Group,
                     Topics = input$checkGroup,
                     dataset= my.dt,
                     source = my.source,
                     state_pop= my.states,
                     runst = runstate)
      
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render('report.Rmd', output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv()))
      # rmarkdown::render(tempReport, output_file = file,
      #                                     params = params,
      #                                     envir = new.env(parent = globalenv())                  
      # )
    }
  )
  
  
}
)
