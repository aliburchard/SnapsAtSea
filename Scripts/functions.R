library(dplyr)
library(magrittr)
library(stringr)
library(lubridate)
library(jsonlite)
library(tidyjson)


check_workflow <- function(data){
     require(lubridate)
     data %>% group_by(workflow_id, workflow_version) %>%
          summarise(., start_date = min(ymd_hms(created_at)), end_date = max(ymd_hms(created_at)), count = n()) %>%
          arrange(workflow_id) %>%
          print
}


#lump all the not-logged-in users together into one username
#if username begins with "not-logged-in", then assign all to just that 
trim_cols <- function(data) {
     out <- data %>% 
     select(., subject_ids, 
            workflow_id, 
            workflow_name, 
            workflow_version,
            classification_id, 
            user_name, 
            user_id, 
            user_ip, 
            created_at)
}



# Group non-logged-ins - adds a separate column that groups non-logged-in users

group_non_logged <- function(dataset) {
     data_out <- dataset %>% 
          mutate(., user = ifelse(str_detect(user_name, pattern = "not-logged-in"), "not-logged-in", as.character(user_name)))
     return(data_out)
}



#### FLATTENING FUNCTIONS - this is going to drop classifications
grab_classification_metadata <- function(q_data) {
     out_times <- q_data %>% 
          as.tbl_json(., json.column = "metadata") %>%
          spread_values(started_at = jstring("started_at"), 
                        finished_at = jstring("finished_at"), 
                        user_agent = jstring("user_agent")) 
     
     out_viewport <- q_data %>% 
       as.tbl_json(., json.column = "metadata") %>%
       enter_object("viewport") %>%
       spread_values(viewport_width = jstring("width"),
                     viewport_height = jstring("height"))
     
     out <- left_join(out_times, out_viewport)
     return(out)
}



grab_mobile_info <- function(data, mobile_pattern = "(mobile|iphone|android)", ipad = "ipad", app = "mobile app") {
     out <- data %>% 
          mutate(., mobile = str_detect(str_to_lower(user_agent), pattern = mobile_pattern), 
                    ipad =   str_detect(str_to_lower(user_agent), pattern = ipad),
                    app =    str_detect(str_to_lower(user_agent),  pattern = app)) %>%
          mutate(., device = ifelse(app == T, "swipe_app", 
                                    ifelse(ipad == T, "ipad",
                                           ifelse(mobile == T, "mobile", "computer"))))
     return(out)
     
}

keep_metadata_rows <- function(data) {
     data_out <- data %>% select(., subject_ids, 
                                             classification_id, 
                                             user_name, 
                                             user_id, 
                                             user_ip, 
                                             workflow_id, 
                                             workflow_name, 
                                             workflow_version,
                                             created_at,
                                             started_at,
                                             finished_at,
                                             #mobile, ipad, app,
                                             device)
}

