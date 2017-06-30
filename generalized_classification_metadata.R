
# read in data

#### FLATTENING FUNCTIONS
grab_classification_metadata <- function(q_data) {
     out <- q_data %>% 
          as.tbl_json(., json.column = "metadata") %>%
          spread_values(started_at = jstring("started_at"), 
                        finished_at = jstring("finished_at"), 
                        user_agent = jstring("user_agent")) %>%
          enter_object("viewport") %>%
          spread_values(viewport_width = jstring("width"),
                        viewport_height = jstring("height"))
     return(out)
}



grab_mobile_info <- function(data) {
     out <- data %>% mutate(., mobile = str_detect(user_agent, pattern = mobile_strings), 
                            ipad = str_detect(user_agent, pattern = ipad)) %>%
          mutate(., device = ifelse(mobile == T, "mobile",
                                    ifelse(ipad == T, "ipad", 
                                           "computer")))
     return(out)
     
}



# run

data_out <- data_in %>% 
     grab_classification_metadata %>% 
     grab_mobile_info %>% 
     select(., subject_ids, 
               classification_id, 
               user_name, 
               user_id, 
               user_ip, 
               workflow_id, 
               workflow_name, 
               workflow_version,
               created_at,
               annotations,
               started_at,
               finished_at,
               device)

