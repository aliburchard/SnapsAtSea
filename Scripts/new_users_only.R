

new_summary <- new_users_only %>% group_by(., experiment) %>% 
     summarise(., total_classifications = n(), total_users = n_distinct(user_name)) %>%
     left_join(., gini_dat)

View(new_summary)



# plot against other projects
lorenz <- all_proj_new %>% 
     group_by(., experiment, user_name) %>%
     summarise(., num_classifications = n()) %>%
     do(grab_lorenz(., class_per_user = "num_classifications"))


pdf(file = "Figures/lorenz_SAS_new_users_all_proj.pdf", width = 12, height = 7)
ggplot(lorenz, aes(x = prop_user, y = prop_class, color = experiment)) + 
     geom_line(size = 1.5) + 
     labs(x = "proportion of volunteers", y = "proportion of classifications") +
     geom_abline(intercept = 0, slope = 1, color = "gray") + theme_bw(base_size = 16)
dev.off()
