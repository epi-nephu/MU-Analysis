# Calculate the number of MU cases by suburb in endemic inner north melbourne

mu_inner_nth_suburb <- mu_in_north %>% 
  filter(lga %in% c("Moreland", "Moonee Valley")) %>% 
  dplyr::distinct(event_id, .keep_all=TRUE) %>% 
  mutate(city = str_to_title(city))

length(unique(mu_inner_nth_suburb$event_id))
tabyl(mu_inner_nth_suburb, city)

yrly_breakdown <- tabyl(mu_inner_nth_suburb, epiyear, city)


inclu_suburb <- c("Brunswick", "Brunswick West", "Coburg", "Pascoe Vale South", 
                  "Essendon", "Moonee Ponds", "Strathmore", "Ascot Vale", 
                  "Thornbury", "Preston")

endemic <- c("Brunswick", "Brunswick West", "Coburg", "Pascoe Vale South", 
             "Essendon", "Moonee Ponds", "Strathmore", "Ascot Vale")
non_endemic <- c("Thornbury", "Preston")

mu_suburb_comp <- mu_in_north %>% 
  dplyr::distinct(event_id, .keep_all=TRUE) %>% 
  mutate(city = str_to_title(city)) %>% 
  filter(city %in% inclu_suburb)

mu_suburb_comp_table <- mu_suburb_comp %>% 
  tabyl(epiyear, city)


# Construct a master df with all months from start date to end date
# start_date <- ymd("2022-01-01")
# end_date <- ymd("2025-10-31")

monthly_sequence <- seq(start_date, max_date, by = "month")

monthly_seq_df <- data.frame(epi_MY = monthly_sequence) %>% # code multiplies row by row n times, each  = number of LGAs
  slice(rep(1:n(), each = 8))

monthly_seq_df <- do.call(rbind, replicate(8, data.frame(epi_MY = monthly_sequence), simplify = FALSE)) %>% 
  # code multiplies block by block n times
  mutate(lga = rep(inclu_lga, each=47)) # each = number of months in required period ie: 12mths x 3 yrs + 10mths


# Calculate monthly counts
counts_mth_lga <- left_join(monthly_seq_df, 
                            mu_in_north %>% dplyr::distinct(event_id, .keep_all=TRUE) %>% group_by(lga, epi_MY) %>% summarise(total = n()), 
                            by=c("lga", "epi_MY")) %>% 
  replace_na(replace=list(total=0)) 


library(ggpattern)

# Plot graph of cases over time
mu_suburb_comp_table %>% 
  pivot_longer(cols=-epiyear, names_to="suburb", values_to="count") %>% 
  mutate(endemic = case_when(suburb %in% endemic ~ "endemic", 
                             suburb %in% non_endemic ~ "non-endemic"), 
         suburb = factor(suburb, levels=inclu_suburb)) %>% 
  ggplot() + 
  geom_col_pattern(aes(x=epiyear, y=count, fill=suburb, pattern_density=endemic), position="dodge", pattern_fill = "gray", 
                   pattern = "stripe", 
                   pattern_angle = 45,
                   pattern_spacing = 0.01) + 
  scale_fill_manual(values=c("yellow", "red", "orange", "firebrick4", "pink", "darkorange3", "hotpink3", "green", "blue", "skyblue")) + 
  scale_y_continuous(expand=c(0,0), name="Notified cases", breaks=integer_breaks()) + 
  scale_x_continuous(expand=c(0,0)) + 
  labs(x="", 
       pattern_density = "")
  

scale_pattern_manual(values=c("", "crosshatch"))




mth_cases_graph <- ggplot(data = counts_mth_lga) +
  #geom_col(aes(x = epi_MY, y=total_nephu, group = 1, fill = "MU cases"), col="gray") + 
  geom_line(aes(x= epi_MY, y=total, group=1, col=lga), linewidth=1) + 
  #scale_fill_manual(values="#F8766D", name="") + 
  #scale_colour_manual(values=c("#619CFF"), name="") + 
  scale_x_date(expand=c(0,0), date_breaks="3 months", date_minor_breaks="month", date_labels="%b %Y", name="") +
  scale_y_continuous(expand=c(0,0), name="Monthly notified cases", breaks=integer_breaks()) + 
  cowplot::theme_cowplot() + 
  theme(plot.caption = element_text(size=11), 
        axis.text.x = element_text(angle = 45, hjust=1))



# Plot cumulative graph
cumsum_data <- counts_mth_lga %>% 
  mutate(yr = year(epi_MY), 
         mth = month(epi_MY, label=TRUE, abbr=TRUE)) %>% 
  group_by(lga, yr) %>% 
  mutate(cum_count = cumsum(total), 
         lga = fct(lga, levels =c("Mornington Peninsula", "Greater Geelong", "Moreland", "Moonee Valley", "Boroondara", "Banyule", "Darebin", "Yarra")))

cumsum_plot <- ggplot(cumsum_data) + 
  geom_line(aes(x=epi_MY, y=cum_count, group=lga, col=lga), linewidth=1) + 
  scale_color_manual(values=c("red", "orange", "firebrick4", "pink", "yellow", "green", "blue", "skyblue")) + 
  scale_x_date(expand=c(0,0), date_breaks="3 months", date_minor_breaks="month", date_labels="%b %Y", name="") +
  scale_y_continuous(expand=c(0,0), breaks=integer_breaks()) + 
  cowplot::theme_cowplot() + 
  theme(plot.title = element_text(size=14), 
        plot.caption = element_text(size=11), 
        axis.text.x = element_text(angle = 45, hjust=1)) + 
  labs(title="Cumulative monthly case numbers per year", 
       #caption=paste0("Data period: ", start_date, " to ", max_date), 
       x="", 
       y="Cumulative case numbers", 
       col="LGA")

ggplotly(cumsum_plot)
