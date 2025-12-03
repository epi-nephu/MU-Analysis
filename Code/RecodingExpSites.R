# Script to recode some data values for MU Surveillance presentation
# recoding some of the exposure sites coordinates to reflect residential location for risk unable to be determined
# cos this is not properly reflected in PHAR even though PHESS is correctly recorded

rud_cases_nephu <- mu_raw %>% 
  clean_names() %>% 
  dplyr::filter(risk_factor=="Risk unable to be determined") %>% 
  mutate(lga = str_remove(lga, "\\s\\((C|S)\\)"), 
         lga_2 = str_remove(lga_2, "\\s\\((C|S)\\)")) %>% 
  filter(lga %in% nephu_lgas) %>% 
  select(event_id, lga, latitude, longitude, primary_exposure, risk_factor, name, 
         risk_factors_exposure_site, lga_2, risk_factors_exposure_site_latitude, risk_factors_exposure_site_longitude)

# recoding section

case_ids <- "320256116840"

mu_data_config <- mu_data_config %>% 
  mutate(exp_site_lga = case_when(event_id==case_ids & primary_exposure=="Primary" ~ lga, 
                           TRUE ~ exp_site_lga), 
         exp_site_latitude = case_when(event_id==case_ids & primary_exposure=="Primary" ~ latitude, 
                                       TRUE ~ exp_site_latitude), 
         exp_site_longitude = case_when(event_id==case_ids & primary_exposure=="Primary" ~ longitude, 
                                        TRUE ~ exp_site_longitude)
  )

