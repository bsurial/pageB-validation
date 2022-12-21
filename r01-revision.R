# Revision
# Run 08-validation until line 201 first.


## HIV RNA at baseline

time_sub %>% 
  mutate(rna_cat = case_when(rna_baseline >= 200 ~ "≥200 cp/mL", 
                             rna_baseline >= 50 ~ "≥50 and <200 cp/mL", 
                             rna_baseline < 50 ~ "below 50 cp/mL", 
                             TRUE ~ "Missing")) %>% 
  select(cohort, rna_cat) %>% 
  tbl_summary(by = cohort) %>% 
  add_overall()




## Alcohol

subs_aqui <- raw_read_aqui("tblvis_subs") %>% 
  filter(subs_id == "ALCO") %>% 
  rename(alcohol_y = subs_y)

subs_es <- raw_read_eu("tblvis") %>% 
  select(patient, vis_d, alcohol_y) %>% 
  mutate(alcohol_y = na_if(alcohol_y, "NULL"), 
         alcohol_y = na_if(alcohol_y, "9"), 
         alcohol_y = as.numeric(alcohol_y)) %>% 
  mutate(patient = as.character(patient)) %>% 
  rename(subs_d = vis_d)


sm <- time_sub %>% 
  select(p_id, cohort, tfv_sd) %>% 
  mutate(tfv_min = tfv_sd - 365.25, 
         tfv_max = tfv_sd + 365.25) 



sm %>% 
  filter(cohort == "Aquitaine") %>% 
  left_join(subs_aqui, by = join_by(p_id == patient, 
                                    tfv_min <= subs_d, 
                                    tfv_max >= subs_d)) %>% 
  mutate(delta = abs(as.numeric(subs_d - tfv_sd))) %>% 
  arrange(p_id, delta) %>% 
  group_by(p_id) %>% 
  slice(1) %>% 
  ungroup() %>% 
  select(p_id, cohort, tfv_sd, alcohol_y) %>% 
  count(alcohol_y) %>% 
  mutate(p = n / sum(n))

sm %>% 
  filter(cohort == "Eurosida") %>% 
  left_join(subs_es, by = join_by(p_id == patient, 
                                  tfv_min <= subs_d, 
                                  tfv_max >= subs_d)) %>% 
  mutate(delta = abs(as.numeric(subs_d - tfv_sd))) %>% 
  arrange(p_id, delta) %>% 
  group_by(p_id) %>% 
  slice(1) %>% 
  ungroup() %>% 
  select(p_id, cohort, tfv_sd, alcohol_y) %>% 
  count(alcohol_y) %>% 
  mutate(p = n / sum(n))


## How many died: 

time_sub %>% 
  count(hcc, death) %>% 
  filter(hcc == "TRUE") %>% 
  mutate(p = n/sum(n))

x <- time_sub %>% 
  filter(hcc == TRUE & time_y2 != 15) %>% 
  mutate(d = if_else(death == TRUE, death_d, last_fup_d)) %>% 
  mutate(s_time = as.numeric(d - hcc_d)/365.25*12) 

x %>% 
  count(death) %>% 
  mutate(p = n/sum(n))

survfit(Surv(s_time, death) ~ 1, data = x)


## Incidence rate 


time_african <- time_sub %>% 
  select(time_y2, event2, origin_cat) %>% 
  mutate(african = origin_cat == "African")

time_african %>% 
  group_by(african) %>% 
  summarise(years = sum(time_y2), 
            events = sum(event2)) %>% 
  mutate(incidence = events / years * 1000) %>% 
  knitr::kable()

m <- glm(event2 ~ african + offset(log(time_y2)), data = time_african, 
         family = poisson(link = "log"))

tidy(m)

new <- tibble(time_y2 = 1000, 
       african = c(TRUE, FALSE))

new$est <- predict(m, newdata = new, se.fit = TRUE)$fit
new$se <- predict(m, newdata = new, se.fit = TRUE)$se.fit

new %>% 
  mutate(clo = est - 1.96 * se, 
         chi = est + 1.96 * se) %>% 
  mutate(across(c(est, clo, chi), exp))
  





