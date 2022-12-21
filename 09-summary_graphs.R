
# Packages ----------------------------------------------------------------


library(tidyverse)
library(here)
library(glue)
library(flextable)
library(gt)
library(paletteer)
library(gtsummary)
library(patchwork)
library(survival)


# Themes ------------------------------------------------------------------


# Flextable Theme
big_border = officer::fp_border(color="black", width = 0.5)
theme_ft_surial <- function(x) {
  x %>% 
    border_remove() %>% 
    hline_bottom(part = "header", border = big_border) %>%
    hline_bottom(part = "body", border = big_border) %>%
    bold(part = "header") %>% 
    font(fontname = "Tahoma", part = "all") %>% 
    fontsize(size = 9, part = "all") %>% 
    padding(padding.top = 2, padding.bottom = 2) %>% 
    autofit() 
}

# Theme for graphs
theme_kaplan <- function(..., base_size = 12, base_family = "Roboto") {
  survminer::theme_survminer(base_family = base_family, base_size = base_size) +
    theme(axis.line = element_line(color = "grey60"), 
          axis.ticks = element_line(color = "grey60"), 
          axis.text.x = element_text(margin = margin(t = 2, unit ="mm")),
          axis.text.y = element_text(margin = margin(r = 2, unit ="mm")), 
          axis.title.x = element_text(margin = margin(t = 4, unit = "mm")),
          axis.title.y = element_text(margin = margin(r = 4, unit = "mm")), 
          legend.position = c(0.3, 0.8), 
          panel.grid.major.y = element_line(color = "grey95", size = 0.5),
          ...
    )
}




# Load Data ---------------------------------------------------------------

cc_df <- bernr::pro_read("08-studypop.rds")
imp_df <- bernr::pro_read("08-imp_long_df.rds")
imp_noafrican_df <- bernr::pro_read("08a-imp_long_df_noafrican.rds")


# Histogram Page-B --------------------------------------------------------

cc_cases <- cc_df %>% 
  filter(hcc == TRUE) %>% 
  mutate(page_b_cat = fct_recode(page_b_cat,
    "Page-B ≤ 9" = "Score ≤9",
    "Page-B 10-17" = "Score 10-17",
    "Page-B ≥ 18" = "Score ≥18"
  )) %>% 
  ggplot(aes(x = page_b)) +
  geom_dotplot(aes(fill = page_b_cat), color = "grey30", binwidth = 1, 
               stroke = 2, dotsize = 0.8) + 
  scale_x_continuous(breaks = seq(0, max(cc_df$page_b, na.rm = T), 2), 
                     limits = c(0, max(cc_df$page_b, na.rm = T))) + 
  scale_fill_paletteer_d("colorblindr::OkabeIto") + 
  theme_kaplan() + 
  theme(legend.position = "None",
        axis.line.y = element_blank(),
        plot.margin = margin(t = 5, b = 15, r = 5, l = 5, unit = "mm")) + 
  scale_y_continuous(NULL, breaks = NULL) + 
  labs(x = "PAGE-B", fill = NULL, 
       subtitle = "(**B**) Hepatocellular Carcinoma Cases")
 
cc_histogram <- cc_df %>% 
  ggplot(aes(x = page_b)) + 
  geom_histogram(binwidth = 1, 
                 aes(fill = page_b_cat), color = "grey30") + 
  scale_x_continuous(breaks = seq(0, max(cc_df$page_b, na.rm = T), 2)) + 
  scale_fill_paletteer_d("colorblindr::OkabeIto") + 
  theme_kaplan() + 
  theme(legend.position = "None",
        plot.margin = margin(t = 15, b = 5, r = 5, l = 5, unit = "mm")) + 
  labs(x = "PAGE-B", y = "Count",
       subtitle = "(**A**) Distribution of Page-B Values")


cc <- cc_histogram / cc_cases & 
  theme(plot.subtitle = ggtext::element_markdown(hjust = 0.5,
                                                 color = "grey50"),
        plot.margin = margin(r = 3, 
                             l = 3, 
                             b = 3, t = 3, unit = "mm"))

ggsave(here("graphs", "09-page_b_distribution_cc.png"), 
       dpi = 300, bg = "white", width = 10, height = 10)





imp_cases <- imp_df %>% 
  filter(.imp == 1) %>% 
  filter(hcc == TRUE) %>% 
  mutate(page_b_cat = fct_recode(page_b_cat,
                                 "Page-B ≤ 9" = "Score ≤9",
                                 "Page-B 10-17" = "Score 10-17",
                                 "Page-B ≥ 18" = "Score ≥18"
  )) %>% 
  ggplot(aes(x = page_b)) +
  geom_dotplot(aes(fill = page_b_cat), color = "grey30", binwidth = 1, 
               stroke = 2, dotsize = 0.8) + 
  scale_x_continuous(breaks = seq(0, max(cc_df$page_b, na.rm = T), 2), 
                     limits = c(0, max(cc_df$page_b, na.rm = T))) + 
  scale_fill_paletteer_d("colorblindr::OkabeIto") + 
  theme_kaplan() + 
  theme(legend.position = "None",
        axis.line.y = element_blank(),
        plot.margin = margin(t = 5, b = 15, r = 5, l = 5, unit = "mm")) + 
  scale_y_continuous(NULL, breaks = NULL) + 
  labs(x = "PAGE-B", fill = NULL, 
       subtitle = "(**D**) Hepatocellular Carcinoma Cases")

imp_histogram <- imp_df %>% 
  filter(.imp == 1) %>% 
  ggplot(aes(x = page_b)) + 
  geom_histogram(binwidth = 1, 
                 aes(fill = page_b_cat), color = "grey30") + 
  scale_x_continuous(breaks = seq(0, max(cc_df$page_b, na.rm = T), 2)) + 
  scale_fill_paletteer_d("colorblindr::OkabeIto") + 
  theme_kaplan() + 
  theme(legend.position = "None",
        plot.margin = margin(t = 15, b = 5, r = 5, l = 5, unit = "mm")) + 
  labs(x = "PAGE-B", y = "Count",
       subtitle = "(**C**) Distribution of Page-B Values")


imp <- imp_histogram / imp_cases & 
  theme(plot.subtitle = ggtext::element_markdown(hjust = 0.5,
                                                 color = "grey50"),
        plot.margin = margin(r = 3, 
                             l = 3, 
                             b = 3, t = 3, unit = "mm"))

ggsave(here("graphs", "09-page_b_distribution_imp.png"), 
       dpi = 300, bg = "white", width = 10, height = 10)


# All combined
cc_histogram <- cc_histogram +
  scale_y_continuous(limits = c(0, 500)) + 
  labs(title = "Complete Case Dataset")

imp_histogram <- imp_histogram + 
  scale_y_continuous(limits = c(0, 500)) + 
  labs(title = "Imputation Dataset")

(cc_histogram + imp_histogram) / (cc_cases + imp_cases) & 
  theme(plot.subtitle = ggtext::element_markdown(hjust = 0.5,
                                                 color = "black", 
                                                 size = 14,
                                                 margin = margin(t = 5, b = 2, unit = "mm")),
        plot.title.position = "panel",
        plot.title = ggtext::element_markdown(hjust = 0.5, size = 16,
                                              color = "grey50",
                                              margin = margin(b = 7, unit = "mm")),
        plot.margin = margin(r = 3, 
                             l = 3, 
                             b = 3, t = 3, unit = "mm"))

ggsave(here("graphs", "09-page_b_distribution_all.png"), 
       dpi = 300, bg = "white", width = 10, height = 9)

ggsave(here("graphs", "09-page_b_distribution_all.pdf"), 
       dpi = 300, bg = "white", width = 10, height = 9, device = cairo_pdf)




# Histogram LP ------------------------------------------------------------

cc_df %>% 
  select(LP) %>% 
  mutate(data = "Complete Case Dataset") %>% 
  bind_rows(imp_df %>% 
              filter(.imp == 1) %>% 
              select(LP) %>% 
              mutate(data = "Imputation Dataset")) %>% 
  ggplot(aes(x = LP)) + 
  geom_density(fill = "grey60", color = "grey20", alpha = 0.6, 
               size = 0.8) +
  facet_wrap(~data, nrow = 2) + 
  theme_kaplan() + 
  theme(strip.background = element_blank(), 
        strip.text = element_text(size = 14)) + 
  labs(x = "Prognostic Index", 
       y = "Density")

ggsave(here("graphs", "09-prognostic_index_density.png"), 
       dpi = 300, bg = "white", width = 10, height = 12)

cc_df %>% 
  select(LP) %>% 
  mutate(data = "Complete Case Dataset") %>% 
  bind_rows(imp_df %>% 
              filter(.imp == 1) %>% 
              select(LP) %>% 
              mutate(data = "Imputation Dataset")) %>% 
  ggplot(aes(x = LP)) + 
  geom_histogram(fill = "grey60", color = "grey20", alpha = 0.6, 
               size = 0.4) +
  facet_wrap(~data, nrow = 2) + 
  theme_kaplan() + 
  theme(strip.background = element_blank(), 
        strip.text = element_text(size = 14)) + 
  labs(x = "Prognostic Index", 
       y = "Frequency")

ggsave(here("graphs", "09-prognostic_index_histogram.png"), 
       dpi = 300, bg = "white", width = 10, height = 8)



# Description HCC cases ---------------------------------------------------

vars <- c("male", "age", "caucasian", "origin_cat", 
          "transmission_risk", "prior_xtc", "prior_xtc_years", "plt", 
          "plt_c", "hbe_baseline", "any_hdv", 
          "cd4_baseline", "bmi_baseline", "co_dia", "hcv_coinfection", 
          "apri_above_2", "cirrhosis", "alt_baseline", "time_y2")


ft <- cc_df %>% 
  filter(event2 == TRUE) %>% 
  select(cohort, one_of(vars)) %>% 
  droplevels() %>% 
  mutate(across(c(male, caucasian, origin_cat, transmission_risk, prior_xtc,
                  plt_c, hbe_baseline, any_hdv, co_dia, hcv_coinfection,
                  apri_above_2, cirrhosis), 
                ~factor(.x) %>% fct_explicit_na(na_level = "(Missing)"))) %>% 
  tbl_summary(label = list(male ~ "Male sex",
              age ~ "Age (years)", 
              caucasian ~ "Caucasian", 
              origin_cat ~ "Origin",
              transmission_risk ~ "Transmission Group",
              prior_xtc ~ "3TC/FTC use before TFV", 
              prior_xtc_years ~ "Prior 3TC/FTC (years)", 
              plt ~ "Platelets (10^3)", 
              plt_c ~ "Platelets (10^3)",
              hbe_baseline ~ "HBeAg positive", 
              any_hdv ~ "HDV coinfection", 
              cd4_baseline ~ "CD4 cell count", 
              bmi_baseline ~ "BMI (kg/m2)", 
              co_dia ~ "Diabetes", 
              hcv_coinfection ~ "HCV coinfection", 
              apri_above_2 ~ "APRI > 2", 
              cirrhosis ~ "Liver cirrhosis",
              alt_baseline ~ "ALT at baseline (IU/L)", 
              time_y2 ~ "Follow-up (years)"),
              type = all_continuous() ~ "continuous2",
              missing = "no",
              statistic = all_continuous() ~ c("{median} ({p25} to {p75})",
                                               "{N_miss} ({p_miss}%)")) %>% 
  modify_table_body(
    dplyr::mutate,
    label = ifelse(label == "N missing (% missing)",
                   "(Missing)",
                   label)
  ) %>% 
  bold_labels() %>%
  as_flex_table() %>% 
  theme_ft_surial()


ft_names <- dim(ft)$widths %>% names() # names from the table

first_col <- dim_pretty(ft)$widths[1] # first row is "pretty"

# Rest will be evenly spaced
other_cols <- (6.3 - first_col)/ length(dim_pretty(ft)$widths[-1])

ft %>% 
  width(j = as.formula(glue("~{ft_names[1]}")), 
        width = first_col) %>% 
  width(j = as.formula(paste0("~",glue_collapse(ft_names[-1], sep = " + "))), 
        width = other_cols) %>% 
  save_as_docx(path = here("tables", "09-description_HCC_cases.docx"))


read_csv("plot-data-der.csv") %>% 
  mutate(x = round(x, 0)) %>% 
  mutate(y = round(y * 100, 0)) %>% 
  mutate(group = "derivation") %>% 
  bind_rows(read_csv("plot-data-val.csv") %>% 
              filter(x > 0) %>% 
              mutate(x = round(x, 0)) %>% 
              mutate(y = round(y, 0)) %>% 
              mutate(group = "validation")) %>% 
  mutate(page_b = rep(rep(c("Page B ≤ 9", "Page B 10-17", "Page B ≥ 18"), each = 4), 2)) %>% 
  print(n = 24)
  


# The patients with PAGE-B < 10 and HCC
cc_df %>% filter(hcc == TRUE & page_b < 10) %>% 
  select(c_id, gender, origin_cat, cirrhosis, cirrhosis_basis, plt, page_b)





# Distribution of Page-B --------------------------------------------------


cc_df %>% 
  janitor::tabyl(page_b_cat) %>% 
  janitor::adorn_pct_formatting() 

imp_df %>% 
  filter(.imp == 1) %>% 
  janitor::tabyl(page_b_cat) %>% 
  janitor::adorn_pct_formatting()



# Page-B and HCC risk - Predictions ---------------------------------------

m_imp <- coxph(Surv(event = event2, time = time_y2) ~ page_b, 
               data = imp_df %>% filter(.imp == 1))
m_cc <- coxph(Surv(event = event2, time = time_y2) ~ page_b, 
               data = cc_df)

m_imp_noafr <- coxph(Surv(event = event2, time = time_y2) ~ page_b, 
                     data = imp_noafrican_df %>% filter(.imp == 1))

time_arg <- 15
newdat <- tibble(page_b = seq(1, 25, 0.1))


fit_imp <- survfit(m_imp, newdata = newdat, times = time_arg, conf.type = "plain")
fit_noafr <- survfit(m_imp_noafr, newdata = newdat, times = time_arg, conf.type = "plain")

sum_imp<- summary(fit_imp, times = time_arg)
sum_noafr<- summary(fit_noafr, times = time_arg)

tibble(x = sum_noafr$cumhaz[1,], 
       y = 1-sum_noafr$surv[1,]) %>% 
  mutate(d = x - y) %>% 
  arrange(desc(d))

sum_imp$surv
cumhaz_imp <- newdat %>% 
  mutate(cum_haz = sum_imp$surv[1,],
         clow = sum_imp$lower[1,],
         chi = sum_imp$upper[1,])

cumhaz_noafr <- newdat %>% 
  mutate(cum_haz = sum_noafr$surv[1,],
         clow = sum_noafr$lower[1,],
         chi = sum_noafr$upper[1,])




cumhaz_comb <- cumhaz_imp %>% 
  mutate(group = "(**A**) Full Dataset") %>% 
  bind_rows(cumhaz_noafr %>% mutate(group = "(**B**) Non-African Individuals"))
  
cumhaz_comb <- cumhaz_comb %>% 
  mutate(page_b_cat = case_when(page_b < 10 ~ "PAGE-B ≤ 9", 
                                page_b >= 10 & page_b < 18 ~ "PAGE-B 10-17", 
                                page_b >= 18 ~ "PAGE-B ≥ 18"), 
         page_b_cat = fct_inorder(page_b_cat))

cumhaz_comb %>% 
  ggplot(aes(x = page_b, y = 1-cum_haz)) + 
  geom_ribbon(aes(ymin = 1-clow, ymax = 1-chi, fill = page_b_cat), alpha = 0.2) +
  geom_hline(yintercept = 0.002 * time_arg, size = 0.3, color = "firebrick", 
             linetype = 2) + 
  geom_line(size = 0.75, aes(color = page_b_cat)) +
  geom_point(data = cumhaz_comb %>% group_by(group) %>% 
               slice(which.min(abs((1-clow) - 0.002 * time_arg))), 
             color = "grey60") + 
  ggrepel::geom_text_repel(aes(label = glue::glue("PAGE-B: {floor(page_b)}")), 
                           data = cumhaz_comb %>% group_by(group) %>% 
                             slice(which.min(abs((1-clow) - 0.002 * time_arg))), 
                           nudge_x = -6, nudge_y = 0.15, 
                           family = "Roboto", fontface = "bold", 
                           color = "grey60") +
  facet_wrap(~group, nrow = 1) + 
  scale_x_continuous(breaks = seq(0, 25, 2)) + 
  scale_fill_paletteer_d("colorblindr::OkabeIto") + 
  scale_color_paletteer_d("colorblindr::OkabeIto") + 
  labs(x = "PAGE-B", 
       y = glue("Cumulative {time_arg}-Year HCC-Risk")) + 
  theme_kaplan() + 
  theme(strip.background = element_blank(), 
        strip.text = ggtext::element_markdown(size = 14, color = "grey50"),
        legend.position = "None")

ggsave(here("graphs", "09-hcc_risk_graph.png"), 
       dpi = 300, width =8, height = 4)

ggsave(here("graphs", "09-hcc_risk_graph.pdf"), 
       dpi = 300, width = 8, height = 4, device = cairo_pdf)



# Look at optimal cutoff, averaged over several years
page_b_cutoff <- numeric(length = 15)
for (i in 1:15) {
  time_arg <- i
  
  ff_new <- survfit(m_cc, 
                    newdata = tibble(page_b = seq(1, 25, 0.1)), 
                    times = time_arg, conf.type = "plain")
  
  x <- summary(ff_new, times = time_arg)
  
  dff <- tibble(page_b = seq(1, 25, 0.1), 
                cum_haz = x$cumhaz[1,],
                clow = x$lower[1,],
                chi = x$upper[1,])
  page_b_cutoff[i] <- dff %>% 
    slice(which.min(abs((1-clow) - 0.002 * time_arg))) %>% 
    pull(page_b)
}

# Optimal cutoffs
tibble(page_b_cutoff, 
       time = 1:15) %>% 
  ggplot(aes(x = time, y = page_b_cutoff)) +
  geom_line()
