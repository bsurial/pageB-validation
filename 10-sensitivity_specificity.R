library(tidyverse)
library(here)
library(survival)
library(timeROC)


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

imp_long <- read_rds(here("processed", "08-imp_long_df.rds"))
imp_long_noafr <- read_rds(here("processed", "08a-imp_long_df_noafrican.rds"))

df_1 <- imp_long %>% filter(.imp == 1) %>% as_tibble() %>% 
  select(time_y2, event2, page_b)

df_1_noafr <- imp_long_noafr %>% filter(.imp == 1) %>% as_tibble() %>% 
  select(time_y2, event2, page_b)


nobs <- nrow(df_1)

roc_analysis <- survivalROC::survivalROC(df_1$time_y2, 
                                         df_1$event2, 
                                         marker = df_1$page_b, 
                                         predict.time = 5,
                                         span = 0.25*nobs^(-0.20), 
                                         cut.values = 10)


timeROC::SeSpPPVNPV(cutpoint=10,
                    T=df_1$time_y2,
                    delta=df_1$event2,marker=df_1$page_b,
                    cause=1,weighting="marginal",
                    times=c(5, 14.9),iid=FALSE)


timeROC::SeSpPPVNPV(cutpoint=10,
                    T=df_1_noafr$time_y2,
                    delta=df_1_noafr$event2,marker=df_1_noafr$page_b,
                    cause=1,weighting="marginal",
                    times=c(5, 14.9),iid=FALSE)


# Cut off point for Full Dataset (<0.2% per year)
timeROC::SeSpPPVNPV(cutpoint=12,
                    T=df_1$time_y2,
                    delta=df_1$event2,marker=df_1$page_b,
                    cause=1,weighting="marginal",
                    times=c(5, 14.9),iid=FALSE)

# Cut off point for Non-African (<0.2% per year)
timeROC::SeSpPPVNPV(cutpoint=13,
                    T=df_1_noafr$time_y2,
                    delta=df_1_noafr$event2,marker=df_1_noafr$page_b,
                    cause=1,weighting="marginal",
                    times=c(5, 14.9),iid=FALSE)



# ROC Curve
a <- timeROC(T=df_1$time_y2,
        delta=df_1$event2, marker=df_1$page_b,
        cause=1,weighting="aalen",
        times=c(5))

b <- timeROC(T=df_1_noafr$time_y2,
             delta=df_1_noafr$event2, marker=df_1_noafr$page_b,
             cause=1,weighting="aalen",
             times=c(5))

atib <- tibble(TP = a$TP[,2], 
       FP = a$FP[,2], 
       model = "Full dataset") 

btib <- tibble(TP = b$TP[,2], 
            FP = b$FP[,2], 
            model = "Non-African") 

bind_rows(atib, btib) %>% 
  ggplot(aes(x = FP, y = TP)) +
  geom_abline(intercept = 0, slope = 1, 
              color = "grey70", linetype = 2, size = 0.5) + 
  geom_line(aes(linetype = model), size = 0.75) + 
  scale_linetype_manual(values = c("solid", "dotted"), 
                        name = element_blank(),
                        labels = c("Full Dataset", "Non-African"),
                        guide = guide_legend(reverse = TRUE)) + 
  labs(x = "1-Specificity", 
       y = "Sensitivity", 
       linetype = NULL) + 
  theme_kaplan() + 
  theme(legend.position = c(0.75, 0.2), panel.grid.major.y = element_blank(),
        legend.text=element_text(size=11))

ggsave(here("graphs", "10-ROC_curve.png"), 
       dpi = 300, bg = "white", width = 8, height = 8)

ggsave(here("graphs", "10-ROC_curve.pdf"), 
       dpi = 300, bg = "white", width = 8, height = 8, device = cairo_pdf)
        