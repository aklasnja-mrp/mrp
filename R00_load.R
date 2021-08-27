library(officer) # plots to ppt 
library(rvg)
library(magrittr)
library(psych) # descriptive stats by grp
#library(lessR) # convenient cross-tab 
#library(bestNormalize) # fit yeo-johnson transform
library(rstatix) # ttest wrapper for piping
library(GGally) # convenient corrplot
library(ggpubr) # ggplot wrapper with themes 
library(ggsci)
library(tidyverse)
library(broom) 

library(quantreg) # fit quantile regression 
library(rms) # cv of quantile regression goodness-of-fit

dir <- "~/2021/retrievals/"
ppt <- paste0(dir, "plots_final.pptx")
doc <- read_pptx(ppt)

print_png_to_ppt <- function(png, k, c, y, ppt){
  doc <- doc %>% add_slide(layout="Title and Content", master="Office Theme") %>%
    ph_with(location=ph_location_type(type="title"), fpar(ftext(paste(y," ~ ",c," - Rank: ",k), prop=fp_text(font.size=16)))) %>%
    ph_with(location=ph_location_type(type="body"), value=external_img(png), use_loc_size=TRUE)
  print(doc, ppt) 
}
print_gglist_to_ppt <- function(plist, k, c, y, ppt){
  for(f in 1:length(plist)) {
    doc <- doc %>% add_slide(layout="Title and Content", master="Office Theme") %>% 
      ph_with(location=ph_location_type(type="title"), fpar(ftext(paste(y," ~ ",c," - Rank: ",k), prop=fp_text(font.size=16)))) %>%
      ph_with(dml(ggobj=plist[[f]]), location=ph_location_type(type="body")) 
  }
  print(doc, ppt) 
}

# from https://www.r-bloggers.com/2017/12/combined-outlier-detection-with-dplyr-and-ruler/
get_outliers <- function(data_tbl, colA, colB) {
  isnt_out_z <- function(x, thres = 3, na.rm = TRUE) {
    abs(x - mean(x, na.rm = na.rm)) <= thres * sd(x, na.rm = na.rm)
  }
  isnt_out_mad <- function(x, thres = 3, na.rm = TRUE) {
    abs(x - median(x, na.rm = na.rm)) <= thres * mad(x, na.rm = na.rm)
  }
  isnt_out_tukey <- function(x, k = 1.5, na.rm = TRUE) {
    quar <- quantile(x, probs = c(0.25, 0.75), na.rm = na.rm)
    iqr <- diff(quar)
    (quar[1] - k * iqr <= x) & (x <= quar[2] + k * iqr)
  }
  maha_dist <- . %>% select_if(is.numeric) %>%
    mahalanobis(center = colMeans(.), cov = cov(.))
  isnt_out_maha <- function(tbl, isnt_out_f, ...) {
    tbl %>% maha_dist() %>% isnt_out_f(...)
  }
  isnt_out_funs <- funs(
    z = isnt_out_z,
    mad = isnt_out_mad,
    tukey = isnt_out_tukey
  )
  data_tbl <- data_tbl %>%
    unite(col = "group", colA, colB)
  compute_group_non_outliers <- . %>% # Compute per group mean values of columns
    group_by(group) %>%
    summarise_if(is.numeric, mean) %>%
    ungroup() %>% # Detect outliers among groups
    mutate_if(is.numeric, isnt_out_funs) %>% # Remove unnecessary columns
    select_if(Negate(is.numeric))
  data_tbl %>% compute_group_non_outliers()
  row_packs_isnt_out <- row_packs( # Non-outliers based on some column
    column = . %>% transmute_if(is.numeric, isnt_out_funs), # Non-outliers based on Mahalanobis distance
    maha = . %>% transmute(maha = maha_dist(.)) %>%
      transmute_at(vars(maha = maha), isnt_out_funs)
  )
  #group_packs_isnt_out <- group_packs( # Non-outliers based on grouping
    #group = compute_group_non_outliers,
    #.group_vars = "group"
  #)
  full_report <- data_tbl %>%
    expose(row_packs_isnt_out, #group_packs_isnt_out,
           .remove_obeyers = FALSE) %>%
    get_report()
  used_rules <- full_report %>%
    distinct(pack, rule)
  breaker_report <- full_report %>%
    filter(!(value %in% TRUE))
  group_breakers <- breaker_report %>%
    # Filter group packs
    filter(pack == "group") %>%
    # Expand rows by matching group with its rows
    select(-id) %>%
    left_join(
      y = data_tbl %>% transmute(var = group, id = 1:n()),
      by = "var"
    ) %>%
    select(pack, rule, var, id, value)
  outliers <- bind_rows(
    breaker_report %>% filter(pack != "group"),
    group_breakers
  ) %>%
    select(pack, rule, id)
  outlier_score <- outliers %>%
    group_by(id) %>%
    # nrow(used_rules) equals total number of applied methods
    summarise(score = n() / nrow(used_rules))
  return(outlier_score)
}
# use example
# df <- df %>% filter(rank==10)
# outlier_score <- get_outliers(df, "male", "feml")
# df <- df %>%
#   mutate(id = 1:n()) %>%
#   left_join(y = outlier_score, by = "id") %>%
#   mutate(
#     score = coalesce(score, 0),
#     is_out = if_else(score > 0.2, "Outlier", "Not outlier")
#   )
