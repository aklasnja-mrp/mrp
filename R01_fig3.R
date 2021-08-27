
#load file"
r0 <- read.csv(paste0(dir, "r_qe.csv"), stringsAsFactors=TRUE)
r0 <- r0 %>% select(-c("X","query_no","qid_text","query_text"))
labvar <- c("qid","method","dataset")
yvar <- c(names(r0)[grep('tf|bool', names(r0))])

#load file to add gender
ref <- read.csv("~/2021/reference_tables/ref_queries.csv", stringsAsFactors=TRUE)
r0 <- r0 %>% 
  left_join(ref[,c("qid","method","dataset","gender")])
labvar <- c(labvar,"gender")

plt <- r0 %>%
  filter(method == "none") %>%
  filter(gender == "neutral") %>%
  select(c(dataset, yvar)) %>%
  pivot_longer(-dataset, names_to="var", values_to="x") %>% 
  separate(var, into=c("variable", "rank"), "_(?=[^_]+$)") %>%
  group_by(dataset, variable) %>%
  summarise(new = list(mean_se(x))) %>% 
  unnest(new) %>%
  separate(variable, into=c("var", "type"), "_(?=[^_]+$)") 

p1 <- ggplot(plt, aes(x=dataset, y=y, group=type, fill=type)) +
  geom_col(position = "dodge") +
  geom_errorbar(aes(ymin = ymin, ymax = ymax), position = "dodge") +
  facet_wrap(~var, scales="free", ncol=4) +
  theme_classic() +
  scale_fill_jco() +
  theme(legend.position='bottom', legend.box = 'horizontal') 
p1 <- p1 + 
  theme(axis.text.x=element_text(angle=45, hjust=1)) 
png <- paste0(dir, 'png/compare_bias_r_ds.png')
ggsave(png, plot=p1, width=7, height=3)

doc <- read_pptx(ppt)
print_png_to_ppt(png, k="all ranks", c="r_qe by ds", y="", ppt)
