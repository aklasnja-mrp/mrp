
#load file to add "bm25-neutral", "bm25-female", "bm25-male"
r0 <- read.csv(paste0(dir, "r_qe.csv"), stringsAsFactors=TRUE)
r0 <- r0 %>% select(-c("X","query_no","qid_text","query_text"))
labvar <- c("qid","method","dataset")
yvar <- c(names(r0)[grep('tf|bool', names(r0))])
yvar <- yvar[grep('bias', yvar)]

pre <- r0[which(r0$method=="none"), c(labvar, yvar)] %>% 
  pivot_longer(!!yvar, names_to="yvar", values_to="pre")
pre[which(pre$pre<0), "bm25"] <- "male" 
pre[which(pre$pre>0), "bm25"] <- "female"
pre[which(pre$pre==0), "bm25"] <- "neutral"
bm25_levels <- c("neutral", "female", "male")
pre$bm25 <- factor(pre$bm25, levels=bm25_levels)

r <- read.csv(paste0(dir, "ratio_qe.csv"), stringsAsFactors=TRUE)
r <- r %>% select(-c("index","qid.1","dataset.1","method.1"))
labvar <- c("qid","method","dataset","class_0", "class_1", "class_2")
statsvar <- setdiff(names(r), labvar)
r[statsvar] <- sapply(r[statsvar], as.numeric)

class_0_levels <- c("none", "global_analysis", "local_analysis")
class_1_levels <- c("none", "stemming", "semantic", "web", "prf")
class_2_levels <- c("none", "krovetz","lovins","paicehusk","porter","porter2","sstemmer","trunc4","trunc5","tagmee","thesaurus","wordnet","conceptnet","sensedisambiguation", "anchor","glove","wiki","word2vec", "adaponfields","bertqe","concept_cluster","doc_cluster","onfields","relevancefeedback","rm3","termluster")
r$class_0 <- factor(r$class_0, levels=class_0_levels)
r$class_1 <- factor(r$class_1, levels=class_1_levels)
r$class_2 <- factor(r$class_2, levels=class_2_levels)

#load file to add gender
ref <- read.csv("~/2021/reference_tables/ref_queries.csv", stringsAsFactors=TRUE)
r <- r %>% 
  left_join(ref[,c("qid","method","dataset","gender")])
labvar <- c(labvar,"gender")
statsvar <- setdiff(names(r), labvar)

yvar <- c(names(r0)[grep('tf|bool', names(r0))])
yvar <- yvar[grep('male|feml', yvar)]

df <- r %>%
  select(!!labvar, !!yvar) %>%
  pivot_longer(!!yvar, names_to="yvar", values_to="x") %>%
  separate(yvar, into=c('variable', 'rank'), '_(?=[^_]+$)') %>%
  separate(variable, into=c('var', 'type'), '_(?=[^_]+$)') %>%
  pivot_wider(names_from="type", values_from="x") 

rank_levels <- c(5,10,20,30,50)
df$rank <- factor(df$rank, levels=rank_levels)


df <- df %>% filter(rank==10)
outlier_score <- get_outliers(df, "male", "feml")
rpt<- get_outliers(df, "male", "feml")
 df <- df %>%
   mutate(id = 1:n()) %>%
   left_join(y = outlier_score, by = "id") %>%
   mutate(
     score = coalesce(score, 0),
     is_out = if_else(score > 0.2, "Outlier", "Not outlier")
   )

plist <- list()
plist[[1]] <- df %>% 
  ggplot(aes(x=male, y=feml, color=is_out)) + 
  scale_colour_manual(values = c("#AAAAAA", "#004080")) +
  guides(colour = guide_legend(title = NULL, override.aes = list(size = 4))) +
  geom_point() +
  geom_hline(yintercept=0) +
  geom_vline(xintercept=0) +
  facet_wrap(~var, scales="free") +
  theme_classic() +
  #scale_color_jco() +
  theme(legend.position='top', legend.box = 'horizontal') + 
  #theme(axis.text.x=element_text(angle=45, hjust=1)) + 
  ggtitle("") +  
  xlab("male score") + 
  ylab("feml score") +
  labs(color = "")
#print_gglist_to_ppt(plist=plist, k=k, c="none", y="", ppt)

png <- paste0(dir, paste0('png/scatter_plot_male_feml_10.png'))
ggsave(png, plot=plist[[1]], width=7, height=7)
#doc <- read_pptx(ppt)
#print_png_to_ppt(png, k="all ranks", c="ratio by ds and bm25", y="", ppt)
}


