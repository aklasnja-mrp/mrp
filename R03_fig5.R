#load file


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

df <- r %>%
  select(!!labvar, !!yvar) %>%
  pivot_longer(!!yvar, names_to="yvar", values_to="x") %>%
  left_join(pre[,c("qid","dataset","yvar","bm25")]) %>%         #add bm25
  separate(yvar, into=c('variable', 'rank'), '_(?=[^_]+$)')

rank_levels <- c(5,10,20,30,50)
df$rank <- factor(df$rank, levels=rank_levels)
gender_levels <- c("neutral", "female", "male")
df$gender <- factor(df$gender, levels=gender_levels)

# plot frequencies of gender alignment
  plt <- df %>%
    filter(method!="none") %>% 
    filter(dataset!="msmarco") %>% group_by(gender, rank, variable) %>%
    summarise(new = list(mean_se(x))) %>% unnest(new) 
  
  plist <- list()
  #plot averages across datasets
  plist[[1]] <- plt %>% 
    ggplot(aes(x=rank, y=y, group=variable, color=variable)) +
    geom_point(position=position_dodge(0.1)) +
    geom_line(position=position_dodge(0.1), size=1) +
    geom_errorbar(aes(ymin=ymin, ymax=ymax), width=.05, position=position_dodge(0.1)) +
    geom_hline(yintercept=0) +
    facet_wrap(~gender, ncol=3, scales="free") +
    theme_classic() +
    scale_color_jco() +
    theme(legend.position='top', legend.box = 'horizontal') + 
    ggtitle("") +  
    #paste0("What is happening with gender as measured by RaB/ARaB variations)) +
    xlab("No. top documents used in RaB/ARaB calculation") + 
    ylab("Bias relative to original query") +
    labs(color = "Bias metric")
  #print_gglist_to_ppt(plist=plist, k=k, c="none", y="", ppt)
  
  png <- paste0(dir, paste0('png/bias_gender_ranks.png'))
  ggsave(png, plot=plist[[1]], width=9, height=3)
  doc <- read_pptx(ppt)
  print_png_to_ppt(png, k="all ranks", c="ratio by ds and bm25", y="", ppt)

  
  
# change for class_2
  
  
 for (g in levels(df$class_1)){
   
  # plot frequencies of gender alignment
  plt <- df %>%
    filter(class_1==g) %>%
    filter(method!="none") %>% 
    filter(dataset!="msmarco") %>% group_by(class_2, gender, rank, variable) %>%
    summarise(new = list(mean_se(x))) %>% unnest(new) 
  
  plist <- list()
  #plot averages across datasets
  plist[[1]] <- plt %>% 
    ggplot(aes(x=rank, y=y, group=class_2, color=class_2)) +
    geom_point(position=position_dodge(0.1)) +
    geom_line(position=position_dodge(0.1), size=1, alpha=0.3) +
    geom_errorbar(aes(ymin=ymin, ymax=ymax), width=.05, position=position_dodge(0.1)) +
    geom_hline(yintercept=0) +
    facet_wrap(gender~variable, scales="free") +
    theme_classic() +
    #scale_color_jco() +
    theme(legend.position='top', legend.box = 'horizontal') + 
    ggtitle("") +  
    #paste0("What is happening with gender as measured by RaB/ARaB variations)) +
    xlab("No. top documents used in RaB/ARaB calculation") + 
    ylab("Bias relative to original query") +
    labs(color = "Bias metric")
  png <- paste0(dir, paste0('png/bias_gender_class_2_ranks', g, '.png'))
  ggsave(png, plot=plist[[1]], width=9, height=4)
  
 } 

