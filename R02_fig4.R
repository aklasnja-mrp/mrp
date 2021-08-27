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

#make chart of gender alignment
ck <- df %>%
  filter(method=="none") %>% 
  filter(dataset!="msmarco") %>%
  mutate(rowid=row_number())
ck <- pivot(data=ck, compute=tabulate, variable=c(), by=c(variable, bm25, gender, rank)) %>%
  filter(n > 0) %>%
  pivot_wider(names_from=rank, values_from=n) 

# plot frequencies of gender alignment
at_ranklist <- c(5,10,20,30,50) 
for (i in 1:length(at_ranklist)){
  k <- at_ranklist[i]
  plt <- df %>%
    filter(rank==k) %>%
    filter(method=="none") %>% 
    filter(dataset!="msmarco") %>%
    group_by(dataset, variable, bm25, gender) %>%
    summarise(n=n())  
  
  plist <- list()
  #plot averages across datasets
  plist[[1]] <- plt %>% complete(gender, nesting(dataset, variable, bm25), fill=list(n=0)) %>% 
    ggplot(aes(x=dataset, y=n, group=bm25, fill=bm25)) +
    geom_bar(stat='identity', position='dodge') +
    facet_wrap(gender~variable, ncol=4, scales="free") +
    theme_classic() +
    scale_fill_jco() +
    theme(legend.position='top', legend.box = 'horizontal') + 
    theme(axis.text.x=element_text(angle=45, hjust=1)) + 
    ggtitle("") +  #paste0("Interpretation of gender by RaB/ARaB metrics from baseline BM25 retrievals of top ",k)) +
    xlab("Test collection") + 
    ylab("N") +
    labs(fill = "Gender according to RaB/ARaB")
  #print_gglist_to_ppt(plist=plist, k=k, c="none", y="", ppt)
  
  png <- paste0(dir, paste0('png/gender_interpretations_qid_',k,'.png'))
  ggsave(png, plot=plist[[1]], width=9, height=9)
  doc <- read_pptx(ppt)
  print_png_to_ppt(png, k="all ranks", c="ratio by ds and bm25", y="", ppt)
}


