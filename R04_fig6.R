#load file"
r <- read.csv(paste0(dir, "r_qe.csv"), stringsAsFactors=TRUE)
r <- r %>% select(-c("X","query_no","qid_text","query_text"))
labvar <- c("qid","method","dataset","class_0", "class_1", "class_2")
statsvar <- setdiff(names(r), labvar)
r[statsvar] <- sapply(r[statsvar], as.numeric)

class_0_levels <- c("none", "global_analysis", "local_analysis")
class_1_levels <- c("none", "stemming", "semantic", "web", "prf")
class_2_levels <- c("none", "krovetz","lovins","paicehusk","porter","porter2","sstemmer","trunc4","trunc5","tagmee","thesaurus","wordnet","conceptnet","sensedisambiguation", "anchor","glove","wiki","word2vec", "adaponfields","bertqe","concept_cluster","doc_cluster","onfields","relevancefeedback","rm3","termluster")
r$class_0 <- factor(r$class_0, levels=class_0_levels)
r$class_1 <- factor(r$class_1, levels=class_1_levels)
r$class_2 <- factor(r$class_2, levels=class_2_levels)
#ck <- pivot(data=r, compute=tabulate, variable=c(qid), by=c(class_0,class_1,class_2,method))

#load file to add gender
ref <- read.csv("~/2021/reference_tables/ref_queries.csv", stringsAsFactors=TRUE)
r <- r %>% 
  filter(dataset!="msmarco") %>%
  left_join(ref[,c("qid","method","dataset","gender")])
labvar <- c(labvar,"gender")
statsvar <- setdiff(names(r), labvar)

yvar <- c(names(r)[grep('tf|bool', names(r))])
yvar <- yvar[grep('bias', yvar)]

#chunk off dataset by ranks
at_ranklist <- c(5,10,20,30,50) 
rank_df_list <- list()
for (i in 1:length(at_ranklist)){
  k <- at_ranklist[i]
  tmp <- r[, c(labvar, names(r)[grep(paste0("_",k,"$"), names(r))])]
  names(tmp) <-  sub(paste0("_",k), "", names(tmp))
  rank_df_list[[i]] <- tmp
}

#categorizations <- c("class_0", "class_1", "class_2")
for (i in 1:length(at_ranklist)){

  k <- at_ranklist[i]
  df <- rank_df_list[[i]]
  
  yvar <- names(df)[grep("tf|bool", names(df))]
  pre <- df[which(df$method=="none" & df$gender=="neutral"), c(labvar, yvar)] %>% 
    pivot_longer(!!yvar, names_to="yvar", values_to="pre") 
  df <- df %>%
    filter(method!="none") %>%
    select(!!labvar, !!yvar) %>%
    pivot_longer(!!yvar, names_to="yvar", values_to="post") %>%
    left_join(pre[,c("qid","dataset","yvar","pre")]) %>% 
    pivot_longer(c(pre, post), names_to="cond", values_to="value") %>%
    pivot_wider(names_from="yvar", values_from="value")  %>% 
    pivot_longer(c("class_1"), names_to="class_lvl", values_to="class") %>%
    pivot_longer(all_of(yvar), names_to="bias", values_to="values")
  
  df[which(df$cond=="pre"), "cond"] <- "original query" 
  df[which(df$cond=="post"), "cond"] <- "reformulation" 
  
  yvar_levels <- c("bool_RaB_bias", "bool_ARaB_bias", "tf_RaB_bias", "tf_ARaB_bias", 
                   "bool_RaB_male", "bool_ARaB_male", "tf_RaB_male", "tf_ARaB_male", 
                   "bool_RaB_feml", "bool_ARaB_feml", "tf_RaB_feml", "tf_ARaB_feml") 
  df$bias <- factor(df$bias, levels=yvar_levels)
  
  tests_at_rank <- data.frame()
  testdf <- df %>%
    group_by(class_lvl, class, bias) 
  stat.test <- testdf %>%
    wilcox_test(values ~ cond, paired=TRUE) %>%
    adjust_pvalue(method="bonferroni") %>%
    add_significance("p.adj")
  tests_at_rank <- bind_rows(tests_at_rank, stat.test) 
    
  names(tests_at_rank)[9:12] <- paste(names(tests_at_rank)[9:12], k, sep="_")
  if (i==1){
    pairwisetests <- tests_at_rank
  } else {
    pairwisetests <- full_join(pairwisetests, tests_at_rank)
  }
  plist <- list() 
  stat.test <-  stat.test %>%
    add_xy_position(x=c("class"), fun="mean_sd", scales="free")
  plist <- list() 
  plist[[1]] <- ggboxplot(testdf, x="class", y="values", color="cond",
                   palette="jco", add="mean_sd", scales="free",
                   facet.by=c("bias")) +
    stat_pvalue_manual(stat.test, x="class", hide.ns=TRUE) +
    scale_y_continuous(expand=expansion(mult=c(0.05, 0.15))) +
    theme(axis.text.x=element_text(angle=45, hjust=1)) + 
    ggtitle("") +  
    xlab("") + 
    ylab("Bias metric") +
    labs(color = "")
  #print_gglist_to_ppt(plist=plist, k=k, c="neutral query bias", y="", ppt)
  png <- paste0(dir, paste0('png/ttest_',k,'.png'))
  ggsave(png, plot=plist[[1]], width=9, height=7)
  
  # for (e in c("bias","male","feml")){
  #   plt <- df %>%
  #     filter(grepl(e, bias)) %>%
  #     pivot_wider(names_from="cond", values_from="values") %>%
  #     mutate(difference=post - pre) %>%
  #     group_by(class) %>%
  #     mutate(mean=mean(difference, na.rm =TRUE)) %>% ungroup()
  #   png <- paste0(dir, "png/hist_paireddiff_", k, ".png")
  #   p1 <- gghistogram(plt, x="difference",
  #               add="mean", rug=TRUE,
  #               position=position_dodge(),
  #               color="class", fill="class",
  #               palette=c("jco")) +
  #     scale_y_log10(breaks=scales::log_breaks()) +
  #     facet_wrap(class_lvl ~bias , scales="free_x", ncol=4) +
  #     labs(title="",x="difference with QE", y="count") +
  #     theme(axis.text=element_text(size=rel(0.6)))
  #   ggsave(png, plot=p1, width=6, height=4)
  #   print_png_to_ppt(png, k=k, c="neutral query bias", y="", ppt)
  #   png <- paste0(dir, "png/qq_paireddiff_", k, ".png")
  #   p1 <- ggqqplot(plt, x="difference", 
  #                  color="class", fill="class",
  #                  palette=c("jco"), size=0.1) +
  #     facet_wrap(class_lvl ~bias , scales="free", ncol=4) +
  #     theme(axis.text=element_text(size=rel(0.6)))
  #   ggsave(png, plot=p1, width=6, height=4)
  #   print_png_to_ppt(png, k=k, c="neutral query bias", y="", ppt)
  #}
}

write.csv(pairwisetests, paste0(dir, "/model/pairwisetests.csv"))
