library(quantreg) # fit quantile regression 
library(rms) # cv of quantile regression goodness-of-fit

#load file
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

#remove other variables
exclude <- paste(c('^male','female','affect', 'social', 'percept','cogproc','bio','drives','relativ','negemo'), collapse="|")
exclude <- names(r)[grep(exclude, names(r))]
exclude <- c(exclude, names(r)[grep("tc", names(r))]) 
#exclude <- c(exclude, c("see", "hear", "feel", "motion", "space", "time"))
r <- r %>% 
  select(!exclude) %>%
  filter(dataset!="msmarco")
statsvar <- setdiff(names(r), labvar)

#chunk off dataset by ranks
at_ranklist <- c(5,10,20,30,50) 
rank_df_list <- list()
for (i in 1:length(at_ranklist)){
  k <- at_ranklist[i]
  tmp <- r[, c(labvar, names(r)[grep(paste0("_",k,"$"), names(r))])]
  names(tmp) <-  sub(paste0("_",k), "", names(tmp))
  rank_df_list[[i]] <- tmp
}

# regression  > qr_bias_coefficients_30.csv 
###########################################

c <- c("class_1")
k <- c(30)
df <- rank_df_list[[4]]
yvar <- names(df)[grep("tf|bool", names(df))]
statsvar <- setdiff(names(df), c(labvar, yvar))
#yvar <- yvar[!grepl("bias", yvar)]
#yvar <- c("bool_RaB_male", "bool_ARaB_male", "tf_RaB_male", "tf_ARaB_male", 
#          "bool_RaB_feml", "bool_ARaB_feml", "tf_RaB_feml", "tf_ARaB_feml") 
yvar <- c("bool_RaB_bias", "bool_ARaB_bias", "tf_RaB_bias", "tf_ARaB_bias")
IV <- paste(c(statsvar), collapse="+")
df0 <- df

counter <- 0
for (y in yvar){
  df <- df0[which(df0$method!="none"), c(labvar, statsvar, y)]
  df$gender <- factor(df$gender, levels=c("neutral","male","female"))
  df$qid <- paste(df$dataset, df$qid, df$method, sep="_")
  #yeojohnson_obj <- yeojohnson(df[,y])
  #df[,y] <- yeojohnson_obj$x.t

  #full model - quantile regression model 
  f <- as.formula(paste(y, '~', IV, '+ gender'))
  at_taulist <- c(0.1,0.5,0.9)
  
  q <- rq(f, tau=at_taulist, df)
  
  for (i in 1:length(at_taulist)){
    xtau <- at_taulist[i]
    m <- Rq(f, tau=xtau, df, y=TRUE, x=TRUE)
    validate(m, method="boot", B=1000, rule="aic")
  
  out <- summary(q)
  png <- paste0(dir, 'png/qr_2_', y, '.png')
  png(png, width=11, height=11, units="in", res=300)
  plot(out, level=0.95)
  dev.off()
  for (i in 1:length(at_taulist)){
    xtau <- at_taulist[i]
    print(i)
    print(xtau)
    y_tau <- as.data.frame(out[[i]][[3]]) #coefficients
    ref <- paste(y, c("Estimate","Std. Error","t value","Pr(>|t|)"), xtau, sep="---")
    names(y_tau) <- ref
    y_tau$key <- rownames(y_tau)
    rownames(y_tau) <- c()
    y_tau <- y_tau[,c("key", ref)]
    print(y_tau[1,])
    if (counter==0 & i==1){
      qr <- y_tau
    } else {
      qr <- merge(qr, y_tau, by.x="key", by.y="key", all=TRUE)
    }
  }    
  counter <- counter + 1
}
names(qr)[1] <- "coefficient"
write_csv(qr, paste0(dir, "/model/qr_2_coefficients_", k, ".csv"))

#boot validate 

counter <- 0
for (y in yvar){
  df <- df0[which(df0$method!="none"), c(labvar, statsvar, y)]
  df$gender <- factor(df$gender, levels=c("neutral","male","female"))
  df$qid <- paste(df$dataset, df$qid, df$method, sep="_")
  
  #full model - quantile regression model 
  f <- as.formula(paste(y, '~', IV, '+ gender'))
  at_taulist <- c(0.1,0.5,0.9)

  for (i in 1:length(at_taulist)){
    xtau <- at_taulist[i]
    m <- Rq(f, tau=xtau, df, y=TRUE, x=TRUE)
    ck <- validate(m, method="boot", B=1000, rule="aic")
    vali_tau <- cbind(c('MAD','rho','g-index','Intercept','Slope'), as.data.frame(ck[21:25]))
    names(vali_tau) <- c('key',paste0(y,'---',xtau))
    if (counter==0 & i==1){
      validation <- vali_tau
    } else {
      validation <- merge(validation, vali_tau, by.x="key", by.y="key", all=TRUE)
    }
    counter <- counter + 1
  }
}

comment="
plt <- qr %>%
  select(coefficient, names(qr)[grep('Estimate|Pr', names(qr))]) %>%
  pivot_longer(-coefficient, names_to='var', values_to='x') %>% 
  separate(var, into=c('variable', 'tau'), '---(?=[^---]+$)') %>%
  separate(variable, into=c('var', 'type'), '---(?=[^---]+$)') %>%
  pivot_wider(names_from='type', values_from='x') %>%
  mutate(color=case_when(`Pr(>|t|)`<0.005~ 'goldenrod1',TRUE ~ 'gray70'), 
         P=round(`Pr(>|t|)`, 3),
         facet=paste0(var, '\n', tau))

for (p in c('bias')){
  for (t in c('bool','tf')){
    png <- paste0(dir, 'png/qr_2_coeff_', p, '_', t, '.png')
    p1 <- plt %>% 
      filter(grepl(p, var)) %>%
      filter(grepl(t, var)) %>%
      ggplot(aes(x=coefficient, y=Estimate, fill=color)) +
      geom_col() +
      geom_text(aes(label=P), size=3) +
      facet_grid(~facet) +
      #scale_fill_jco() +
      coord_flip() + 
      scale_fill_identity(guide='none') +
      theme_minimal()
    ggsave(png, plot=p1, width=7, height=5)
    print_png_to_ppt(png, k=k, c=paste(at_taulist, collapse=', ') , y='quantile regression', ppt)
  }
}
"

comment="
#ols model
  f <- as.formula(paste(y, '~', IV, '+ gender'))
  m <- lm(f,data=df)
  png <- paste0(dir, 'png/coeff_lm_', y, '.png')
  p1 <- ggcoef(tidy(m, conf.int=TRUE), sort='ascending')
  ggsave(png, plot=p1, width=4, height=4)
  print_png_to_ppt(png, k=k, c='', y=y, ppt)
  #summary(gvlma(m))
  #plot(m, id.n=5, labels.id=df$qid)
"


