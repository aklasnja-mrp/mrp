library(psych)
library(lessR)

# tables
dir <- "~/2021/"
ref <- read.csv(paste0(dir, "reference_tables/ref_queries.csv"), stringsAsFactors=TRUE)

ref$gender <- factor(ref$gender, levels = c("neutral", "male", "female"))
ref$class_0 <- factor(ref$class_0, levels = c("none", "global_analysis", "local_analysis"))
ref$class_1 <- factor(ref$class_1, levels = c("none", "stemming", "semantic", "web", "prf"))
ref$class_2 <- factor(ref$class_2, levels = c("none", "krovetz","lovins","paicehusk","porter","porter2","sstemmer","trunc4","trunc5","tagmee","thesaurus","wordnet","conceptnet","sensedisambiguation", "anchor","glove","wiki","word2vec", "adaponfields","bertqe","concept_cluster","doc_cluster","onfields","relevancefeedback","rm3","termluster"))

# get counts
ck <- pivot(data=ref, compute=tabulate, variable=c(qid), by=c(class_0, class_1,class_2, dataset)) %>%
  filter(n > 0) %>% pivot_wider(names_from=dataset, values_from=n) 
ck <- pivot(data=ref, compute=tabulate, variable=c(qid), by=c(class_1, dataset, gender)) %>%
  filter(n > 0) %>% pivot_wider(names_from=class_1, values_from=n) 

ref0 <- ref %>% 
  filter(method=="none") %>%
  select(qid,dataset,gender) %>%
  rename(gender_0=gender)

ref1 <- ref %>%
  left_join(ref0) %>%
  mutate(conversion= paste(gender_0, gender, sep="_"))

ref1[ref1$conversion %in% c("neutral_neutral","male_male","female_female"),"conversion"] <- "unchanged"

ck <- pivot(data=ref1, compute=tabulate, variable=c(id), by=c(class_1, conversion)) %>%
  filter(n > 0) %>% pivot_wider(names_from=class_1, values_from=n) 

#load file
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

yvar <- c(names(r)[grep('tf|bool', names(r))])
yvar <- yvar[grep('bias', yvar)]

comment="
# get describeBy
plt <- r %>%
  select(dataset, gender, yvar) %>%
  pivot_longer(yvar, names_to='var', values_to='x') %>% 
  separate(var, into=c('variable', 'rank'), '_(?=[^_]+$)') %>%
  mutate(rank=factor(rank, levels=str_sort(unique(rank), numeric=TRUE))) %>% 
  group_by(variable) %>%
  mutate(row = row_number()) %>%
  pivot_wider(names_from=variable, values_from=x)%>%
  select(-row, -gender)
ck <- describeBy(plt ~ dataset,mat=TRUE, digits=2)
"
comment="
# get counts
ck <- pivot(data=r, compute=tabulate, variable=c(qid), by=c(class_0, class_1, class_2, dataset)) %>%
  filter(n > 0) %>%
  pivot_wider(names_from=dataset, values_from=n) 
"
comment="
subset to reduce size and even out variance in lm
set.seed(1)
c <- 'class_1'
no_rows <- r %>% 
  group_by(!!sym(c)) %>%
  summarise(no_rows=length(!!sym(c)))
n <- min(no_rows$no_rows)
r <- r %>%
  group_by(!!sym(c)) %>%
  slice_sample(n=n) %>% ungroup() %>%
  pivot_longer(!!statsvar, names_to='var', values_to='x') %>% 
  separate(var, into=c('variable', 'rank'), '_(?=[^_]+$)') %>%
  mutate(rank=factor(rank, levels=str_sort(unique(rank), numeric=TRUE))) %>%
  pivot_wider(c(labvar, 'rank'), names_from='variable', values_from='x') 
"







