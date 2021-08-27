import os
import pandas as pd
import re

import numpy as np
import collections
import pickle

os.chdir("/home/anja/")

#ds = 'gov2'
#f_topics = 'reference_tables/topics.gov2.701-850.bm25.map.dataset.csv'
#d_retrieved =  'data/gov2/docs/parsed_retrieved'
#d_qrels =  'data/gov2/docs/parsed_qrels'

#ds = 'cw09b'
#f_topics = 'reference_tables/topics.clueweb09b.1-200.bm25.map.dataset.csv'
#d_retrieved =  'data/cw09b/docs/parsed_retrieved'
#d_qrels =  'data/cw09b/docs/parsed_qrels'

#ds = 'cw12b13'
#f_topics = 'reference_tables/topics.clueweb12b13.201-300.bm25.map.dataset.csv'
#d_retrieved =  'data/cw12b13/docs/parsed_retrieved'
#d_qrels =  'data/cw12b13/docs/parsed_qrels'

#ds = 'robust04'
#f_topics = 'reference_tables/topics.robust04.bm25.map.dataset.txt'
#d_retrieved =  'data/robust04/docs/raw_retrieved'
#d_qrels =  'data/robust04/docs/raw_qrels'

ds = 'msmarco'
f_topics = 'reference_tables/-----'
d_retrieved =  'data/msmarco/raw_retrieved_out.tsv'
d_qrels =  'data/msmarco/raw_qrels.tsv'


#bias calc
save_pkl = {'tc': os.path.join('data',ds,'bias',ds+'_tc.pkl'), \
            'bool': os.path.join('data',ds,'bias',ds+'_bool.pkl'), \
            'tf': os.path.join('data',ds,'bias',ds+'_tf.pkl')}
save_qpkl = {'tc': os.path.join('data',ds,'bias',ds+'_qrels_tc.pkl'), \
            'bool': os.path.join('data',ds,'bias',ds+'_qrels_bool.pkl'), \
            'tf': os.path.join('data',ds,'bias',ds+'_qrels_tf.pkl')}
wordlist = 'reference_tables/wordlist_genderspecific.txt'

#2.1.1
#bias calc 

#get wordlist
genderwords_feml = []
genderwords_male = []
for l in open(wordlist):
    vals = l.strip().split(',')
    if vals[1]=='f':
        genderwords_feml.append(vals[0])
    elif vals[1]=='m':
        genderwords_male.append(vals[0])
genderwords_feml = set(genderwords_feml)
genderwords_male = set(genderwords_male)

#count gender words
def get_tokens(text):
    return text.lower().split(" ")

def get_bias(tokens):
    text_cnt = collections.Counter(tokens)
    cnt_feml = 0
    cnt_male = 0
    cnt_logfeml = 0
    cnt_logmale = 0
    for word in text_cnt:
        if word in genderwords_feml:
            cnt_feml += text_cnt[word]
            cnt_logfeml += np.log(text_cnt[word] + 1)
        elif word in genderwords_male:
            cnt_male += text_cnt[word]
            cnt_logmale += np.log(text_cnt[word] + 1)
    text_len = np.sum(list(text_cnt.values()))
    
    bias_tc = (float(cnt_feml - cnt_male), float(cnt_feml), float(cnt_male))
    bias_tf = (np.log(cnt_feml + 1) - np.log(cnt_male + 1), np.log(cnt_feml + 1), np.log(cnt_male + 1))
    bias_bool = (np.sign(cnt_feml) - np.sign(cnt_male), np.sign(cnt_feml), np.sign(cnt_male)) 
    #3 val each
    return bias_tc, bias_tf, bias_bool 

def calc_doc_bias(collection, save_pkl):   
    docs_bias = {'tc':{}, 'tf':{}, 'bool':{}}
    empty_cnt = 0

    if ds == 'msmarco':
        with open(collection) as fr:    
            for i, line in enumerate(fr):
                vals = line.strip().split('\t')
                docid = int(vals[0])
                if len(vals) == 2:
                    text = vals[1]
                else:
                    text = ""
                    empty_cnt += 1
                    print(docid)
                            
                res = get_bias(get_tokens(text))
                docs_bias['tc'][docid] = res[0]
                docs_bias['tf'][docid] = res[1]
                docs_bias['bool'][docid] = res[2]    
        
    else:
        for i, docid in enumerate(os.listdir(collection)):
            try:
                with open(os.path.join(collection, docid), 'r') as f:       
                    text = f.read().replace('\n', ' ')           

                    res = get_bias(get_tokens(text))
                    docs_bias['tc'][docid.replace('.txt','')] = res[0]
                    docs_bias['tf'][docid.replace('.txt','')] = res[1]
                    docs_bias['bool'][docid.replace('.txt','')] = res[2]   
            except:
                text = ""
                empty_cnt += 1
                print(empty_cnt, docid)

            if i % 1000000 == 0:
                print (i)

    # save bias values of documents
    for method in docs_bias:
        with open(save_pkl[method], 'wb') as fw:
            pickle.dump(docs_bias[method], fw)

#saved document bias values
calc_doc_bias(d_retrieved, save_pkl)

#calc_doc_bias(d_qrels, save_qpkl)