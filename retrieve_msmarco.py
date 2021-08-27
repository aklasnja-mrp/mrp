import os
from pyserini import collection, index
from pyserini.search import SimpleSearcher
from pyserini.search import querybuilder
import pandas as pd
import re
import nltk
from nltk.corpus import stopwords
stop_words = set(stopwords.words('english'))
l = ['.', ',', '!', '?', ';', 'a', 'an', '(', ')', "'", '_', '-', '<', '>', 'if', '/', '[', ']', '&nbsp;']
stop_words.update(l)


os.chdir("/home/anja/")

ds = 'msmarco'

qrels = 'reque_workspace/ReQue/ds/msmarco/qrels.msmarco-passage.dev-subset.txt'

searcher = SimpleSearcher.from_prebuilt_index('msmarco-passage')
#searcher = SimpleSearcher('reque_workspace/ReQue/ds/msmarco/index-msmarco-passage')

d = pd.read_csv('reference_tables/topics.msmarco.bm25.map.dataset.csv')


#original layout - separate queries., method names

d = d.set_index('qid')
d.rename(columns={'abstractqueryexpansion': 'query.0'}, inplace=True)

q_cols = d[[col for col in d.columns if 'query.' in col]]
m_cols = d[[col for col in d.columns if 'method.' in col]]

qids = q_cols.index.tolist()

ref_models = pd.read_csv('reference_tables/ref_models.csv')

#--------------------------------------------------------------------
#make queries reference

ref = pd.DataFrame(columns = ['qid','qid_text','query_ no','query_text','method'])

for i, qid in enumerate(qids):
    no_queries = q_cols.loc[qid,].count()
    for j in range(0, no_queries): 
        if j > 0:
            ref = ref.append({'qid':qid, \
                              'qid_text':q_cols.loc[qid, 'query.0'], \
                              'query_ no':'query.'+str(j), \
                              'query_text':q_cols.loc[qid, 'query.'+str(j)], \
                              'method':m_cols.loc[qid, 'method.'+str(j)]}, ignore_index=True)
        else:
            ref = ref.append({'qid':qid, \
                              'qid_text':q_cols.loc[qid, 'query.0'], \
                              'query_ no':'query.'+str(j), \
                              'query_text':q_cols.loc[qid, 'query.'+str(j)], \
                              'method':'none'}, ignore_index=True)
ref = ref.set_index('method').join(ref_models.set_index('method'), how='left').reset_index()

file = ds + '_ref.csv'
ref.to_csv(os.path.join('data',ds,file), index=True, header=True, sep=',')


#--------------------------------------------------------------------
#retrieve docs

for i, qid in enumerate(qids):
    print(qid) 
    qid_text = q_cols.loc[qid, 'query.0']
    no_queries = q_cols.loc[qid,].count()
    
    file = ds + '_q_cnt.txt'
    with open(os.path.join('data',ds,'notes',file), 'a') as out:
        out.write(f'{qid} ({no_queries} QE) {qid_text}\n')    
    
    for j in range(0, no_queries): 
        query_no = 'query.'+str(j)
        query_text = q_cols.loc[qid, query_no]
        if j > 0:
            method = m_cols.loc[qid, 'method.'+str(j)]
        else:
            method = 'none'
            
        if re.search(r'{.*}', query_text):
            #build weighted query with terms if query expansion method produced weights
            query_terms = re.findall( r"'(.*?)':", query_text)
            query_weights = re.findall( r": (.*?)[,|}]", query_text)
            try:
                should = querybuilder.JBooleanClauseOccur['should'].value
                boolean_query_builder = querybuilder.get_boolean_query_builder()
                for l in range(0, len(query_terms)):
                    term = querybuilder.get_term_query(query_terms[l])
                    boost = querybuilder.get_boost_query(term, float(query_weights[l]))
                    boolean_query_builder.add(boost, should)      
                query = boolean_query_builder.build()
            except:
                failed = ds + '_failed_queries.txt'
                with open(os.path.join('data',ds,'notes',failed), 'a') as out:
                    out.write(f'{qid} {query_no} {method}\n{query_terms}\n{query_weights}\n\n')
                    print(f'failed {qid} {query_no}')
                    continue
        else:
            query = query_text
            
        hits = searcher.search(query, 1000)
        
        #print hits:
        for k in range(0, min(100, len(hits))):
            rank = k
            docid = hits[k].docid
            raw = hits[k].raw
                              
            file = ds + '_bm25'
            with open(os.path.join('data',ds,file), 'a') as out:
                out.write(f'{qid} {query_no} {method} {rank} {docid}\n')
                
#--------------------------------------------------------------------
#re-retrieve failed queries without stop words

try:

    failed = ds + '_failed_queries.txt'
    failed = os.path.join('data',ds,'notes',failed)
                
    qid_f = []
    query_no_f = []
    method_f = []

    for i, l in enumerate(open(failed)):
        if i == 0 or (i != 0 and i % 4 == 0):
            vals = l.strip().split(' ')
            qid_f.append(int(vals[0]))
            query_no_f.append(vals[1])
            method_f.append(vals[2])

    for i, qid in enumerate(qid_f):
        print(qid) 
        query_no = query_no_f[i]
        query_text = q_cols.loc[qid, query_no]
        method = method_f[i]
        if re.search(r'{.*}', query_text):
        
            #build weighted query with terms if query expansion method produced weights
            query_terms = re.findall( r"'(.*?)':", query_text)
            query_weights = re.findall( r": (.*?)[,|}]", query_text)
            #but first remove problem words
            remove_idx = [] 
            for l in range(0, len(query_terms)):
                if query_terms[l] in stop_words:
                    remove_idx.append(l)
            for index in sorted(remove_idx, reverse=True):
                del query_terms[index]
                del query_weights[index]
 
            try:
                should = querybuilder.JBooleanClauseOccur['should'].value
                boolean_query_builder = querybuilder.get_boolean_query_builder()
                for l in range(0, len(query_terms)):
                    term = querybuilder.get_term_query(query_terms[l])
                    boost = querybuilder.get_boost_query(term, float(query_weights[l]))
                    boolean_query_builder.add(boost, should)      
                query = boolean_query_builder.build()
            except:
                    failed = ds + '_failed_queries_2.txt'
                    with open(os.path.join('data',ds,'notes',failed), 'a') as out:
                        out.write(f'{qid} {query_no} {method}\n{query_terms}\n{query_weights}\n\n')
                        print(f'failed {qid} {query_no}')
                        continue
                
        hits = searcher.search(query, 1000)
    
        #print hits:
        for k in range(0, min(100, len(hits))):
            rank = k
            docid = hits[k].docid
            raw = hits[k].raw
                              
            file = ds + '_bm25'
            with open(os.path.join('data',ds,file), 'a') as out:
                out.write(f'{qid} {query_no} {method} {rank} {docid}\n')

except:
    print('_failed_queries.txt not printed')