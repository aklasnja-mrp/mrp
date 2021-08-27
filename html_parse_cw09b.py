from bs4 import BeautifulSoup
import html5lib
import os

os.chdir("/home/anja/")

ds = 'cw09b'

empty_cnt = 0

indir = os.path.join('data',ds,'docs','raw_qrels')
outdir = os.path.join('data',ds,'docs','parsed_qrels')
           
os.mkdir(outdir)

for i, docid in enumerate(os.listdir(indir)):
    if len(docid) > 0:
        try:
            with open(os.path.join(indir, docid), 'r') as f:
                html = f.read()

            soup = BeautifulSoup(html, 'html.parser')

            with open(os.path.join(outdir, docid), "w") as f:
            
                for x in soup.find_all('title'):
                    f.write(x.text) 
                    
                for x in soup.find_all('body'):
                    f.write(x.text) 
                    
            soup = None
            
        except:
            empty_cnt += 1
            
            failed = ds + '_failed_parsing_qrels.txt'       
                 
            with open(os.path.join('data',ds,'notes',failed), 'a') as out:
            
                out.write(f'{empty_cnt} {docid}\n')
                continue


empty_cnt = 0

indir = os.path.join('data',ds,'docs','raw_retrieved')
outdir = os.path.join('data',ds,'docs','parsed_retrieved')
           
os.mkdir(outdir)

for i, docid in enumerate(os.listdir(indir)):
    if len(docid) > 0:
        try:
            with open(os.path.join(indir, docid), 'r') as f:
                html = f.read()

            soup = BeautifulSoup(html, 'html.parser')

            with open(os.path.join(outdir, docid), "w") as f:
            
                for x in soup.find_all('title'):
                    f.write(x.text) 
                    
                for x in soup.find_all('body'):
                    f.write(x.text) 
                    
            soup = None
            
        except:
            empty_cnt += 1
            
            failed = ds + '_failed_parsing_retrieved.txt'            
            with open(os.path.join('data',ds,'notes',failed), 'a') as out:
            
                out.write(f'{empty_cnt} {docid}\n')
                continue
