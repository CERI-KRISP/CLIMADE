# This code uses Biopython to retrieve lists of articles from pubmed
# you need to install Biopython first.

# If you use Anaconda:
# conda install biopython

# If you use pip/venv:
# pip install biopython

# Full discussion:
# https://marcobonzanini.wordpress.com/2015/01/12/searching-pubmed-with-python/

from Bio import Entrez
import pandas as pd
from Bio import SeqIO
import re

def search(query):
    Entrez.email = 'houriiyah.tegally@gmail.com'
    handle = Entrez.esearch(db='pubmed', 
                            sort='relevance', 
                            retmax='10000',
                            retmode='xml', 
                            term=query)
    results = Entrez.read(handle)
    return results


accession_file=pd.read_csv('yellowfever_Africa_all_genbank_accessions.txt', sep=' ', header=None, names=["IDs"])

nuc_uids=accession_file['IDs'].tolist()    
#print(nuc_uids)    

Entrez.email = 'houriiyah.tegally@gmail.com'
    
records = []
for nuc_uid in nuc_uids:
	
	handle = Entrez.efetch(db="nucleotide", rettype="gb", retmode="text", id=nuc_uid)
	gb_record=handle.readlines()
	#records.append(SeqIO.read(handle, 'genbank'))
	#print(gb_record)
	
	authors_found=0
	more_authors_needed=0
	last_char=''
	for line in gb_record:
		if authors_found==1 and more_authors_needed==1:
			authors2=line
			#print(authors2)
			authors=authors.strip()+' '+authors2.strip()
			last_char=line.rstrip()[-1]
			if last_char==',' or last_char=='d': #and
				more_authors_needed=1
				authors_found=1
			else:
				authors_found=0
				more_authors_needed=0
			#print(authors)
		if re.search("AUTHORS", line):
			#print(line)
			authors=line.rsplit('  ')[2]
			#print(authors)
			authors_found=1
			last_char=line.rstrip()[-1]
			#print(last_char)
			if last_char==',' or last_char=='d': #and
				more_authors_needed=1
			else:
				more_authors_needed=0
		if re.search("PUBMED", line):
			pubmed=line.rsplit('  ')[2]
			#print(pubmed)
		if re.search("TITLE", line):
			paper=line.rsplit('  ')[2]
			#print(paper)
			
	print(nuc_uid+"\t"+authors)
			
		


    

#print(records, "\n")
#print("DONE")