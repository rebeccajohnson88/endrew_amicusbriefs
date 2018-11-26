
################## Imports

import shogun
from shogun import StringCharFeatures, RAWBYTE
from shogun import BinaryLabels
from shogun import SubsequenceStringKernel
from shogun import LibSVM
import spacy
get_ipython().system('python -m spacy download en')
import io
import os
import glob
import numpy as np  
import pdfminer
from pdfminer.pdfinterp import PDFResourceManager, PDFPageInterpreter
from pdfminer.converter import TextConverter
from pdfminer.layout import LAParams
from pdfminer.pdfpage import PDFPage
from sklearn.feature_extraction.text import CountVectorizer
import re
import string
import difflib
from difflib import SequenceMatcher
from heapq import nlargest as _nlargest
import pandas as pd
from IPython.core.interactiveshell import InteractiveShell
InteractiveShell.ast_node_interactivity = "all"

## string cleaning
from nltk.corpus import stopwords
from nltk.stem.porter import PorterStemmer
from nltk.tokenize import wordpunct_tokenize
stop_words = set(stopwords.words('english'))
## add punctuation and some application-specific words
## to stopword list
stop_words.update(['.', ',', '"', "'", '?', '!', ':', ';', 
                   '(', ')', '[', ']', '{', '}', 'iep','idea']) # 
porter = PorterStemmer()

## sentiment analysis
from nltk.sentiment.vader import SentimentIntensityAnalyzer
import nltk
nltk.download('vader_lexicon')

## link processing
import urllib
from bs4 import BeautifulSoup



# In[2]:


print("Loaded imports successfully")


################## Functions


## function (from stackoverflow)
## that
## takes in:
## @path: pathname
## returns:
## string
def convert_pdf_to_txt(path):
    rsrcmgr = PDFResourceManager()
    retstr = io.StringIO()
    codec = 'utf-8'
    laparams = LAParams()
    device = TextConverter(rsrcmgr, retstr, codec=codec, laparams=laparams)
    fp = open(path, 'rb')
    interpreter = PDFPageInterpreter(rsrcmgr, device)
    password = ""
    maxpages = 0
    caching = True
    pagenos = set()

    for page in PDFPage.get_pages(fp, pagenos, maxpages=maxpages,
                                  password=password,
                                  caching=caching,
                                  check_extractable=True):
        interpreter.process_page(page)

    text = retstr.getvalue()

    fp.close()
    device.close()
    retstr.close()
    return text

def lower_removepunct(string_toclean):
    string_lower = string_toclean.lower()
    string_lower_char = "".join(x for x in string_lower if x not in string.punctuation) 
    return(string_lower_char)

occurrences = lambda s, lst: (i for i,e in enumerate(lst) if s in e)

def get_close_matches_indexes(word, possibilities, n=3, cutoff=0.6):
    """Use SequenceMatcher to return a list of the indexes of the best 
    "good enough" matches. word is a sequence for which close matches 
    are desired (typically a string).
    possibilities is a list of sequences against which to match word
    (typically a list of strings).
    Optional arg n (default 3) is the maximum number of close matches to
    return.  n must be > 0.
    Optional arg cutoff (default 0.6) is a float in [0, 1].  Possibilities
    that don't score at least that similar to word are ignored.
    """

    if not n >  0:
        raise ValueError("n must be > 0: %r" % (n,))
    if not 0.0 <= cutoff <= 1.0:
        raise ValueError("cutoff must be in [0.0, 1.0]: %r" % (cutoff,))
    result = []
    s = SequenceMatcher()
    s.set_seq2(word)
    for idx, x in enumerate(possibilities):
        s.set_seq1(x)
        if s.real_quick_ratio() >= cutoff and s.quick_ratio() >= cutoff and s.ratio() >= cutoff:
            result.append((s.ratio(), idx))

    # Move the best scorers to head of list
    result = _nlargest(n, result)

    # Strip scores for the best n matches
    return [x for score, x in result]

## function for dtm representation
def create_nonmasked_dtm(list_of_strings, metadata):
    vectorizer = CountVectorizer(lowercase = True)
    dtm_sparse = vectorizer.fit_transform(list_of_strings)
    dtm_dense_named = pd.DataFrame(dtm_sparse.todense(), columns=vectorizer.get_feature_names())
    dtm_dense_named_withid = pd.concat([metadata, dtm_dense_named], axis = 1)
    return(dtm_dense_named_withid)

def create_sentiment_dataframe(list_ofsentiment_dicts, metadata):
    sentiment_df = pd.DataFrame.from_records(list_ofsentiment_dicts).fillna(0)
    sentiment_df.columns = ['sentimentsummary_'+ str(i) for i in sentiment_df.columns]
    sentiment_withid = pd.concat([metadata, sentiment_df], axis = 1)
    return(sentiment_withid)

def create_distance_matrix(full_essays, ids):
    string_features = StringCharFeatures(full_essays, RAWBYTE)
    sk = SubsequenceStringKernel(string_features, string_features, 3, 0.5)
    sk_matrix = sk.get_kernel_matrix()
    sk_df = pd.DataFrame(sk_matrix)
    sk_df.columns = ['id_'+ str(i) for i in ids]
    return(sk_df)
    

def return_links_todocs(url, prefix = None):
    
    ## call link and open
    request = urllib.request.urlopen(url)
    opened_page = request.read()
    
    ## parse page and find all links
    parsed_page = BeautifulSoup(opened_page, "lxml")
    links_page = parsed_page.findAll('a')
    
    ## iterate over links and return all
    all_links = [a['href'] for a in links_page]
    
    ## if there's a prefix to subset by, add that
    if prefix is not None:
        prefix_links = [link for link in all_links if link.startswith(prefix)]
        
        return(prefix_links)
    
    else:
        
        return(all_links)
    
## for each case, create dataframe
def get_metadata_andtext(parsed_page):
    
    case_metadata = parsed_page.find_all('meta', attrs = {'name': True})
    what_to_extract = ['docket', 'decided', 'caption', 'judge', 'summary']
    case_metadata_content = [tag['content'] for tag in case_metadata if tag['name'] in what_to_extract]
    case_dict = dict(zip(what_to_extract, case_metadata_content))
    case_text = parsed_page.find('p').getText()
    case_dict['full_text'] = case_text
    case_dict_df = pd.DataFrame(case_dict, index = [0])
    return(case_dict_df)
    
def split_caption(data, nameof_captioncol):
    
    caption_split = data[nameof_captioncol].str.split('v.', 1).tolist()
    plaintiff = [item[0] if isinstance(item, list) else 'Bad split' for item in caption_split]
    defendant = [item[1] if isinstance(item, list) and len(item) > 1 else 'Bad split' for item in caption_split]
    return(plaintiff, defendant)

def return_amicus_df(path):
    
    amicus_alltext = convert_pdf_to_txt(path)
    
    ## split amicus to get summary
    amicus_split = amicus_alltext.splitlines()

    ## pull the index of the start and ends of the brief
    arg_start = 'SUMMARY OF ARGUMENT'
    arg_start_all = [amicus_split.index(i) for i in amicus_split if arg_start in i]
    if len(arg_start_all) == 0:
        arg_start_index = 0
    elif len(arg_start_all) == 1:
        arg_start_index = arg_start_all[0]
    else:
        arg_start_index = arg_start_all[1]

    arg_end = 'Respectfully submitted'
    arg_end_index = min([amicus_split.index(i) for i in amicus_split if arg_end in i])
    
    ## extract the brief argument
    amicus_arg = " ".join(amicus_split[arg_start_index+1:arg_end_index])

    ## return stemmed/lemmatized 
    amicus_stemmed = " ".join([porter.stem(i.lower()) for i in wordpunct_tokenize(amicus_arg) if 
                        i.lower() not in stop_words and i.lower().isalpha()])
    
    return(amicus_stemmed)
    


################## Step one: read in briefs 


path_to_briefs_parents = '/Users/raj2/Dropbox/endrewamicus_textanalysis/parents/'
os.chdir(path_to_briefs_parents)
current_path = os.getcwd()
briefs = glob.glob('*pdf*')
briefs_paths_parents = [path_to_briefs_parents + "/" + brief for brief in briefs]
path_to_briefs_district = '/Users/raj2/Dropbox/endrewamicus_textanalysis/district/'
os.chdir(path_to_briefs_district)
current_path = os.getcwd()
briefs = glob.glob('*pdf*')
briefs_paths_district = [path_to_briefs_district + "/" + brief for brief in briefs]


################## Step two: iterate over briefs, convert pdf to text, and
################## store stemmed representation in list


all_parents = []
all_districts = []
for i in range(0, len(briefs_paths_parents)):
    
    one_path = briefs_paths_parents[i]
    print(one_path)
    one_amicus = return_amicus_df(one_path)
    print('returned amicus')
    all_parents.append(one_amicus)
    

for i in range(0, len(briefs_paths_district)):
    
    one_path = briefs_paths_district[i]
    print(one_path)
    one_amicus = return_amicus_df(one_path)
    print('returned amicus')
    all_districts.append(one_amicus)



################## Step three: extract names of petitioners and store in dataframe along with text

## name of amicus petitioners 
parent_briefs_init = [re.sub("/Users/raj2/Dropbox/endrewamicus_textanalysis/parents//(15-827-amicus-petitioner-)?", "", x) 
                 for x in briefs_paths_parents]
parent_briefs_final = [re.sub(".pdf", "", x) 
                 for x in parent_briefs_init]

district_briefs_init = [re.sub("/Users/raj2/Dropbox/endrewamicus_textanalysis/district//15-827.amicus.resp(ondent)?[\\-|\\_]", "", x) 
                 for x in briefs_paths_district]
district_briefs_final = [re.sub(".pdf", "", x) 
                 for x in district_briefs_init]

filed_for = ['parent'] * len(parent_briefs_final) + ['district'] * len(district_briefs_final)

amicus_id = pd.DataFrame({'org': parent_briefs_final + district_briefs_final,
                             'who_filed_for': filed_for})

################## Step four: create document-term matrix representation and write to .csv

amicus_dtm = create_nonmasked_dtm(list_of_strings=all_parents + all_districts, metadata = amicus_id)

amicus_dtm.to_csv('/Users/raj2/Dropbox/endrewamicus_textanalysis/output/endrew_amicus_dtm.csv',
                  index = False)
  

