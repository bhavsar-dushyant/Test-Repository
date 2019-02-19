import re
import pandas as pd
import xlrd
#import matplotlib.pyplot as plt
#%pylab inline

data = pd.read_excel("Book2.xlsx")
#print(data.head(2))
#data.head(3)
#Lower case----------------
data['alternate_name'] = data['alternate_name'].apply(lambda x: " ".join(x.lower() for x in str(x).split()))
print(data['alternate_name'].head())

#Removing Punctuation(including special characters)-------------

data['alternate_name'] = data['alternate_name'].str.replace('#+',' ')
#data['alternate_name'] = data['alternate_name'].str.replace('0-9','')
data['alternate_name'] = data['alternate_name'].str.replace('_+',' ')
data['alternate_name'] = data['alternate_name'].str.replace('-',' ')
data['alternate_name'] = data['alternate_name'].str.replace(',',' ')
data['alternate_name'] = data['alternate_name'].str.replace('[^\w\s]','')
#data['alternate_name'] = data['alternate_name'].apply(lambda x: ''.join([x for x in str(x) if not x.isdigit()]))
data['alternate_name'] = data['alternate_name'].apply(lambda x: ''.join(x for x in str(x) if ord(x)<128))
data['alternate_name'] = data['alternate_name'].str.replace(' +',' ')
data['alternate_name'] = data['alternate_name'].str.replace('^ +','')
data['alternate_name'] = data['alternate_name'].str.replace(' +$','')

df = data
#print(df)
df.to_csv('EmbargoOut.csv')


#data['alternate_name'] = data['alternate_name'].str.encode('ascii',errors='ignore')
#s = s.encode('ascii',errors='ignore')


#data['alternate_name'] = re.sub(' +',)
#Number of Words----------------
data['word_count'] = data['alternate_name'].apply(lambda x: len(str(x).split(" ")))
print(data[['alternate_name','word_count']].head())

set1 = set(map(lambda word: word, data['alternate_name'][2].split(' ')))
print(set1)


