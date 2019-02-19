import pandas as pd

data = pd.read_csv("Book5.csv")


#Lower case----------------
data['CompanyName'] = data['CompanyName'].apply(lambda x: " ".join(x.lower() for x in str(x).split()))
print(data['CompanyName'].head())


#Removing Punctuation(including special characters)-------------

data['CompanyName'] = data['CompanyName'].str.replace('#+',' ')
data['CompanyName'] = data['CompanyName'].str.replace('0-9','')
data['CompanyName'] = data['CompanyName'].str.replace('_+',' ')
data['CompanyName'] = data['CompanyName'].str.replace('-',' ')
#data['CompanyName'] = data['CompanyName'].str.replace('ooo','')
data['CompanyName'] = data['CompanyName'].str.replace('[^\w\s]','')
data['CompanyName'] = data['CompanyName'].apply(lambda x: ''.join([x for x in str(x) if not x.isdigit()]))
data['CompanyName'] = data['CompanyName'].apply(lambda x: ''.join(x for x in str(x) if ord(x)<128))
data['CompanyName'] = data['CompanyName'].str.replace(' +',' ')
data['wholename'] = data['wholename'].str.replace('^ +','')
data['wholename'] = data['wholename'].str.replace(' +$','')

#Number of Words----------------
data['word_count'] = data['CompanyName'].apply(lambda x: len(str(x).split(" ")))
print(data[['CompanyName','word_count']].head())

set1 = set(map(lambda word: word, data['CompanyName'][2].split(' ')))
print(set1)

#Lower case----------------
data['Name'] = data['Name'].apply(lambda x: " ".join(x.lower() for x in str(x).split()))
print(data['Name'].head())


#Removing Punctuation(including special characters)-------------

data['Name'] = data['Name'].str.replace('#+',' ')
data['Name'] = data['Name'].str.replace('0-9','')
data['Name'] = data['Name'].str.replace('_+',' ')
data['Name'] = data['Name'].str.replace('-',' ')
#data['Name'] = data['Name'].str.replace('ooo','')
data['Name'] = data['Name'].str.replace('[^\w\s]','')
data['Name'] = data['Name'].apply(lambda x: ''.join([x for x in str(x) if not x.isdigit()]))
data['Name'] = data['Name'].apply(lambda x: ''.join(x for x in str(x) if ord(x)<128))
data['Name'] = data['Name'].str.replace(' +',' ')

#Number of Words----------------
data['word_count'] = data['Name'].apply(lambda x: len(str(x).split(" ")))
print(data[['Name','word_count']].head())

set1 = set(map(lambda word: word, data['Name'][2].split(' ')))
print(set1)

df = data
print(df.head())
df.to_csv('EmbargoInternalOutput.csv')