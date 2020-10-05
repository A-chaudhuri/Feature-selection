import random
import numpy as np
import pandas as pd
from scipy.sparse import *
from skfeature.utility.construct_W import construct_W
import math
import time

'''
############ CHANGE THIS
FILEPATH = '/home/deepu/Documents/Sem6/BTP/kmodes_master'

import sys
sys.path.insert(0, FILEPATH)

'''
start_time=time.time()
df = pd.read_csv("D:/arpita (1)/arpita/data analytics/my work/flag.data.txt",header=None,na_values=["?"])
feats = list(df)
dataset= df.values
y_true = dataset[:,0]
dataset = dataset[:,1:]
n_samples = dataset.shape[0]
n_feats= dataset.shape[1]


sample = (int)( 0.2*n_feats)
feat_ranks = []
count =0
rem_cols = [ i for i in range(n_feats)]

while count < sample:
    print(len(rem_cols))
    xx = len(rem_cols)
    print(xx)
    entropies = np.zeros(xx)
    #print(entropies.shape)
    for i in range(len(rem_cols)):
        myset = rem_cols.copy()
        #print(len(rem_cols))
        myset.remove(rem_cols[i])
        #print(len(rem_cols))
        #print(rem_cols)
        # FIND ENTROPY OF THIS SET
        entr = 0
        for j in range(n_samples):
            for k in range(n_samples):
                x_j = dataset[j,myset]
                x_k = dataset[k,myset]
                score = 0
                for m in range(len(myset)):
                    if x_j[m]==x_k[m]:
                        score += 1
                S_jk = score / len(myset)
                #print(S_jk)
                if S_jk:
                    entr -= S_jk* math.log(S_jk) 
                if (1-S_jk):    
                    entr -=  (1-S_jk) * math.log(1-S_jk)
        #print((i,len(entropies))) 
        entropies[i] = entr
        
    index = np.argmin(entropies)
    feat_ranks.append(rem_cols[index])
    rem_cols.remove(rem_cols[index])
    #print(rem_cols[index])
    count += 1
 
print([feats[i] for i in feat_ranks])    

end_time=time.time()
print("time difference")
print(end_time-start_time)
newfeats = feat_ranks[0:sample]
dataset_new = dataset[:,newfeats] 

classes = np.unique(y_true)





from kmodes.kmodes import KModes as kkd
kk = kkd(n_clusters=classes.shape[0])




mydf = pd.DataFrame(dataset_new)
mydf = mydf.apply(lambda x:x.fillna(x.value_counts().index[0]))
dataset_new = mydf.values
part = kk.fit(dataset_new)

y_pred = part.predict(dataset_new)


import metrics
print(metrics.accuracy(y_true,y_pred))       
       

        







