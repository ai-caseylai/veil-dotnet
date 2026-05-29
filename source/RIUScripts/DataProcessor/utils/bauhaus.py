#!/usr/bin/env python3
# Utils files for Bauhaus
import numpy as np
import pandas as pd
import logging,ast,sys
from toolz import filter


def clean_data(df):
    # Remove unnessesary items
    df = df[(df['ProductID'] != 'PSB001') & (df['Quantity'] > 0)]
    
    # The following adjust the UnitPrice and TotalPrice (rename to be NetPrice) 
    # to be the Price per Unit
    df = df.assign(UnitPrice=pd.Series(np.asarray(df['UnitPrice']) / 
           np.abs(np.asarray(df['Quantity'])), index=df.index))
    df = df.assign(TotalPrice=pd.Series(np.asarray(df['TotalPrice']) / 
           np.abs(np.asarray(df['Quantity'])), index=df.index))
    return df


def debug_print(*args,debug=None):
    if debug: print(debug, ' ',*args,file=sys.stderr)


class PersistentLabelBuilder:
    '''Derive categories from product names'''
    from sklearn.feature_extraction.text import CountVectorizer

    def __init__(self,excludes=frozenset(),store=None):
        '''excludes should be an iterables of excluded words/tokens'''
        self.tokenSpec = r'\b[a-z]{3,}' # should use \w as there are chinese characters
        self.excludes = excludes
        self.reset()
        import re;
        from cytoolz import filter
        from nltk import regexp_tokenize
        #token_regex = r'\b[a-z\u4e00-\u9fff]{3,}\b'
        # replace t with tee and avoid using something like XXX (clearly not an english word)
        self.tokenizer = lambda prodname: filter(lambda x: x not in self.excludes,regexp_tokenize(re.sub(r'\bt\b',' tee ',' '.join(self.categoryMap[prodname]).lower()),self.tokenSpec))
        
    def reset(self):
        self.redundantMap = {}
        self.categoryMap = {}
        self.featureProductMap = {} # feature is a frozen set of key words, product is a 'normalized' product name
        self.productFeatureMap = {} # values are (str,featureVector) tuples
        self.vecCounterMap = {}
        self.vocabulary_ = None
        
    def save(self,path):
        with open(path,'w') as f:
            f.write(list(filter(lambda x:len(x) > 1,self.categoryMap.values())).__repr__())

    def restore(self,path,reset=True):
        '''default is clear internal state before loading; otherwise, attempt to merge(no validation to object
           being consistent)
        '''
        if reset: self.reset()
        self.load(path)

    def load(self,path):
        from ast import literal_eval
        with open(path) as f:
            categorySets = literal_eval(f.read())
        for s in categorySets:
            key = None
            for i in s:
                if key is None:
                    key = i; self.categoryMap[key] = s
                else:
                    assert(i not in self.redundantMap)
                    self.redundantMap[i] = key

    def addProducts(self,names):
        from translate import Translator
        translator = Translator(to_lang="en",from_lang="zh")
        for n in set(filter(lambda x: x not in self.categoryMap and x not in self.redundantMap,names)):
            tn = translator.translate(n)
            self.categoryMap[tn] = {tn}
            if tn != n: self.redundantMap[n] = tn

    def getEffectiveName(self,nameKey,stopKey=None):
        '''stopKey is a known mapping for nameKey, primarly use is to avoid infinite loop as
           a result of cycle. The default means no known mapping found'''
        while nameKey in self.redundantMap and nameKey != stopKey:
            nameKey = self.redundantMap[nameKey]
        assert(nameKey in self.categoryMap or nameKey == stopKey)
        return nameKey

    def mergeTo(self,destKey,*srcKeys,debug=None):
        assert(destKey in self.categoryMap)
        for k in filter(lambda x: x != destKey and x in self.categoryMap, srcKeys):
            assert(k not in self.redundantMap)
            self.categoryMap[destKey] |= self.categoryMap.pop(k)
            self.redundantMap[k] = destKey
            if (debug):
                print("{} merge: {} -> {}".format(debug,k,destKey),file=sys.stderr)

    def addAlias(self,i):
        '''create productname alias i[0] -> i[1], where i could be a tuple or list'''
        assert(i[0] != i[1])
        if i[0] in self.categoryMap:
            y1 = self.getEffectiveName(i[1])
            self.mergeTo(y1,i[0],debug='Direct')
        elif i[0] in self.redundantMap: # i[0] is a removed value from categories
            # try to see if i[1] can be found by going thru the redundant chain starting from i[0]
            y1 = self.getEffectiveName(self.redundantMap[i[0]],i[1])
            if y1 == i[1]: return  # i[0] -> i[1] chain is already established 
            assert(y1 in self.categoryMap)
            
            y2 = self.getEffectiveName(i[1],y1)
            if (y1 == y2): return
            assert(y2 in self.categoryMap)

            #defaultLog.warning("(%s,%s) leads to: %s <=> %s",i[0],i[1],y1,y2)
            self.mergeTo(y2,y1,'Indirect')
        else:
            self.redundantMap[i[0]] = i[1]

    def deriveCategory(self,name):
        effName = self.getEffectiveName(name)
        return self.productFeatureMap[effName][0] if effName in self.productFeatureMap else effName

    def getProductFeatures(self,prodname):
        if prodname in self.categoryMap:
            from nltk import FreqDist
            from cytoolz import filter,map
            # get the freq table and the strip off anything smaller than half of max values
            freqDist = FreqDist(self.tokenizer(prodname))
            if 'tee' in freqDist and 'shirt' in freqDist:
                freqDist.pop('shirt')
            if freqDist:
                freqs = sorted(freqDist.values());
                freqs = freqs[-6:]
                freqThreshold = max(freqs[-1] * 0.4,freqs[0])
                featureSet = frozenset(map(lambda x: x[0],filter(lambda x: x[1] >= freqThreshold,freqDist.items())))
                return featureSet
        # '2112' 'S03F-009' are names that have no features
        return frozenset()

    
    def buildVocabulary(self):
        '''for each category, derive a set of features(words) by concat all elements into a corpus
           merge all sets of the same features
        '''
        self.featureProductMap = {}
        redo = False
        catKeys = sorted(self.categoryMap.keys())
        for ck in catKeys:
            if ck in self.categoryMap: # previous merge can remove ck from categoryMap
                featureSet = self.getProductFeatures(ck)
                if not featureSet: continue  # '2112' is a product name in category.
                if featureSet in self.featureProductMap:
                    assert(ck != self.featureProductMap[featureSet])
                    self.mergeTo(self.featureProductMap[featureSet],ck,debug='Vocab')
                    redo = True
                else:
                    self.featureProductMap[featureSet] = ck
        if redo:
            return self.buildVocabulary()
        else:
            # try to combine small product groups before building vocabulary(feature keywords)
            vocab = self.vocabulary(True)
            for a,b in self.featureProductMap.items():
                self.productFeatureMap[b] = ('%:@'.join(sorted(list(a))),a) ##tuple(vocab[f] for f in a))
            return vocab
        
    def buildCounterMap(self):
        '''build a CountVectorizer per category. This is causing an exception because
           some product names (an an entry for fitting) doesn't have a valid token.
        '''
        for c in self.productFeatureMap.keys():
            # n-gram should be 1 as there is inconsistent
            cvec = CountVectorizer(max_features=4,ngram_range=(1,2),min_df=0.1,lowercase=True,
                                   token_pattern=self.tokenSpec)
            cvec.fit(list(self.categoryMap[c]))
            self.vecCounterMap[c] = cvec

    def vocabulary(self,skipCache=False):
        '''return keyword|feature to index dict'''
        if skipCache or self.vocabulary_ is None:
            from cytoolz import reduce
            from collections import Counter
            self.features_ = reduce(lambda x,y: x.update(y) or x, self.featureProductMap.keys(), Counter())
            self.vocabulary_ = dict(zip(self.features_.keys(),range(0,len(self.features_))))
        return self.vocabulary_

    def pickFeatures(self,features,n):
        '''pickFeatures help eliminate insignificant words/features'''
        picked = list(map(lambda f: (self.features_[f],f),features))[-n:]
        return frozenset([x[1] for x in picked])

    def combineBySize(self,inclusiveThreshold,featureCnt=4):
        ''' inclusiveThreshold < 1 will be measured as fraction of existing base categories, >= 1 as number of products in a category.
            This method will try to combine the categories <= the given threshold -- by extract a feature vector [featureCnt is the
            max dimension of feature vector].
            The feature vectors are compared using set membership test.
            return the reduction in number of categories
        '''
        localFeatureProductMap = {}
        # sorted returns a list
        sm = sorted(filter(lambda x: len(x[1]) <= inclusiveThreshold,self.categoryMap.items()),key=lambda x:len(x[1]),reverse=True)
        print("sm threshold={} length={}".format(inclusiveThreshold,len(sm)),file=sys.stderr)
        for fCnt in range(featureCnt,2,-2): # make sure one iteration is one even when featureCnt is small
            for k,_ in sm:
                if k not in self.categoryMap: continue
                featureSet = self.pickFeatures(self.getProductFeatures(k),fCnt)
                if not featureSet: continue
                while featureSet in localFeatureProductMap:
                    self.mergeTo(k,localFeatureProductMap.pop(featureSet),debug='duplicate')
                    featureSet = self.pickFeatures(self.getProductFeatures(k),fCnt)

                if len(featureSet) > fCnt - 2 or fCnt -2 < 2:
                    for s in list(localFeatureProductMap.keys()):
                        if featureSet < s and k != localFeatureProductMap[s] and len(featureSet) * 2 > len(s):
                            self.mergeTo(k,localFeatureProductMap.pop(s),debug="close subset")

                # multiple entries mapped to the same k is possible
                localFeatureProductMap[featureSet] = k

    def combineByProportion(self,inclusiveThreshold,featureCnt=3):
        assert(inclusiveThreshold < 1)
        localFeatureProductMap = {}
        origSize = len(self.categoryMap)
        handleSize = int(origSize * inclusiveThreshold)
        rankedPairs = sorted(self.categoryMap.items(),key=lambda x:len(x[1]),reverse=False)
        debug_print("mapSize={} fCnt={}".format(len(self.categoryMap),featureCnt),debug='Enter')
        for k,_ in rankedPairs[0:handleSize]:
            if k not in self.categoryMap: continue
            featureSet = self.pickFeatures(self.getProductFeatures(k),featureCnt)
            if not featureSet: continue; #should consider droping it
            while featureSet in localFeatureProductMap:
                self.mergeTo(k,localFeatureProductMap.pop(featureSet),debug='duplicate')
                featureSet = self.pickFeatures(self.getProductFeatures(k),featureCnt)

            betterFit = None
            for t,_ in rankedPairs[handleSize:]:
                tfeatures = self.pickFeatures(self.getProductFeatures(t),featureCnt+featureCnt)
                common = featureSet & tfeatures
                if len(common) * 2 > len(featureSet):
                    if betterFit is None or len(common) > len(betterFit[0]):
                        betterFit = (common,t)


            if betterFit is None:
                localFeatureProductMap[featureSet] = k
            else:
                self.mergeTo(betterFit[1],k,debug='subportion')

        debug_print("mapSize={} fCnt={}".format(len(self.categoryMap),featureCnt),debug='Exit')
        return origSize - len(self.categoryMap)


def derive_categories(df_txn, df_categories):
    import re
    colorNames = frozenset(map(lambda s: re.search('[a-z]{3,4}',s.lower()).group(), 
                               frozenset(df_categories.ColorName[~pd.isnull(df_categories.ColorName)].values)))
    df_categories = df_categories[~pd.isnull(df_categories.ProductName)]
    
    debug_print(len(df_categories),debug='ProductName size in catgories')

    merged = pd.merge(df_txn, df_categories['ProdCodeStyle ProductID ProductName DepartName'.split(' ')], 
                                         on='ProductID')

    check = merged[merged.ProductName_x != merged.ProductName_y]
    check = check[~pd.isnull(check.ProductName_x)]['ProductID ProductName_x ProductName_y'.split()]

    builder = PersistentLabelBuilder(colorNames)

    fh = logging.FileHandler('EquivalentCategories.log',mode='w');
    defaultLog = logging.getLogger('check.ProductName.X.Y')
    defaultLog.addHandler(fh)

    # valid product name set starts with what's in categories.
    builder.addProducts(df_categories.ProductName)
    
    def equivalentByStyle(builder):
        #local function to have an scope to update redundant and categoryMap
        # check if a product name appears to have multiple style codes
        subcategories = df_categories[~pd.isnull(df_categories.ProdCodeStyle)]
        subcategories = subcategories.select(lambda x: subcategories.ProductName[x] in merged.ProductName_y)
                                   
        styleCheckList = list(subcategories.groupby('ProdCodeStyle ProductName'.split()).groups.keys())

        nameCache = {}
        for x in styleCheckList:
            # x[0] is style, x[1] is product name
            if x[1] in nameCache:
                nameCache[x[1]].add(x[0])
            else:
                nameCache[x[1]] = {x[0]}
                
        # trying to find ProductNames of same code Style
        styleCache = {}
        for x in styleCheckList:
            # x[0] is style, x[1] is product name
            effectiveStyle = max(nameCache[x[1]])
            if effectiveStyle not in styleCache:
                styleCache[effectiveStyle] = {x[1]}
                continue
            else:
                if x[1] in styleCache[effectiveStyle]: continue

            destKey = builder.getEffectiveName(tuple(styleCache[effectiveStyle])[0],x[1])
            # x[1] is not in styleCache, so new product name for the style
            styleCache[effectiveStyle].add(x[1])
            if destKey != x[1]:
                refKey = builder.getEffectiveName(x[1])
                if (refKey != destKey):
                    builder.mergeTo(destKey,refKey,debug='Style')
                    assert(builder.getEffectiveName(x[1]) == destKey)
                
        # only keep those with multiple values in set
        for k in list(filter(lambda x: len(nameCache[x]) <= 1,nameCache.keys())):
            nameCache.pop(k)

        return (nameCache,styleCache)

    # for any appears in check.ProductName_x
    nameCheckList = check.groupby(['ProductName_x','ProductName_y']).groups.keys()

    for i in nameCheckList:
        assert(i[0] != i[1])
        builder.addAlias(i)

    nameDict,styleDict = equivalentByStyle(builder)

    for fcnt in [6,4,3]:
        if 0 == fcnt %2: builder.buildVocabulary()
        builder.combineBySize(22,fcnt)
        chgThreshold = len(builder.categoryMap) * 0.1;
        while builder.combineByProportion(0.25,fcnt) > chgThreshold:
            pass

    builder.combineBySize(25)
    fh.close()
    
    merged['derivedCategory'] = merged.ProductName_y.apply(builder.deriveCategory)

    return merged['ProductID derivedCategory'.split()]
