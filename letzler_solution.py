#Score notes--
#Basic rule-based approach to correcting a/an errors: 0.0% target rate
#Bigram a/an replacement--4.57%
#Just 4-grams: 36.6%/72.28% [Note--made mistake: not enough spaces in the ngrams]
#Add two rules--37.45%/74.56%
#3-grams plus two rules--40.17%/78.42%

import json
import itertools
from __future__ import division

with open("./grammarly_research_assignment/data/sentence_train.txt") as f:
    text = json.load(f)
with open("./grammarly_research_assignment/data/corrections_train.txt") as f:
    correct = json.load(f)
with open("./grammarly_research_assignment/data/sentence_train.txt") as f:
    submission= json.load(f)
with open("./grammarly_research_assignment/data/ngrams.txt") as f:
    ngrams = json.load(f)
with open("./grammarly_research_assignment/data/pos_tags_train.txt") as f:
    pos = json.load(f)

text = [[x.lower() for x in y] for y in text]
submission = [[x.lower() for x in y] for y in submission]
vowels = ['a', 'e', 'i', 'o', 'u']
nouns = ["NN", "NNS", "NNP", "NNPS"]

for i in range(0, len(text)):

    #Locate all articles
    art_index = [y for y,x in enumerate(text[i]) if x == 'a' or x == "an" or x=="the"]

    #Part I: Generate base probabilities for each article based on 4-grams
    #Generate list of 4-grams for each article (or 2/3-grams if at start of sentence)
    for j in range(0, len(art_index)):
        current = text[i][art_index[j]-1:art_index[j]+2]
        #Deal with cases where there are either no articles or articles at the start of the sentence
        if current ==[]:
            try:
                if text[i].index("the" or "a" or "an")==0:
                    current = text[i][0:2]
            except:
                pass
        #Generate at set of possible article ngrams
        if len(current)==3:
            the_gram = current[0] + " the " + current[2]
            a_gram = current[0] + " a " + current[2]
            an_gram = current[0] + " an " + current[2]
        elif len(current)==2:
            the_gram = "the " + current[1]
            a_gram = "a " + current[1]
            an_gram = "an " + current[1]
        else:
            continue
        #Determine frequency of ngrams
        try:
            a_total = ngrams[a_gram]
        except:
            a_total = 0
        try:
            an_total = ngrams[an_gram]
        except:
            an_total = 0
        try:
            the_total = ngrams[the_gram]
        except:
            the_total = 0
        #Determine relative frequency
        total = a_total + an_total + the_total
        if total!=0:
            a_current = a_total/total
            an_current = an_total/total
            the_current = the_total/total
            #Make corrections in submission file
            if current[-2]=="a" or current[-2]=="an":
                if a_current + an_current >= the_current:
                    submission[i][art_index[j]] = ["a", a_current + an_current]
                else:
                    submission[i][art_index[j]] = ["the", the_current]
            else:
                if the_current >= a_current + an_current:
                    submission[i][art_index[j]] = ["the", the_current]
                else:
                    submission[i][art_index[j]] = ["a", a_current + an_current]
        #Look past rarely occurring phrases
        else:
            submission[i][art_index[j]] = [current[-2], 1]

        #RULE: Superlatives always take "the"
        if pos[i][art_index[j]+1]=="JJS":
            submission[i][art_index[j]] = ['the', 1]
        #RULE: Plural and proper nouns only take "the"
        it=1
        while art_index[j] + it < len(text[i]):
            if pos[i][art_index[j] + it] in nouns:
                if pos[i][art_index[j]+it] in ["NNS", "NNPS"]:
                    submission[i][art_index[j]] = ['the', 0.99]
                break
            it +=1

    #A/an replacement
    #Locate all words/indices for indefinite articles
    indef_index = [y for y,x in enumerate(submission[i]) if type(x)==list and (x[0] == 'a' or x[0] == "an")]
    indef_art = [x[0] for x in submission[i] if type(x)==list and (x[0] == 'a' or x[0] == "an")]
    noun_index = [x + 1 for x in indef_index]
    indef_nouns = []
    for index in noun_index:
        indef_nouns.append(text[i][index])
    bigram = [x + " " + y for x,y in zip(indef_art, indef_nouns)]
    #Lookup bigrams in ngram dictionary
    for k in range(0, len(indef_nouns)):
        current_noun = ["a " + indef_nouns[k], "an " + indef_nouns[k]]
        try:
            a_total = ngrams[current_noun[0]]
        except:
            a_total = 0
        try:
            an_total = ngrams[current_noun[1]]
        except:
            an_total = 0

        #Determine relative frequenices of a/an
        indef_total = a_total + an_total
        if indef_total!=0:
            a_current = a_total/indef_total
            an_current = an_total/indef_total
        #Rely on basic aeiou rule for rarities
        elif indef_total==0:
            if list(indef_nouns[k])[0] in vowels:
                a_current = 0.01
                an_current = 0.99
            else:
                a_current = 0.99
                an_current = 0.01

        #Input corrections where necessary
        if a_current >= an_current:
            submission[i][indef_index[k]] = ['a', a_current * submission[i][indef_index[k]][1]]
        else:
            submission[i][indef_index[k]] = ['an', an_current * submission[i][indef_index[k]][1]]

    #Eliminate predictions where original text is already correct
    for l in range(0, len(text[i])):
        #Convert non-articles to None
        if text[i][l]==submission[i][l]:
            submission[i][l] = None
        #Convert correct articles to None
        try:
            if text[i][l]==submission[i][l][0]:
                submission[i][l] = None
        except:
            pass
        else:
            pass




#vowels = ['a', 'e', 'i', 'o', 'u']
#for i in range(0, len(text)):
#    indef_index = [y for y,x in enumerate(text[i]) if x == 'a' or x == "an"]
#    noun_index = [x + 1 for x in indef_index]
#    indef_nouns = []
#    for index in noun_index:
#        indef_nouns.append(list(text[i][index]))
#    indef_art = []
#    for word in indef_nouns:
#        if word[0] in vowels:
#            indef_art.append('an')
#        else:
#            indef_art.append('a')
#    indef_sub = zip(indef_art, indef_index)
#    for j in range(0, len(indef_sub)):
#        text[i][indef_sub[j][1]] = indef_sub[j][0]
#
#    for k in range(0, len(text[i])):
#        if text[i][k]==submission[i][k]:
#            submission[i][k] = None
#        else:
#            submission[i][k] = [submission[i][k], 1]

#
