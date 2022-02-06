# boolydict: Simple dictionary search with Boolean operators

This package is basically the search functionality of the corpustools package,
but without having to use corpustools. It's currently in early dev, but the idea
is that you can perform dictionary search directly on a data.frame with
text columns. The key features are:

* It supports Boolean operators like AND, OR, NOT, multiword strings, proximity and more. You could for instance query for <(president OR joe) biden>~10, to only match "Biden" within 10 words from "president" or "Joe"
* These operators can be applied on both full texts and tokenized texts. This means that you can take a data.frame of tokens, produced by an NLP pipeline like udpipe, spacyr or tidytext, and apply these Boolean empowered dictionaries to find matches at the level of individual tokens. This provides a flexible way for rule-based token annotation.


## Installation

``` r
remotes::install_github('boolydict')
```

## Example

Directly filter a data.frame based on a query (or vector of queries)

``` r
library(boolydict)

war_and_peace = dict_filter(sotu_texts, 'war AND peace')
table(war_and_peace$president)
```

Annotate tokens using a dictionary

``` r
person_dict = data.frame(string= c('john AND mary','mary AND pete', '<according to>'),
                         label = c('JOHN AND MARY','MARY AND PETE', 'ACCORDING TO'))

tokens = dict_summarize(corenlp_tokens, person_dict, label=label[1], fill='',
                        text_col='token', context_col='sentence')
tokens[,c('id','token','label')]
```

Apply a dictionary and add summary of matches to data


``` r
library(textdata)
nrc = lexicon_nrc_eil()
dict = import_dictionary(nrc)

## summarize by summing the dictionary score for every AffectDimension label.
d = dict_summarize(sotu_texts, dict, score=sum(score), by_label='AffectDimension', fill=0)

head(d[, c('president','anger','fear','joy','sadness')])
```
