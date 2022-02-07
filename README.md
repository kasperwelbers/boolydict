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

## Examples

The center pieces of the package is three functions: dict_match, dict_add and dict_filter.
They all work mostly the same. The first argument is a data.frame that has a text column,
which should be indicated with the text_col argument (default is "text"). The second
argument is a dictionary, which is a data.frame with a "string" column, that contains
the dictionary term or boolean query, and any other columns that you might want to use. 

### dict_filter

You can use dict_filter to filter a data.frame based on a query (or vector of queries).

``` r
library(boolydict)

dict = data.frame(string = 'war AND peace')
war_and_peace = dict_filter(sotu_texts, dict)
table(war_and_peace$president)
```

For convenience, in dict_filter the dict can also just be a character vector.

``` r
war_and_peace = dict_filter(sotu_texts, 'war AND peace')
table(war_and_peace$president)
```

For matching the dictionary to tokens, you need to provide the context_col argument
to specify a unique context_id. The following data is a tokens data.frame produced by the 
coreNLP pipeline. Each row is a token, and here we want to filter within sentences.

``` r
dict_filter(corenlp_tokens, '<according to>', text_col='token', context_col='sentence')
```

By default dict_filter returns the full context if one of the rows is matched. You can
also just retrieve the specific rows.

``` r
dict_filter(corenlp_tokens, '<according to>', text_col='token', context_col='sentence',
            keep_context=F)
```


### dict_add

use dict_add to apply a dictionary and add matches to input data frame. Since each row in the data
can have multiple dictionary matches, the columns in the dictionary need to be summarized/aggregated into scalars (i.e. single values). 
This is done with name-value pairs, where the name is the name of the column
that will be added, and the value is an expression to extract the scalar from the
matched rows in the dictionary. 

``` r
person_dict = data.frame(string= c('john AND mary','mary AND pete', '<according to>'),
                         label = c('JOHN AND MARY','MARY AND PETE', 'ACCORDING TO'))

tokens = dict_add(corenlp_tokens, person_dict, label=label[1], 
                                  text_col='token', context_col='sentence')
tokens[,c('id','token','label')]
```

You can also use by_label to perform the summary for each value of a categorical variable.
Here we use a dictionary that has the columns AffectDimension and score. We want to sum
the score for each AffectDimension label.

``` r
library(textdata)
nrc = lexicon_nrc_eil()
dict = import_dictionary(nrc)

d = dict_add(sotu_texts, dict, score=sum(score), by_label='AffectDimension', fill=0)
head(d[, c('president','anger','fear','joy','sadness')])
```

There are other ways of filtering and joining that we have not (yet) implemented.
We exposed the bare-bones function for retrieving the matches if you need more flexibility.

``` r
matches = dict_match(sotu_texts, dict)
matches
```
