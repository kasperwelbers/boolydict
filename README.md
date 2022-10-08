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
They all work mostly the same. 

* The first argument is a data.frame that has a text column,
which should be indicated with the text_col argument (default is "text"). 
* The second argument is the dictionary. This can simply be a character vector of dictionary
terms or Lucene-like Boolean queries, but it can also be a data.frame with a string column. 
Using a data.frame with **dict_add** allows you to add information from the other columns.
For example, adding labels or sentiment scores.

The package should work with any type of data.frame (and actually relies heavily on data.table behind the scenes). 
In the following examples we just use tidyverse because we're cool like that.

### dict_filter

You can use dict_filter to filter a data.frame based on a query (or vector of queries).


``` r
library(boolydict)
library(tidyverse)

sotu_texts %>%
   dict_filter('war AND peace') %>%
   select(id, president)
```

For matching the dictionary to tokens, you need to provide the context_col argument
to specify a unique context_id. The following data is a tokens data.frame produced by the 
coreNLP pipeline. Each row is a token, and here we want to filter within sentences.

``` r
corenlp_tokens %>%
   dict_filter('<according to>', text_col='token', context_col='sentence') %>%
   select(sentence, token)
```

By default dict_filter returns the full context if one of the rows is matched. You can
also just retrieve the specific rows.

``` r
corenlp_tokens %>%
   dict_filter('<according to>', text_col='token', context_col='sentence', keep_context=F) %>%
   select(sentence, token)
```


### dict_add

Use dict_add to apply a dictionary and add matches to the input data frame. 
Note that every row in the input data frame can potentially match multiple terms from the dictionary.
Therefore, the dictionary matches needs to be summarized/aggregated into scalars (i.e. single values),
or spread over multiple columns. 
This is best explained with an example.

We'll first use the query_from_string function as a convenient way to specify some queries.
Note that spaces without Boolean operators are interpreted as OR conditions and the ~s flag means case sensitive. 

``` r
dict = dict_from_string('
  The Netherlands = netherlands NL~s
  United States = "united states" (US USA)~s
')
```

Now say we have some texts that can mention multiple countries

``` r
df = data.frame(text = c("The Netherlands is smaller than the US",
                         "Or should we say \"The Netherlands are\"?",
                         "Is/are the United States?"))
```

If we now run dict_add without arguments, it tells us how many matches there are.

``` r
df %>%
  dict_add(dict)
```

This shows that we can't just add the dictionary label to **df**, because there would be
multiple labels for the first text. Instead, we should summarize data from the dictionary.
We use a syntax similar to the summarize function in dplyr, or the aggregate syntax in data.table. 
Simply pass name-value pairs, where the name becomes the column name, and the value should be an expression
that returns a single value (or a list, if you're ok with nested data)

``` r
df %>%
  dict_add(dict, n=length(label), first_label=label[1], label_list=list(label))
```

Note that 'hits' is now gone, because it's only included if no custom summaries are specified.
But we can add it ourselves, as we did here with 'n'.

Alternatively, we could also cast the multiple matches as columns.
The summaries are then shown per label.
If no summary statistics are given, the number of matches (hits) will be shown.
Furthermore, fill can be used to replace NA values.

``` r
df %>%
  dict_add(dict, by_label="label", fill=0)
```

Here's another example for aggregating dictionary scores.

``` r
library(textdata)
nrc = lexicon_nrc_eil()
dict = import_dictionary(nrc)

d = dict_add(sotu_texts, dict, score=sum(score), by_label='AffectDimension', fill=0)
head(d[, c('president','anger','fear','joy','sadness')])
```

Finally, if you include multiple aggregating scores, the scores and labels are concatenated
to create the column names. (note that we dropped fill because you might then want to
manually fix NAs for more flexibility. Though in this specific case it would be fine to set them to 0) 

``` r
d = dict_add(sotu_texts, dict, n=length(score), score=sum(score), by_label='AffectDimension')
head(d[, c('president','n.anger','score.anger')])
```



### dict_match

There are off course many other useful ways of filtering and joining that we have not (yet) implemented.
We therefore also expose a bare-bones function for retrieving the matches if you need more flexibility.

``` r
matches = dict_match(sotu_texts, dict)
matches
```

