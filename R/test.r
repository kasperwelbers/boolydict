function() {

  text = c('Renewable fuel is better than fossil fuels!',
           'A fueled debate about fuel',
           'Mark Rutte is simply Rutte. Bos, on the other hand, is not always Wouter',
           'Hey, A ~ symbol!! Can I match that?')
  tc = corpustools::cre



  query = 'mark AND rutte'
  feature = ''
  queries = parse_queries(query, feature = feature)

  tokens = data.frame(token=text, )
  context_level = 'doc_id'
  feature = 'token'
  dict_results = get_dict_results(tc, queries, context_level, feature)


  dictionary_lookup(text, c('mark', 'fossil'))


}
