+ attach user specific data to existing tree object
+ attach user specific data by node number
+ mask function that accepts tree object, feature name and a function that specific how to mask.
  with this function, user can mask some of the features that are not needed to display on a tree.
+ defined a function, compare_sequence, that accept a function that defined sequence feature,
  and compare the feature from parent to children and add store the info in tree object.
  So with ancestral sequences inferred by HYPHY or PAML, we can support any type of sequence feature comparison,
  not only substitution supported internally.
+ support more features that can be plotted at the right hand side of the tree.
  - ~~multiple sequence alignment~~ _Now implemented in 1.1.7_
  

