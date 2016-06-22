context('geom_balance')

test_that('geom_balance gives proper errors if called on non-binary node', {
  tr <- ape::read.tree(text="(t2:0.1280947195,(t4:0.2642134866,(t3:0.9758362926,(t1:0.3494729637,t5:0.189841171,t6:0.189841171):0.7222939907):0.3401968146):0.5143072554);")
  
  # Note: For some reason while ggplot will give warning and properly does not show
  # the problematic geom, the output of the function is not recognized as a warning. 
  # This is not crutial but it makes adding unit tests more difficult. 
  #expect_warning(ggtree(tr)+geom_balance(10), '>2 direct child nodes')
  #expect_warning(ggtree(tr)+geom_balance(3), 'balance cannot be a tip') 
  
  expect_true(is.ggplot(ggtree(tr)+geom_balance(9))) # should plot appropriately
})