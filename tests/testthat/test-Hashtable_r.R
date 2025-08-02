SomeHashtable = Hashtable_r()
SomeHashtable$set_u("Hello", 10)
SomeHashtable$set_u("World", 20)
expect_equal(SomeHashtable$set_s(42, 24), TRUE)
expect_equal(length(SomeHashtable$Keys()), 3)
expect_equal(SomeHashtable$get_u("42"), 24)
expect_equal(SomeHashtable$get_s(42), 24)
expect_equal(SomeHashtable$get_u("Hello"), 10)
expect_equal(SomeHashtable$exists_u("Hello"), TRUE)
expect_equal(SomeHashtable$exists_s(42), TRUE)
expect_equal(SomeHashtable$exists_s("Mischevious Plot"), FALSE)
SomeHashtable$Clear()
expect_equal(length(SomeHashtable$Keys()), 0)
expect_equal(SomeHashtable$exists_u("Hello"), FALSE)
expect_equal(SomeHashtable$exists_s(42), FALSE)

SampleSet = as.character(sample(1:1000, 1000, F))

for(i in SampleSet){
  SomeHashtable$set_u(i, i)
}

for(i in SampleSet){
  expect_identical(SomeHashtable$get_u(i), i)
}


