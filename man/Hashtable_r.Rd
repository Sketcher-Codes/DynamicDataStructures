\name{Hashtable_r}
\alias{Hashtable_r}
\title{
Hashtable_r
}
\description{
Simple hashtable using an R environment's built in hashing.
}
\details{
Simple hashtable using an R environment's built in hashing. I have not yet encountered hash collisions but they may occur. get_u and set_u are unchecked fast operations that assume you provide a valid string as the key.
get_s and set_s will fail, returning NULL and FALSE respectively if the key is not of length 1, NULL or NA, and will attempt to convert the key to a string before use.
}
\examples{
  library(DynamicDataStructures)
  SomeHashtable = Hashtable_r()
  SomeHashtable$set_u("Hello", 10)
  SomeHashtable$set_u("World", 20)
  SomeHashtable$set_s(42, 24)
  print(SomeHashtable$Keys())
  print(SomeHashtable$get_u("42"))
  print(SomeHashtable$get_s(42))
  print(SomeHashtable$get_u("Hello"))
  print(SomeHashtable$exists_u("Hello"))
  print(SomeHashtable$exists_s(42))
  print(SomeHashtable$exists_s("Mischevious Plot"))
  SomeHashtable$Clear()
  print(SomeHashtable$Keys())
  print(SomeHashtable$exists_u("Hello"))
  print(SomeHashtable$exists_s(42))
}
