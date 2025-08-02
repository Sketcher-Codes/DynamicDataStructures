\name{DynamicVector_r}
\alias{DynamicVector_r}
\title{
DynamicVector_r
}
\description{
DynamicVector_r is S3 class that functiosn as a smoothly resizable vector in the C++ sense.
It is useful for when you don't know how big a vector is going to be, and can't pre-allocate.
Under the hood it works by allocating chunks (R vectors of pre-defined length), as sequential members of a R list.
get and set operations are slower than get-and-set for a typical R vector.
however vector growth is much faster as no copying of existing vector data is performed.
}
\details{
Note that unlike a C style array, you can set values outside the upper bound of the array, the dynamic vector will grow and fill with NAs up to that point.
Because the vector data is stored in its own environment, you can easily pass the DynamicVector_r from one function to another without having a huge pass-by-value overhead.
If you're familiar with C++ or C# this pass by reference behaviour should be famaliar to you.
}

\examples{

MyNewVector = DynamicVector_r(10)
MyNewVector$set(1,"Hello")
MyNewVector$set(2,"World")
print(MyNewVector$get(1)) # Hello
print(MyNewVector$get(2)) # World
print(MyNewVector$get(3)) # NA

print(MyNewVector$set(99,3.14))

print(MyNewVector$get(98)) #NA
print(MyNewVector$get(99))  #3.14 - because it's in a different chunk it
                           # is unaffected by the first vector indicies 1 to 10 being strings now.

print(MyNewVector$get(100)) #NA
#print(MyNewVector$get(101)) #an error - cannot get outside the bounds of the array


}
