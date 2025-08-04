test_that("Basic VectorStream tests", {
DestructableDir = paste0(getwd(),"/VectorStreamTest")
unlink(DestructableDir, recursive = TRUE)

expect_equal(!dir.exists(DestructableDir), TRUE)


if(class(try(expect_error(VectorStream_r(100, getwd()), "VectorStream_r will shred through files in the operating directory.\nFor safe operation please specify a valid name for a directory that doesn't exist."), silent = TRUE))[1] != "simpleError"){
stop("FATAL - Vector stream respect an existing directory. Halting to avoid risk of file system damage.")
}

MyVectorStream = VectorStream_r(77, DestructableDir) #need a non-size aligned number so that we get some empty space in the top output

expect_equal(dir.exists(DestructableDir), TRUE)

#for(i in 1:320){
for(i in sample(1:320,320,FALSE)){
  MyVectorStream$set(i,i)
}

for(i in sample(1:320,320,FALSE)){
  expect_equal(MyVectorStream$get(i), i)
}
MyVectorStream$set(600,600)
expect_equal(is.na(MyVectorStream$get(440)), TRUE)
expect_equal(MyVectorStream$get(600), 600)
expect_no_error(MyVectorStream$DestroyOperatingDirectoryAndAllFilesTherein())

MyVectorStream = VectorStream_r(100, DestructableDir)

for(i in 1:320){
  MyVectorStream[[i]] = i
}

for(i in sample(1:320,320,FALSE)){
  expect_equal(MyVectorStream[[i]], i)
}

expect_no_error(MyVectorStream$DestroyOperatingDirectoryAndAllFilesTherein())

MyVectorStream = VectorStream_r(100, DestructableDir)

MyVectorStream[1:320] = 1:320

expect_equal(MyVectorStream[1:320], 1:320)

expect_no_error(MyVectorStream$DestroyOperatingDirectoryAndAllFilesTherein())

expect_equal(!dir.exists(DestructableDir), TRUE)

})
