test_that("Basic ListStream tests", {
DestructableDir = paste0(getwd(),"/ListStreamTest")
unlink(DestructableDir, recursive = TRUE)

expect_equal(!dir.exists(DestructableDir), TRUE)


if(class(try(expect_error(ListStream_r(100, getwd()), "ListStream_r will shred through files in the operating directory.\nFor safe operation please specify a valid name for a directory that doesn't exist."), silent = TRUE))[1] != "simpleError"){
stop("FATAL - List stream respect an existing directory. Halting to avoid risk of file system damage.")
}

MyListStream = ListStream_r(77, DestructableDir) #need a non-size aligned number so that we get some empty space in the top output

expect_equal(dir.exists(DestructableDir), TRUE)

#for(i in 1:320){
for(i in sample(1:320,320,FALSE)){
  MyListStream$set(i,i)
}

for(i in sample(1:320,320,FALSE)){
  expect_equal(MyListStream$get(i), i)
}


expect_no_error(MyListStream$DestroyOperatingDirectoryAndAllFilesTherein())

MyListStream = ListStream_r(100, DestructableDir)

for(i in sample(1:320,320,FALSE)){
  MyListStream[[i]] = i
}

for(i in sample(1:320,320,FALSE)){
  expect_equal(MyListStream[[i]], i)
}

expect_no_error(MyListStream$DestroyOperatingDirectoryAndAllFilesTherein())

MyListStream = ListStream_r(100, DestructableDir)

MyListStream[1:320] = 1:320

expect_equal(MyListStream[1:320], 1:320)

expect_no_error(MyListStream$DestroyOperatingDirectoryAndAllFilesTherein())

expect_equal(!dir.exists(DestructableDir), TRUE)

MyListStream = ListStream_r(100, DestructableDir)

for(i in sample(1:320,320,FALSE)){
  MyListStream[[i]] = as.list(rep(1, i))
}

for(i in sample(1:320,320,FALSE)){
  expect_equal(MyListStream[[i]], as.list(rep(1, i)))
}

expect_no_error(MyListStream[[1000]])
expect_equal(is.na(MyListStream[[2000]]), TRUE)

expect_no_error(MyListStream$DestroyOperatingDirectoryAndAllFilesTherein())


})
