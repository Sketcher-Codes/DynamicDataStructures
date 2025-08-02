test_that("Basic VectorStream tests", {
DestructableDir = paste0(getwd(),"/VectorStreamTest")
unlink(DestructableDir, recursive = TRUE)

expect_equal(!dir.exists(DestructableDir), TRUE)


if(class(try(expect_error(VectorStream_r(100, getwd()), "VectorStream_r will shred through files in the operating directory.\nFor safe operation please specify a valid name for a directory that doesn't exist."), silent = TRUE))[1] != "simpleError"){
stop("FATAL - Vector stream respect an existing directory. Halting to avoid risk of file system damage.")
}

MyVectorStream = VectorStream_r(100, DestructableDir)

expect_equal(dir.exists(DestructableDir), TRUE)

expect_no_error(MyVectorStream$DestroyOperatingDirectoryAndAllFilesTherein())

expect_equal(!dir.exists(DestructableDir), TRUE)

})
