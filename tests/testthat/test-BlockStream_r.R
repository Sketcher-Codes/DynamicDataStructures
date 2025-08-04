test_that("block stream tests", {
  DestructableDir = paste0(getwd(),"/VectorStreamTest")
  unlink(DestructableDir, recursive = TRUE)
  MyBlockStream <- BlockStream_r(DestructableDir)
  MyBlockStream$AddVectorColumn("Names",124)
  MyBlockStream$AddVectorColumn("Numbers",50)
  MyBlockStream$AddListColumn("Name-Number-Pairs",17)

  for(i in 1:200){
    letter = letters[(i-1)%%i + 1]
    number = sqrt(i)
    pair = list()
    pair$MyName = letter
    pair$MyNumber = number

    MyBlockStream$set_u("Names", i, letter)
    MyBlockStream$set_u("Numbers", i, number)
    MyBlockStream$set_u("Name-Number-Pairs", i, pair)
  }


  for(i in sample(1:200, size = 200, replace = FALSE)){
    letter = letters[(i-1)%%i + 1]
    number = sqrt(i)
    pair = list()
    pair$MyName = letter
    pair$MyNumber = number

    expect_equal(MyBlockStream$get_u("Names", i), letter)
    expect_equal(MyBlockStream$get_u("Numbers", i), number)
    expect_equal(MyBlockStream$get_u("Name-Number-Pairs", i), pair)
  }

  for(i in sample(1:200, size = 200, replace = FALSE)){
    letter = letters[(i-1)%%i + 1]
    number = sqrt(i)
    pair = list()
    pair$MyName = letter
    pair$MyNumber = number

    TestList = list()
    TestList[[1]] = letter
    TestList[[2]] = number
    TestList[[3]] = pair

    expect_equal(MyBlockStream$get_row(i), TestList)

  }

  expect_no_error(MyBlockStream$DestroyOperatingDirectoryAndAllFilesTherein())
})
