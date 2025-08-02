test_that("Basic numeric tests", {
  MyVector = DynamicVector_r(7)
  for(i in 1:100){
    MyVector$set(i, i)
  }
  for(i in 1:100){
    expect_equal(MyVector$get(i), i)
  }
  expect_equal(length(MyVector$getFlatVector()), 100)
  expect_equal(sum(MyVector$getFlatVector()), sum(1:100))
  expect_equal(MyVector$VectorLength(), 100)
  expect_equal(MyVector$ChunkCount(), 15)
  expect_equal(MyVector$ChunkSize(), 7)
  expect_equal(MyVector$Capacity(), 105)

  expect_error(MyVector$get(-1), regexp = "attempt to select less than one element in get1index")
  expect_error(MyVector$get(0), regexp = "attempt to select less than one element in get1index")
  expect_error(MyVector$get(9001), regexp = "subscript out of bounds")

  MyVector$clear()
  expect_equal(MyVector$VectorLength(), 0)
  expect_equal(MyVector$ChunkCount(), 1)
  expect_equal(MyVector$ChunkSize(), 7)
  expect_equal(MyVector$Capacity(), 7)
  expect_equal(length(MyVector$getFlatVector()), 1)#I.e. 1:0


  for(i in 1:7){
    expect_equal(is.na(MyVector$get(i)), TRUE)
  }

  AnotherDynamicVector = DynamicVector_r(10)

  for(i in 1:100){
    AnotherDynamicVector$set(i,0)
  }

  expect_equal(AnotherDynamicVector$get(42), 0)
  AnotherDynamicVector$set(96,69)

  PassingScopeFunctionA <- function(SomeDynamicVector_r){
    PassingScopeFunctionB <- function(SomeDynamicVector_r){
      expect_equal(SomeDynamicVector_r$get(96),69)
      SomeDynamicVector_r$set(42,24)
      SomeDynamicVector_r$set(420,-1)
    }
    PassingScopeFunctionB(SomeDynamicVector_r)
  }

  PassingScopeFunctionA(AnotherDynamicVector)

  expect_equal(AnotherDynamicVector$get(42), 24)
  expect_equal(length(AnotherDynamicVector$getFlatVector()), 420)

  expect_equal(length(MyVector$getFlatVector()), 1)#This vector should be unchanged


  #Vector growth speed test
  Temp = c()
  gc()
  StartTimeBaseR = Sys.time()
  for(i in 1:30000){
    Temp = c(Temp,i)
  }
  EndTimeBaseR = Sys.time()
  DeltaTimeBaseR = EndTimeBaseR - StartTimeBaseR

  Temp = DynamicVector_r(1000)
  gc()
  StartTimeDVR = Sys.time()
  for(i in 1:30000){
    Temp$set(i,i)
  }
  EndTimeDVR = Sys.time()
  DeltaTimeDVR = EndTimeDVR - StartTimeDVR

  expect_lt(DeltaTimeDVR, DeltaTimeBaseR)




})
