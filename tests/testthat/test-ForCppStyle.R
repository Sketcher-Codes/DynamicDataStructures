test_that("does our for work as expected", {
  SomeVector = rep(0, 10)
  expect_equal(length(SomeVector), 10)
  expect_equal(sum(SomeVector), 0)


  {i = 1; i <= 10; i = i + 1} %for% {
    SomeVector[i] = i
  }

  expect_equal(sum(SomeVector), sum(1:length(SomeVector)))
  expect_equal(i, length(SomeVector) + 1)

  {i = 1; i <= 10; i = i + 1} %for% {
    SomeVector[i] = -i
    if(i == 5){
      break;
    }
  }

  expect_equal(i, 5)
  expect_equal(sum(SomeVector), sum(-1:-5,6:length(SomeVector)))

  {{i = 1; j = 10}; i <= 10; {i = i + 1; j = j + 10}} %for% {
    SomeVector[i] = j
  }

    expect_equal(sum(SomeVector), 10*sum(1:length(SomeVector)))
    expect_equal(i, length(SomeVector) + 1)
    expect_equal(j, 110)
})
