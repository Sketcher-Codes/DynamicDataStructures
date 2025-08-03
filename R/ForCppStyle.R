`%for%` <- function(Iterator, Payload){
  #SubPayloadText = capture.output(str(substitute(Payload)))[1]
  SubPayloadText = substitute(Payload)
  SPLen = length(SubPayloadText)
  SubPayloadText = paste0(trimws(capture.output(SubPayloadText)), collapse = "\n")
  SubPayloadText = substring(SubPayloadText, 2, nchar(SubPayloadText) - 2) #Remove the final "}"

  SubIterator = substitute(Iterator)

  Initialiser = paste0(trimws(capture.output(SubIterator[2])), collapse = "\n")
  Initialiser = substring(Initialiser, 1, nchar(Initialiser)-2)


  Condition = paste0(trimws(capture.output(SubIterator[3])), collapse = "\n")
  Condition = substring(Condition, 1, nchar(Condition)-2)


  Action = paste0(trimws(capture.output(SubIterator[4])), collapse = "\n")
  Action = substring(Action, 1, nchar(Action)-2)

  LoopFunction = paste0(
    Initialiser,
    ";while(",Condition,"){",
    SubPayloadText,"\n",
    Action,"}"
  )

  eval(parse(text = LoopFunction), envir = parent.frame())
  return(invisible())
}


# SomeVector = 1:10
#
# Test = (
#
# {{i = 1; if(i == 2){i = -1}else{i = 7}; j = 10}; i <= 10; {i = i + 1; j = j + 10}} %for% {
#   SomeVector[i] = j
#   cat(i)
#   cat(j)
# }
#
# )
#
# pT = parse(text = Test)
# eval(pT)
