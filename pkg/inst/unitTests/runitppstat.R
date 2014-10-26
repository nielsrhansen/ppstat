library(ppstat)

data(example)

testPPM <- pointProcessModel(ALPHA ~ gender,
                             data = pointExam,
                             family = Hawkes(),
                             support = 2)
summary(testPPM)

testPPM <- pointProcessModel(ALPHA ~ gender-1,
                             data = pointExam,
                             family = Hawkes(),
                             support = 2)
summary(testPPM)

testPPM <- pointProcessModel(ALPHA ~ id-1,
                             data = pointExam,
                             family = Hawkes(),
                             support = 2)

summary(testPPM)
