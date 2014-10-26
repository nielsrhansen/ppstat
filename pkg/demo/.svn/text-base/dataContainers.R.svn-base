################################################################################
### Simple examples on the use of classes 'ContinuousProcess',
### 'MarkedPointProcess' and 'JumpProcess'. 
################################################################################

## First we load the example data. The data sets are 'contExam' of class
## 'ContinuousProcess', 'pointExam' of class 'MarkedPointProcess' and
## 'jumpExam' of class 'JumpProcess'.

data(example)

### Showing data structure. 

contExam

### Summarizing the data.

summary(contExam)

## Plotting data.

plot(contExam)

## Subsetting data.

subset(contExam, id %in% c("C","D","F"))
subset(contExam, foo > 0)
subset(contExam, gender == "F")

## Plotting a subset.

plot(subset(contExam, subset = (time > 4 & gender == "F"), select = foo))


## Modifying plot using ggplot2.

plot(subset(contExam, id == "A")) + geom_point()
plot(contExam) + facet_null()

## More sophisticated plots can be produced, e.g. different uses of
## faceting and grouping.

plot(contExam) + facet_grid(variable ~ ., scale = "free_y")

## To do this depending on unit-specific variables we must specify an
## additional argument to the plot such that these variables are
## included in the plot data.

plot(contExam, allUnitData = TRUE) +
  facet_grid(variable ~ gender, scale = "free_y")


## Subsetting to non-contiguous subsets of the time axis is
## possible, but 'plot' assumes a contiguous x-axis and interpolates
## the graph.

plot(subset(contExam, id == "A" & (time < 2 | time > 4)))

## Factor columns are plotting using a different aesthetic mapping.

plot(factExam)

## The default is to drop one level (the first), but this can be
## changed or bypassed by setting the 'dropLevels argument.

plot(factExam, dropLevels = 2)
plot(factExam, dropLevels = NULL)

## Summarizing a MarkedPointProcess object gives additional
## informations on the number of points for each unit and each mark. 

summary(pointExam)

## A combined plot for unit "A". 

plot(subset(pointExam, id =="A"))

## Plotting the point process data only.

plot(pointExam[, -(1:3)])

## Or a slightly different version.

plot(pointExam[, -(1:3)], y = "id") + facet_null()

## Plotting the continuous process data only.

plot(pointExam[, -c(4,5)])

## or 

plot(as(pointExam, "ContinuousProcess"))

## Some modifications of the plot.

plot(pointExam, allUnitData = TRUE) + facet_grid(.~gender)
plot(pointExam, y = "id", allUnitData = TRUE) + facet_grid(variable~gender, scale = "free_y")
plot(pointExam, y = "foo") 
plot(pointExam, y = "@top", allUnitData = TRUE) + facet_grid(.~gender)
plot(pointExam, y = "@bottom", allUnitData = TRUE) + facet_grid(gender~., scales = "free_y")
plot(pointExam, y = "id") + facet_null()

## A 'JumpProcess' data set has points with the same marks as the variable names
## in the 'ContinuousProcess' part of the data set. In addition, to each point
## there is a size of the jump associated.

jumpExam
plot(jumpExam)

## We can again use ggplot2 for modifications

plot(jumpExam) + facet_grid(variable ~ .) + aes(color = id)

## Removing objects.
rm(contExam)
rm(pointExam)
rm(jumpExam)


