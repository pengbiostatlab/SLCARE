data(SimData)

# fit SLCARE with K = 2, formula = "x1 + x2" and default settings for other arguments
model1 <- SLCARE(formula = "x1 + x2", data = SimData, K = 2)

# summary results
summary(model1, digits = 3)

# generate model checking plot
plot(model1, type = "ModelChecking")

# plot estimated cumulative baseline intensity function
plot(model1, type = "mu0")

# generate estimated mean function plot
plot(model1, type = "EstMeans")

# check class membership probabilities of the 6th - 10th subjects in SimData
print(model1, type = "ClassProb")[6:10,]

# check the predicted number of recurrent events of the 6th - 10th subjects in SimData
print(model1, type = "PostPredict")[6:10,]

# check the change in parameter estimates in the last iteration
print(model1, type = "ConvergeLoss")
