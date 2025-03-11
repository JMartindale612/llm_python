# Deep Learning with R - Chapter 2 ----

library(tensorflow)
library(keras)
# install_tensorflow()

## Mnist example

## 2.1 The MNIST data
mnist <- dataset_mnist()

# 10,000 observations
# x = 28x28 matrix (pi)
# y = numerical value, which digit

train_images <- mnist$train$x
train_labels <- mnist$train$y 
test_images <- mnist$test$x 
test_labels <- mnist$test$y

## 2.2 The network architecture
model <- keras_model_sequential(list(
  layer_dense(units = 512, activation = "relu"), 
  layer_dense(units = 10, activation = "softmax") 
))

## 2.3 The compilation step
compile(model, 
        optimizer = "rmsprop", 
        loss = "sparse_categorical_crossentropy", 
        metrics = "accuracy")

## 2.4 Preparing the image data
train_images <- array_reshape(train_images, c(60000, 28 * 28)) 
train_images <- train_images / 255 
test_images <- array_reshape(test_images, c(10000, 28 * 28)) 
test_images <- test_images / 255

## 2.5 Fitting the model
fit(model, train_images, train_labels, epochs = 5, batch_size = 128)

## 2.6 Using the model to make predictions
test_digits <- test_images[1:10, ] 
predictions <- predict(model, test_digits) 
str(predictions)

# Subsetting the predictions
predictions[1, ]
which.max(predictions[1, ])

predictions[1, 8]
test_labels[1]

## 2.7 Evaluating the model on new data
metrics <- evaluate(model, test_images, test_labels) 
metrics["accuracy"]