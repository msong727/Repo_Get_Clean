---
title: "Codebook"
output: html_document
---


###  data= train + test w/ SubjectID, activity (labels)

 1. Read data
    1.1 features.txt
    1.2 3 train data sets (X_train.txt, y_train.txt, subject_train.txt)
    1.3 3 test data sets  (X_test.txt,  y_test.txt,  subject_test.txt)
    1.4 activity_labels.txt

 2. Merges the training and the test sets to create one data set "data", and sort by SubjectID and activity


 3. Extracts only the measurements on the mean and standard deviation for each measurement
 4.  Uses descriptive activity names to name the activities in the data set
 5.  Create tidy data set (data.tidy) with column mean by SubjectID and activity
 

### data set "data"" has 10299 rows and 81 columns
   column has same as data_tidy

## data_tidy has 180 rows and 81 comumns

---------

## Columns

```
  1. SubjectID                   
        Subject ID      : 1-30
  2. activity                     
        Activity labels 
  3. tBodyAcc.std...X_mean
  4. tBodyAcc.std...Y_mean
  5. tBodyAcc.std...Z_mean
  6. tGravityAcc.std...X_mean
  7. tGravityAcc.std...Y_mean
  8. tGravityAcc.std...Z_mean
  9. tBodyAccJerk.std...X_mean
 10. tBodyAccJerk.std...Y_mean
 11. tBodyAccJerk.std...Z_mean
 12. tBodyGyro.std...X_mean
 13. tBodyGyro.std...Y_mean
 14. tBodyGyro.std...Z_mean
 15. tBodyGyroJerk.std...X_mean
 16. tBodyGyroJerk.std...Y_mean
 17. tBodyGyroJerk.std...Z_mean
 18. tBodyAccMag.std.._mean
 19. tGravityAccMag.std.._mean
 20. tBodyAccJerkMag.std.._mean
 21. tBodyGyroMag.std.._mean
 22. tBodyGyroJerkMag.std.._mean
 23. fBodyAcc.std...X_mean
 24. fBodyAcc.std...Y_mean
 25  fBodyAcc.std...Z_mean
 26. fBodyAccJerk.std...X_mean
 27  fBodyAccJerk.std...Y_mean
 28. fBodyAccJerk.std...Z_mean
 29. fBodyGyro.std...X_mean
 30. fBodyGyro.std...Y_mean
 31. fBodyGyro.std...Z_mean
 32. fBodyAccMag.std.._mean
 33. fBodyBodyAccJerkMag.std.._mean
 34. fBodyBodyGyroMag.std.._mean
 35. fBodyBodyGyroJerkMag.std.._mean
 36. tBodyAcc.mean...X_mean
 37. tBodyAcc.mean...Y_mean
 38. tBodyAcc.mean...Z_mean
 39. tGravityAcc.mean...X_mean
 40. tGravityAcc.mean...Y_mean
 41. tGravityAcc.mean...Z_mean
 42. tBodyAccJerk.mean...X_mean
 43. tBodyAccJerk.mean...Y_mean
 44. tBodyAccJerk.mean...Z_mean
 45. tBodyGyro.mean...X_mean
 46. tBodyGyro.mean...Y_mean
 47. tBodyGyro.mean...Z_mean
 48. tBodyGyroJerk.mean...X_mean
 49. tBodyGyroJerk.mean...Y_mean
 50. tBodyGyroJerk.mean...Z_mean
 51. tBodyAccMag.mean.._mean
 52. tGravityAccMag.mean.._mean
 53. tBodyAccJerkMag.mean.._mean
 54. tBodyGyroMag.mean.._mean
 55. tBodyGyroJerkMag.mean.._mean
 56. fBodyAcc.mean...X_mean
 57. fBodyAcc.mean...Y_mean
 58. fBodyAcc.mean...Z_mean
 59. fBodyAcc.meanFreq...X_mean
 60. fBodyAcc.meanFreq...Y_mean
 61. fBodyAcc.meanFreq...Z_mean
 62. fBodyAccJerk.mean...X_mean
 63. fBodyAccJerk.mean...Y_mean
 64. fBodyAccJerk.mean...Z_mean
 65. fBodyAccJerk.meanFreq...X_mean
 66. fBodyAccJerk.meanFreq...Y_mean
 67. fBodyAccJerk.meanFreq...Z_mean
 68. fBodyGyro.mean...X_mean
 69. fBodyGyro.mean...Y_mean
 70. fBodyGyro.mean...Z_mean
 71. fBodyGyro.meanFreq...X_mean
 72. fBodyGyro.meanFreq...Y_mean
 73. fBodyGyro.meanFreq...Z_mean
 74. fBodyAccMag.mean.._mean
 75. fBodyAccMag.meanFreq.._mean
 76. fBodyBodyAccJerkMag.mean.._mean
 77. fBodyBodyAccJerkMag.meanFreq.._mean
 78. fBodyBodyGyroMag.mean.._mean
 79. fBodyBodyGyroMag.meanFreq.._mean
 80. fBodyBodyGyroJerkMag.mean.._mean
 81. fBodyBodyGyroJerkMag.meanFreq.._mean
```

