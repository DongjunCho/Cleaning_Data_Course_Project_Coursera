## Getting and Cleaning Data Project

## Purpose of the Project
  The purpose of this project is to demonstrate your ability to collect, work with, and clean a data set.

## About Data and Dataset
  Data is from [UCI Machine Learning Repository](http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones).
  Name of the Dataset is getdata-projectiles-UCI HAR Dataset.
  In the dataset files, it contains two folder (train, test) and  text files (activity labels, features, features info, and README)
  In the train and test folder, it contains data named subject_train, x_train, y_train, subject_test, x_test, y_test.
  Based on [README.md](https://github.com/DongjunCho/Cleaning_Data_Course_Project_Coursera/blob/master/README.md), our job is to merge those data and measure the values.

## Items under text files : activity labels
1. WALKING
2. WALKING_UPSTAIRS
3. WALKING_DOWNSTAIRS
4. SITTING
5. STANDING
6. LAYING

## Column Names
1. "Activity"
2. "subjectId"
3. "TimeBodyAccelerationMean-X"
4. "TimeBodyAccelerationMean-Y"
5. "TimeBodyAccelerationMean-Z"
6. "TimeBodyAccelerationSTD-X"
7. "TimeBodyAccelerationSTD-Y"
8. "TimeBodyAccelerationSTD-Z"
9. "TimeGravityAccelerationMean-X"
10. "TimeGravityAccelerationMean-Y"
11. "TimeGravityAccelerationMean-Z"
12. "TimeGravityAccelerationSTD-X"
13. "TimeGravityAccelerationSTD-Y"
14. "TimeGravityAccelerationSTD-Z"
15. "TimeBodyAccelerationJerkMean-X"
16. "TimeBodyAccelerationJerkMean-Y"
17. "TimeBodyAccelerationJerkMean-Z"
18. "TimeBodyAccelerationJerkSTD-X"
19. "TimeBodyAccelerationJerkSTD-Y"
20. "TimeBodyAccelerationJerkSTD-Z"
21. "TimeBodyGyroscopeMean-X"
22. "TimeBodyGyroscopeMean-Y"
23. "TimeBodyGyroscopeMean-Z"
24. "TimeBodyGyroscopeSTD-X"
25. "TimeBodyGyroscopeSTD-Y"
26. "TimeBodyGyroscopeSTD-Z"
27. "TimeBodyGyroscopeJerkMean-X"
28. "TimeBodyGyroscopeJerkMean-Y"
29. "TimeBodyGyroscopeJerkMean-Z"
30. "TimeBodyGyroscopeJerkSTD-X"
31. "TimeBodyGyroscopeJerkSTD-Y"
32. "TimeBodyGyroscopeJerkSTD-Z"
33. "TimeBodyAccelerationMagnitudeMean"
34. "TimeBodyAccelerationMagnitudeSTD"
35. "TimeGravityAccelerationMagnitudeMean"
36. "TimeGravityAccelerationMagnitudeSTD"
37. "TimeBodyAccelerationJerkMagnitudeMean"
38. "TimeBodyAccelerationJerkMagnitudeSTD"
39. "TimeBodyGyroscopeMagnitudeMean"
40. "TimeBodyGyroscopeMagnitudeSTD"
41. "TimeBodyGyroscopeJerkMagnitudeMean"
42. "TimeBodyGyroscopeJerkMagnitudeSTD"
43. "FrequencyBodyAccelerationMean-X"
44. "FrequencyBodyAccelerationMean-Y"
45. "FrequencyBodyAccelerationMean-Z"
46. "FrequencyBodyAccelerationSTD-X"
47. "FrequencyBodyAccelerationSTD-Y"
48. "FrequencyBodyAccelerationSTD-Z"
49. "FrequencyBodyAccelerationMeanFrequency-X"
50. "FrequencyBodyAccelerationMeanFrequency-Y"
51. "FrequencyBodyAccelerationMeanFrequency-Z"
52. "FrequencyBodyAccelerationJerkMean-X"
53. "FrequencyBodyAccelerationJerkMean-Y"
54. "FrequencyBodyAccelerationJerkMean-Z"
55. "FrequencyBodyAccelerationJerkSTD-X"
56. "FrequencyBodyAccelerationJerkSTD-Y"
57. "FrequencyBodyAccelerationJerkSTD-Z"
58. "FrequencyBodyAccelerationJerkMeanFrequency-X"
59. "FrequencyBodyAccelerationJerkMeanFrequency-Y"
60. "FrequencyBodyAccelerationJerkMeanFrequency-Z"
61. "FrequencyBodyGyroscopeMean-X"
62. "FrequencyBodyGyroscopeMean-Y"
63. "FrequencyBodyGyroscopeMean-Z"
64. "FrequencyBodyGyroscopeSTD-X"
65. "FrequencyBodyGyroscopeSTD-Y"
66. "FrequencyBodyGyroscopeSTD-Z"
67. "FrequencyBodyGyroscopeMeanFrequency-X"
68. "FrequencyBodyGyroscopeMeanFrequency-Y"
69. "FrequencyBodyGyroscopeMeanFrequency-Z"
70. "FrequencyBodyAccelerationMagnitudeMean"
71. "FrequencyBodyAccelerationMagnitudeSTD"
72. "FrequencyBodyAccelerationMagnitudeMeanFrequency"
73. "FrequencyBodyAccelerationJerkMagnitudeMean"
74. "FrequencyBodyAccelerationJerkMagnitudeSTD"
75. "FrequencyBodyAccelerationJerkMagnitudeMeanFrequency"
76. "FrequencyBodyGyroscopeMagnitudeMean"
77. "FrequencyBodyGyroscopeMagnitudeSTD"
78. "FrequencyBodyGyroscopeMagnitudeMeanFrequency"
79. "FrequencyBodyGyroscopeJerkMagnitudeMean"
80. "FrequencyBodyGyroscopeJerkMagnitudeSTD"
81. "FrequencyBodyGyroscopeJerkMagnitudeMeanFrequency"
  
