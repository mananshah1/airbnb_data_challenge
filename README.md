# airbnb_data_challenge

This data is from the Airbnb User Pathways Challenge.

With the data we have here, we first see how the normalized metrics - searches per visitor, messages per visitor, booking requests per visitor are influenced by factors like device type, day of week, time of day and session time. We engineer these features from the data that is given to us.

Morning hours have more booking requests per visitor and evening hours have more searched and messages per visitor. Weekdays are associated with more activity from the users. Mobile devices see a lot more activity than Desktop devices, especially with searhces and messages.

In addition to that we also see the distribution of total session time in relation to whether a search, message or booking request is made by the user. Both are right skewed, but when a search, message or booking is made longer session times are observed. 

We also try making predictions for messages sent by using a simple logistic regression. We see clear class imbalance here and explore oversampling techniques to get an even distribution of classes. We also see improvement in AUC with this method. 

We can also explore more classfication algorithms such as Decision Trees, Random Forests, Stochastic Gradient Boosting for predictions. 
