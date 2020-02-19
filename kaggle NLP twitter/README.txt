!!!!!!!! WORK IN PROGRESS !!!!!!!!
This project is about the kaggle competition Real or Not? NLP with Disaster Tweets : Predict which Tweets are about real disasters and which ones are not
https://www.kaggle.com/c/nlp-getting-started

This projects uses scikit-learn python library to implement many ML models : logistic regression, naive bayes, support vector machine, random forest, decision tree, stochastic gradient descent.
Also different options for data pre-processing are evaluated: keep/remove stop words, stemming, lemmatization, vectorization (binary, word count, tf-idf), n-grams (1 to 3).

A brute-force grid search optimization approach is taken to search for the best combination of pre-processing and hyper parameter values.

Next steps: 
 - try more models
 - Evaluate the best way to combine models (soft value predictions (probabilities), hard value predictions (majority vote))
 - try other libraries than scikit e.g. keras, TensorFlow to get new models (NN, LSTM...)
 
