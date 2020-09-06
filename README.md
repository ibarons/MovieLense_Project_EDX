# MovieLense_Project_EDX

In 2006 Netflix launched a challenge to beat its movie recommendation algorithm by at least 10% of its Root
Mean Squared Error (RMSE). This was achieved in 2009 when the BellKor’s Pragmatic Chaos surpass the
10% barrier. Since then, the business of movie streaming has expanded greatly, with multiple new emerging
platforms, all of which offer thousands of films to their clients. In this new era of streaming platforms offering
enormous amount of movie and series, recommendation mechanism become a key stone for the fidelization
of clients.

In this scenario, new and more sophisticated movie recommendation algorithms have been developed to
improve user experience. The current project aims at humbly contributing to this task by providing a model
able to improve a RMSE of 0.8649.

The data used was extracted from http://files.grouplens.org/datasets/movielens/ml-10m.zip and contained
10,000,054 ratings for 10,677 movies with 69,878 users. During this report we referred to data set when
we speak of the group of data tables. Data table is a single data structure composed of rows and columns
which we referred indistinctly as data or data table. The extracted data was divided into the raw data set,
composed of the Training (a.k.a edx) and Hold-out (a.k.a validation) data tables to follow a Supervised
Learning approach.

The model aimed at the prediction of movie ratings and incorporated five predictors, each of them defined
the deviations form the overall average rating produced by the influence of the independent variables. These
predictors were further regularized and tuned using bootstrap solutions over the training data set. This
allowed to reduce over-training and produce a more smoothed model to fit the hold-out data. The model
was mathematically reduced to a linear model consisting on the summation of the average rating (outcome)
and its identified effects (predictors).

The final solution to predict the outcome rating reached a RMSE of 0.8631 over the hold-out set, improving
in 0.0018 the set objective of 0.8649.
