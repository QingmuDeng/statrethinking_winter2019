# Note
Geocentric model works quite accurately, but it's wrong. (Early form of fourier series.)
Similarly, regression models may provide accurate descriptions, but their underlying mechanism can be wrong.

## Linear regression
- simple statistical golems
    - model of mean and variance of gaussian distributed measure
    - mean as additive combination of weighted variables
    - constant variance

Why normal? 
1. they are easy to calculate with
2. Ontological perspective: they are common in nature. Gaussian distributions are born from adding together fluctuations. Fluctuations tend to cancel, thus mean=0, even if the fluctuations may not be symmetric. Unfortunately, the commonplace nature of normal distribution provide little information to infer the underlying process.
3. Epistemological perspective: know only mean and variance; the least surprising and most conservative (maximum entropy) distribution is Gaussian; nature likes maximum entropy.

## Language for modeling
Define the variables
```
y_i ~ Normal(u_i, sigma)
u_i = beta*x_i
beta ~ Normal(0, 10)
sigma ~ Exponential(1)
x_i ~ Normal(0, 1)
```
## Quadratic Approximation
- Approximate posterior as Gaussian
- Can estimate with twothings
    - peak of posterior, maximum a posteriori
    - standard deviations & correlations of parameters (covariance matrix)
- Climb the hill (gradient ascent) and find the spread along the side of the peak
- with flat priors, same as conventional maximum likelihodd estimation

## Making a line
Adding a predictor variable such as weight to correlate it with height.