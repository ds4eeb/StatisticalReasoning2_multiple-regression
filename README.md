# Activity 9: Statistical reasoning 2: multiple regression


Welcome! This is the second statistical reasoning activity. We will
learn how to implement multiple regression models using the `brms`
package and get more practice interpreting coefficients.

------------------------------------------------------------------------

You will submit one output for this activity:

1.  A **PDF** of a rendered Quarto document with all of your R code.
    Please create a new Quarto document (e.g. don’t use this
    `README.qmd`), include all of the code that appears in this
    document, in addition to adding your own code and **answers to all
    of the questions** in the “Q#” sections. Submit this through
    Gradescope.

*If you have trouble submitting as a PDF, please ask Calvin or Malin for
help. If we still can’t solve it, you can submit the .qmd file instead.*

A reminder: **Please label the code** in your final submission in two
ways: 1) denote your answers to each question using headers that
correspond to the question you’re answering and 2) thoroughly “comment”
your code: remember, this means annotating your code directly by typing
descriptions of what each line does after a `#`. This will help future
you!

------------------------------------------------------------------------

Let’s start by reading in the relevant packages

``` r
library(brms) # for statistics
library(tidyverse) # for data wrangling
library(ggeffects) # for  prediction plots
library(palmerpenguins) # data we'll be using
```

------------------------------------------------------------------------

# 1. Multiple regression

The natural world is complex, and more often than not, there are
multiple variables that influence a given response that we measure.
Think about tomatoes in your garden: you could measure how many tomatoes
your plants produce as a function of how much you water them. But what
about how much sunlight your plants get? Water and sunlight are probably
both important in determining how many tomatoes grow on your plant. To
account for this, we use **multiple regression**.

Multiple regression is when we want to put more than one predictor
variable into our model. The model equation would thus look something
like this:

$$y = intercept + slope_1 \times variable_1 + slope_2 \times variable_2$$

We’re going to demonstrate how to use multiple regression on the palmer
penguins dataset. First, though, we are going to start off with some
refresher on interpreting a simpler linear regression

------------------------------------------------------------------------

## 1.1 Refresh on coefficients

This section: Univariate regression of flipper length ~ body mass

``` r
penguins <- palmerpenguins::penguins

penguins %>% 
  ggplot(aes(x = body_mass_g,
             y = flipper_length_mm)) +
  geom_point() +
  geom_smooth(method = "lm")
```

![](README_files/figure-commonmark/unnamed-chunk-2-1.png)

Run the model

``` r
# latitude model
m.flip.mass <- 
  brm(data = penguins, # Give the model the penguins data
      # Choose a gaussian (normal) distribution
      family = gaussian,
      # Specify the model here. 
      flipper_length_mm ~ 1 + body_mass_g,
      # Here's where you specify parameters for executing the Markov chains
      # We're using similar to the defaults, except we set cores to 4 so the analysis runs faster than the default of 1
      iter = 2000, warmup = 1000, chains = 4, cores = 4,
      # Setting the "seed" determines which random numbers will get sampled.
      # In this case, it makes the randomness of the Markov chain runs reproducible 
      # (so that both of us get the exact same results when running the model)
      seed = 4,
      # Save the fitted model object as output - helpful for reloading in the output later
      file = "output/m.flip.mass")
```

Assess the model fitting by looking at Rhat:

``` r
summary(m.flip.mass)
```

     Family: gaussian 
      Links: mu = identity 
    Formula: flipper_length_mm ~ 1 + body_mass_g 
       Data: penguins (Number of observations: 342) 
      Draws: 4 chains, each with iter = 2000; warmup = 1000; thin = 1;
             total post-warmup draws = 4000

    Regression Coefficients:
                Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    Intercept     136.74      2.00   132.87   140.72 1.00     4466     2908
    body_mass_g     0.02      0.00     0.01     0.02 1.00     4742     2903

    Further Distributional Parameters:
          Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    sigma     6.94      0.26     6.45     7.47 1.00     1775     1712

    Draws were sampled using sampling(NUTS). For each parameter, Bulk_ESS
    and Tail_ESS are effective sample size measures, and Rhat is the potential
    scale reduction factor on split chains (at convergence, Rhat = 1).

Rhat is 1 - looks good!

``` r
plot(m.flip.mass)
```

![](README_files/figure-commonmark/unnamed-chunk-5-1.png)

Looks like the chains converged nicely and the posterior distributions
are smooth. Nice!

Now let’s dig into the actual results by examining the parameter
estimates. We can look at the posterior plots (just above) and look at
the parameter estimates and 95% compatibility intervals from the model
summary:

``` r
summary(m.flip.mass)
```

     Family: gaussian 
      Links: mu = identity 
    Formula: flipper_length_mm ~ 1 + body_mass_g 
       Data: penguins (Number of observations: 342) 
      Draws: 4 chains, each with iter = 2000; warmup = 1000; thin = 1;
             total post-warmup draws = 4000

    Regression Coefficients:
                Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    Intercept     136.74      2.00   132.87   140.72 1.00     4466     2908
    body_mass_g     0.02      0.00     0.01     0.02 1.00     4742     2903

    Further Distributional Parameters:
          Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    sigma     6.94      0.26     6.45     7.47 1.00     1775     1712

    Draws were sampled using sampling(NUTS). For each parameter, Bulk_ESS
    and Tail_ESS are effective sample size measures, and Rhat is the potential
    scale reduction factor on split chains (at convergence, Rhat = 1).

### Q1.1: What is the effect of body mass on flipper length?

1)  What is the magnitude of the relationship between body mass and
    flipper length (e.g. the slope)? Report your result using the units
    of these variables.

2)  Do a quick visual estimate of the slope from the ggplot graph above
    (e.g. take two points and calculate the difference in y divided by
    the difference in x). What is your estimate? To what extent does the
    model’s slope effect match up with your slope from the ggplot?

------------------------------------------------------------------------

### Q1.2: Can we claim that this effect is different from zero?

Using what you learned last class, interpret whether the slope seems to
have a low or high probability of being different from zero. What do you
conclude? Why or why not do you reach this conclusion?

| Let’s also do this calculation more precisely by adding up the fraction of the posterior that is greater than zero. |
|:---|
| \### Q1.3: What is the probability that the slope is different from zero? Look at the output from the code you just ran. |

## 1.2 Additive models

(Need to make a point about geom_smooth() only graphing interactive
models by default)

``` r
penguins %>% 
  ggplot(aes(x = bill_length_mm,
             color = species,
             y = bill_depth_mm)) +
  geom_point() +
  geom_smooth(method = "lm")
```

![](README_files/figure-commonmark/unnamed-chunk-8-1.png)

------------------------------------------------------------------------

# 2. Run multiple regression on your own

------------------------------------------------------------------------

### Render to PDF

When you have finished, remember to pull, stage, commit, and push with
GitHub:

- Pull to check for updates to the remote branch
- Stage your edits (after saving your document!) by checking the
  documents you’d like to push
- Commit your changes with a commit message
- Push your changes to the remote branch

Then submit the well-labeled PDF on Gradescope. Thanks!
