Activity 10: Statistical reasoning 2: multiple regression
================

Welcome! This is the second statistical reasoning activity. We will
learn how to implement multiple regression models using the `brms`
package, specifically additive models, and get more practice
interpreting coefficients.

The goals of this activity are to 1) understand how to interpret output
from an additive multiple regression model and 2) understand how adding
additional predictor variables to a model may change your interpretation
of other predictor variables.

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

Let’s run a univariate linear regression of `flipper length ~ body mass`
across all penguins in the dataset.

First, let’s fetch and plot the data.

``` r
penguins <- palmerpenguins::penguins
```

``` r
penguins %>% 
  ggplot(aes(x = body_mass_g,
             y = flipper_length_mm)) +
  geom_point() +
  # Add a geom_smooth with an "lm" line for visualization
  geom_smooth(method = "lm")
```

![](README_files/figure-commonmark/unnamed-chunk-3-1.png)

------------------------------------------------------------------------

Next, let’s run the model, just like in last activity.

``` r
# flipper length by body mass model
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

------------------------------------------------------------------------

Next, let’s assess the model fitting by looking at Rhat: Rhat is 1 -
looks good!

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

------------------------------------------------------------------------

Next up for assessing the model, let’s look at the posterior
distributions and model chains. Remember, we’re looking for three
things:

1.  Are the posterior samples on the left each a smooth distribution,
    with one clean peak, or do they have multiple clear peaks? The
    latter is a bad sign. They look good in this case.
2.  Are the four chains on the overlapping each other, or are they
    clearly separate? The latter is a bad sign. We again look good in
    this case.
3.  Are the four chains flat, or is there a clear trend up or down? The
    latter is a bad sign. We again look good in this case.

``` r
plot(m.flip.mass)
```

![](README_files/figure-commonmark/unnamed-chunk-6-1.png)

------------------------------------------------------------------------

Now let’s dig into the actual results by examining the parameter
estimates. We can look at the posterior plots (above) and look at the
parameter estimates and 95% compatibility intervals from the model
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

2)  Do a quick visual estimate of the slope from the ggplot graph at the
    beginning (e.g. take two points and calculate the difference in y
    divided by the difference in x). What is your estimate? To what
    extent does the model’s slope effect match up with your slope from
    the ggplot?

------------------------------------------------------------------------

### Q1.2: Can we claim that this effect is different from zero?

Using what you learned last class, interpret whether the slope seems to
have a low or high probability of being different from zero. What do you
conclude? Why or why not do you reach this conclusion?

------------------------------------------------------------------------

### Q1.3: What is the probability that the slope is different from zero?

Let’s also do this calculation more precisely by adding up the fraction
of the posterior that is greater than zero.

``` r
as_draws_df(m.flip.mass) |> # extract the posterior samples from the model estimate
  select(b_body_mass_g) |> # pull out the latitude samples from all 4 chains. we'll get a warning that we can ignore.
  summarize(p_slope_greaterthan_zero = sum(b_body_mass_g > 0)/length(b_body_mass_g))
```

    # A tibble: 1 × 1
      p_slope_greaterthan_zero
                         <dbl>
    1                        1

Look at the output from the code you just ran and report the probability
that the slope is different from zero.

------------------------------------------------------------------------

## 1.2 Additive models

Now it’s time to get into multiple regression. In particular, we are
going to use additive multiple regression models. Writing an additive
model tells the model that the 2+ predictor variables don’t influence
one another’s effect on the response (more on an alternate to additive
models below).

Let’s start by plotting a new set of variables. Now we’re asking: What
is the effect of bill length on bill depth among the penguins in the
dataset?

Let’s plot the data first

``` r
penguins %>% 
  ggplot(aes(x = bill_length_mm,
             y = bill_depth_mm)) +
  geom_point() +
  # Let's add in a basic lm just to visualize
  geom_smooth(method = "lm")
```

![](README_files/figure-commonmark/unnamed-chunk-9-1.png)

### Q1.4: Does bill length seem to have an effect on bill depth?

Qualitatively, does bill length seem to have a positive, negative, or no
effect on bill depth?

------------------------------------------------------------------------

### Run a normal univariate model

Now let’s test this by running a normal univariate model with these two
variables:

``` r
# flipper length by body mass model
m.depth.length <- 
  brm(data = penguins, # Give the model the penguins data
      # Choose a gaussian (normal) distribution
      family = gaussian,
      # Specify the model here. 
      bill_depth_mm ~ 1 + bill_length_mm,
      # Here's where you specify parameters for executing the Markov chains
      # We're using similar to the defaults, except we set cores to 4 so the analysis runs faster than the default of 1
      iter = 2000, warmup = 1000, chains = 4, cores = 4,
      # Setting the "seed" determines which random numbers will get sampled.
      # In this case, it makes the randomness of the Markov chain runs reproducible 
      # (so that both of us get the exact same results when running the model)
      seed = 4,
      # Save the fitted model object as output - helpful for reloading in the output later
      file = "output/m.depth.length")
```

------------------------------------------------------------------------

### Assess and interpret the univariate model

``` r
summary(m.depth.length)
```

     Family: gaussian 
      Links: mu = identity 
    Formula: bill_depth_mm ~ 1 + bill_length_mm 
       Data: penguins (Number of observations: 342) 
      Draws: 4 chains, each with iter = 2000; warmup = 1000; thin = 1;
             total post-warmup draws = 4000

    Regression Coefficients:
                   Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    Intercept         20.89      0.86    19.24    22.58 1.00     3946     2774
    bill_length_mm    -0.09      0.02    -0.12    -0.05 1.00     3994     2771

    Further Distributional Parameters:
          Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    sigma     1.93      0.08     1.79     2.08 1.00     4238     2987

    Draws were sampled using sampling(NUTS). For each parameter, Bulk_ESS
    and Tail_ESS are effective sample size measures, and Rhat is the potential
    scale reduction factor on split chains (at convergence, Rhat = 1).

``` r
plot(m.depth.length)
```

![](README_files/figure-commonmark/unnamed-chunk-12-1.png)

The Rhat, posterior distributions, and chains look good, so we can say
that our model ran well.

------------------------------------------------------------------------

### Q1.5 Write a couple “results section” sentences with your conclusions about whether bill length is associated with bill depth given this model.

Include the estimate for the slope and it’s biological interpretation,
as well as whether or not this slope seems different from zero (and why
you think that).

------------------------------------------------------------------------

### Run the same model with species as a second predictor

Time to do multiple regression!! From our past experience, we know that
there’s more to this penguin story. In fact, there are three separate
species of penguins here. Let’s modify our question to be:

*Does bill length influence bill depth and does bill depth differ with
penguin species?*

To answer this question, we need to turn to multiple regression, where
we will add in `species` as a variable to our model. We are simply going
to add in the `species` column to our model from above. We are also
going to change the “intercept” to 0; this will allow for a bit easier
interpretation of the effect of the categorical variable of `species.`

``` r
# flipper length by body mass model
m.depth.length.species <- 
  brm(data = penguins, # Give the model the penguins data
      # Choose a gaussian (normal) distribution
      family = gaussian,
      # Specify the model here. 
      bill_depth_mm ~ 0 + bill_length_mm + species,
      # Here's where you specify parameters for executing the Markov chains
      # We're using similar to the defaults, except we set cores to 4 so the analysis runs faster than the default of 1
      iter = 2000, warmup = 1000, chains = 4, cores = 4,
      # Setting the "seed" determines which random numbers will get sampled.
      # In this case, it makes the randomness of the Markov chain runs reproducible 
      # (so that both of us get the exact same results when running the model)
      seed = 4,
      # Save the fitted model object as output - helpful for reloading in the output later
      file = "output/m.depth.length.species")
```

------------------------------------------------------------------------

### Q1.6 Assess the model: did it run correctly?

Assess the model using Rhat, the posterior distributions, and the plot
of chains: did it run well?

``` r
summary(m.depth.length.species)
```

     Family: gaussian 
      Links: mu = identity 
    Formula: bill_depth_mm ~ 0 + bill_length_mm + species 
       Data: penguins (Number of observations: 342) 
      Draws: 4 chains, each with iter = 2000; warmup = 1000; thin = 1;
             total post-warmup draws = 4000

    Regression Coefficients:
                     Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    bill_length_mm       0.20      0.02     0.16     0.23 1.00      686      796
    speciesAdelie       10.65      0.71     9.22    12.07 1.01      685      794
    speciesChinstrap     8.73      0.89     6.93    10.51 1.00      693      840
    speciesGentoo        5.55      0.86     3.80     7.27 1.00      686      784

    Further Distributional Parameters:
          Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    sigma     0.96      0.04     0.89     1.03 1.00     1254     1544

    Draws were sampled using sampling(NUTS). For each parameter, Bulk_ESS
    and Tail_ESS are effective sample size measures, and Rhat is the potential
    scale reduction factor on split chains (at convergence, Rhat = 1).

``` r
plot(m.depth.length.species)
```

![](README_files/figure-commonmark/unnamed-chunk-15-1.png)

------------------------------------------------------------------------

### Interpret an additive model

Ok, now let’s dig into the model output and interpret our first additive
multiple regression.

``` r
summary(m.depth.length.species)
```

     Family: gaussian 
      Links: mu = identity 
    Formula: bill_depth_mm ~ 0 + bill_length_mm + species 
       Data: penguins (Number of observations: 342) 
      Draws: 4 chains, each with iter = 2000; warmup = 1000; thin = 1;
             total post-warmup draws = 4000

    Regression Coefficients:
                     Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    bill_length_mm       0.20      0.02     0.16     0.23 1.00      686      796
    speciesAdelie       10.65      0.71     9.22    12.07 1.01      685      794
    speciesChinstrap     8.73      0.89     6.93    10.51 1.00      693      840
    speciesGentoo        5.55      0.86     3.80     7.27 1.00      686      784

    Further Distributional Parameters:
          Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    sigma     0.96      0.04     0.89     1.03 1.00     1254     1544

    Draws were sampled using sampling(NUTS). For each parameter, Bulk_ESS
    and Tail_ESS are effective sample size measures, and Rhat is the potential
    scale reduction factor on split chains (at convergence, Rhat = 1).

------------------------------------------------------------------------

#### The effect of species (a categorical variable)

You’ll notice that added to the table is not a single “Species”
coefficient estimate, but three coefficient estimates: one for each
factor in our `species` column. Our model is a model of
`bill depth ~ bill length` with *three separate intercepts*, one for
each of the three species. We haven’t really talked about intercepts in
the activities, but intercepts are the y-value when the x-value equals
zero. This isn’t super biologically meaningful in that sense (because
the bill width of a penguin will almost certainly not be 10.65 when the
bill length equals 0…), BUT, in an additive model, it tells us the
relative differences between the different factor levels across all
values of your other variable. *In other words, it tells us the
differences in bill width between species.*

For instance, the value for Gentoo is 5.55, for Chinstrap it’s 8.73, and
for Adelie it’s 10.64. Let’s directly compare Adelie and Chinstrap. This
means that for any given bill length, Adelie penguins have
`10.64 - 8.73 =` 1.91mm deeper bills than Chinstraps. Similarly, Adelie
penguins have `10.64 - 5.55 =` 5.09mm deeper bills than Gentoo penguins.

Let’s graph our data using ggplot to get a sense of this difference, but
this time, with penguin species assigned to color.

``` r
penguins %>% 
  ggplot(aes(x = bill_length_mm,
             y = bill_depth_mm,
             color = species)) +
  geom_point() +
  geom_smooth(method = "lm")
```

![](README_files/figure-commonmark/unnamed-chunk-17-1.png)

#### Q1.7 Visually measure differences in bill depth between species

Visually measure differences in bill depth between species; choose a
region of the x-axis which has data for all three species (e.g. around
45mm). Estimate the difference in bill depth (the y-axis) between a)
Adelie and Chinstrap and b) Adelie and Gentoo. Are those differences
consistent with the differences that we calculated from the model?

------------------------------------------------------------------------

#### The effect of bill length

Now let’s move on to interpreting the slope, which in this case is the
effect of `bill length`.

The model output gave us a slope estimate of 0.20, which we interpret
as: for every 1mm of bill length, bill depth increases by 0.20mm. The
95% credible intervals are between 0.16 and 0.23, indicating that we can
be confident that zero is not consistent with our model’s estimate of
slope.

**This is an additive model**, which means that that the effect of one
predictor is treated as independent of other predictors. Rephrased, this
means that this model structure assumes that the effect of bill length
on bill depth *will not differ between species*. If we think of this
graphically, it means that this model forces each species to have the
exact same slope. In our example, this seems like a fine assumption: in
the graph above, notice that the slopes that geom_smooth() puts on the
graph are basically parallel (parallel lines = equal slopes). If we feel
that the slope should actually be different for each species, then we
would use an *interactive model* instead of an *additive model*, but
don’t worry about that - we’ll get to that later in the quarter!

------------------------------------------------------------------------

### Q1.8 Describe how your conclusions changed between running the univariate regression and the multiple regression

How did adding `species` change the results of our model? Fill in the
blanks in the paragraph below and add a couple sentences at the end
explaining how your interpretation of the effect of `bill length` on
`bill depth` changed upon adding `species`.

*The results from the univariate regression of
`bill depth ~ bill length` indicate that the effect of bill length on
bill depth was a change of \_\_\_\_mm of bill depth for every 1mm of
bill length. When we add in `species`, the results from the multivariate
regression of `bill depth ~ bill length + species` indicate that the
effect of bill length on bill depth was a change of \_\_\_\_mm of bill
depth for every 1mm of bill length.* (Remember to add in a few extra
sentences here)

This part is super important! If you’re confused about what the point
here is or if you or your partner would like additional explanation,
please ask Calvin or Malin :)

------------------------------------------------------------------------

#### Plot predictions

Last activity you were asked to plot the model predictions using the
`predict_response()` function. Doing this for an additive model changes
things up a bit. Let’s try it:

``` r
# compatibility interval. the shows uncertainty in the average response.
confm.depth.length.species <- predict_response(m.depth.length.species)

plot(confm.depth.length.species, show_data = TRUE)
```

    $bill_length_mm

![](README_files/figure-commonmark/unnamed-chunk-18-1.png)


    $species

![](README_files/figure-commonmark/unnamed-chunk-18-2.png)

Note that it gives you two graphs: One for the slope estimate, one for
each species’ intercept estimates. The slope is only plotted for one
species though. Let’s take a look at the function output:

``` r
confm.depth.length.species
```

    $bill_length_mm
    # Predicted values of bill_depth_mm

    bill_length_mm | Predicted |       95% CI
    -----------------------------------------
                30 |     16.60 | 16.25, 16.96
                35 |     17.59 | 17.39, 17.80
                40 |     18.59 | 18.43, 18.74
                45 |     19.58 | 19.31, 19.85
                50 |     20.58 | 20.13, 21.01
                55 |     21.57 | 20.95, 22.18
                60 |     22.57 | 21.76, 23.36

    Adjusted for:
    * species = Adelie

    $species
    # Predicted values of bill_depth_mm

    species   | Predicted |       95% CI
    ------------------------------------
    Adelie    |     19.37 | 19.13, 19.60
    Chinstrap |     17.45 | 17.17, 17.74
    Gentoo    |     14.27 | 14.06, 14.48

    Adjusted for:
    * bill_length_mm = 43.92

    attr(,"class")
    [1] "ggalleffects" "list"        
    attr(,"model.name")
    [1] "m.depth.length.species"

The output specifies that it plotted the slope
`Adjusted for: * species = Adelie` and it plotted the species intercepts
`Adjusted for: * bill_length_mm = 43.92`. This means that it just chose
a value of each predictor to report the OTHER predictor’s output at. You
can adjust which value it chooses with the `condition =` argument.

We are going to try and find a better way to plot `brm` model output for
next time, so don’t worry too much about this right now!

We can do this for the prediction interval too

``` r
# prediction interval. this shows uncertainty in the data around the average response.
confm.depth.length.species <- predict_response(m.depth.length.species, interval = 'prediction')
plot(confm.depth.length.species, show_data = TRUE)
```

    $bill_length_mm

![](README_files/figure-commonmark/unnamed-chunk-20-1.png)


    $species

![](README_files/figure-commonmark/unnamed-chunk-20-2.png)

------------------------------------------------------------------------

# 2. Run multiple regression on Iris data

Now it’s your turn. We’re going to use the Iris flower trait data to ask
the question: Does flower petal length vary with sepal length and with
species of iris?

``` r
iris <- datasets::iris
```

------------------------------------------------------------------------

### Q2.1 Make a hypothesis

*Before* you look at the data, what direction of an effect do you
expect? Why? Also, do you think species will be an important factor?
Please write 1-2 sentences.

------------------------------------------------------------------------

### Q2.2 Graph the data

Graph petal length on the y-axis and include sepal length on the x and
species of iris as the point color.

------------------------------------------------------------------------

### Q2.3 Set up and run a model

Set up and run an additive multiple regression model that answers the
question above.

------------------------------------------------------------------------

### Q2.4 Assess the model

Assess whether the model ran correctly by looking at R hat, the chains,
and the posterior distributions. Describe your thought process about
whether the model ran correctly in 1-2 sentences.

------------------------------------------------------------------------

### Q2.5 Interpret the model

Interpret your model by answering:

1.  What is the effect of sepal length on petal length? Remember to
    describe the effect using the units to make it biologically
    meaningful. Is the effect reasonably different from zero? How do you
    know?
2.  How does petal length vary with species?

------------------------------------------------------------------------

### Q2.6 Write a small results paragraph

Including the information from Q2.6, write 2-3 sentences as if you were
writing the results section of a scientific paper. Include a conclusion
sentence that summarizes your finding.

------------------------------------------------------------------------

### Bonus: See how your result changes if you leave `Species` out

If you have time at the end, rerun the analysis but this time leave out
`Species` so the model is just `Petal.Length ~ 1 + Sepal.Length`.
Remember to change the name of a) your model object and b) the file name
in the `file = "output/..."` argument.

Answer this: How did adding `Species` as a variable change the magnitude
of the effect of Sepal length on Petal length?

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
