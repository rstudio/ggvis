# ggvis 0.2.0.99

* histogram binning controls are now `width` and one of `center` or `boundary`
rather than `binwidth` and `origin`.  In addition, the algorithms for 
determining various defaults have been modified by R Pruim.  In particular, 
for integer data, `width` will be a pretty integer avoiding distortions due 
to some bin ranges containing more integers than others.

* `add_dscale()` has been replaced with `scale_quantitative()`,
  `scale_nominal()`, `scale_ordinal()`, and similar.

* Reactive expressions can be used for scale domains. This allows the scale
  domain to change dynamically.

* Axis and legend properties are fixed. (#90)

* Histograms allow stacking.

* Dynamic plots now with with by_group. (#71)

* Gear icon displays properly in Windows. (#159)

* New `singular()` and corresponding `scale_singular()` make it easier to
  draw plots where x or y are constant (and hence uninteresting), such as
  for a 1d dot plot (#127).

* `compute_histogram()` gains `pad` argument to control whether empty bins
  on either side of the data extents are added. This is useful for frequency
  polygons and to ensure that histograms don't jam up against the axes.

# ggvis 0.2

The main change is that ggvis now uses a functional approach to building plots. Instead of doing:

    ggvis(mtcars, props(~wt, ~mpg)) + layer_point()

You now do:

    layer_points(ggvis(mtcars, ~wt, ~mpg))

This is a bit clunky, but we streamline it by using the pipe operator (`%>%`, from magrittr):

    mtcars %>%
      ggvis(~wt, ~mpg) %>%
      layer_points()
  
We think that this change will make it a little easier to create plots, and just as importantly, it's made the internals of ggvis much much simpler (so now we actually understand how it works!). As part of these changes:

* We now have a better idea of how layers should work. These are the "magic"
  bits of ggvis - they can inspect the current state of the plot, the data and
  the visual properties and decide what to do. For an example, take a look at
  `layer_guess()` which implements the most important parts of `qvis()`, 
  guessing which type of layer to use to display the data.

* `ggvis()` and all layer functions now take props directly - you no longer 
  need to use `props()` in everyday work.

* You can seamlessly use data transformations from dplyr: that means that
  you use `group_by()` to define grouping in the plot, and you can use
  `filter()`, `summarise()`, `mutate()` and `arrange()` both inside and
  outside of visualisations. See `ggvis?dplyr` for more examples.

* Data transformations are now handled by `compute_*()` functions. These
  are S3 generics with methods for data frames, grouped data frames and 
  ggvis objects. This means that any transformation done by ggvis for
  a visualisation (e.g. smoothing) can also be done on ordinary datasets so
  you can see exactly what variables are being created.

* It is possible to extract all the data objects, including those that are
  created by a transformation function, with the `get_data()` function. This
  makes it easier to inspect and understand what's happening to your data.

* The `explain()` function shows the structure of the ggvis object in a
  somewhat-readable format.

* New `handle_click()`, `handle_hover()`, `handle_resize()` and 
  `handle_brush()` allow you to connect callbacks to important ggvis events.
  A fully reactive interface will follow in the future.

* The process of embedding ggvis plots in shiny apps has been overhauled and
  simplified. See details in `ggvis?shiny` and sample apples in `demos/apps/`.

* A new built-in dataset: cocaine, recording cocaine seizures in the US in
  2007. We plan to transition our dummy examples that use mtcars to something
  more useful/informative over time.


# ggvis 0.1

* First release
