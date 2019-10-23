# ggvis 0.4.5

* Switch from `dplyr::rbind_all()` to `dplyr::bind_rows()`, becuase `rbind_all` is being removed from dplyr 0.8.4. ([#482](https://github.com/rstudio/ggvis/pull/482))

# ggvis 0.4.4

* Various fixes to S3 method registration.

# ggvis 0.4.3

* Switched from stripped-down build of jQuery UI to a full build. (#410)

* Fixed problems for R CMD check in R 3.3.0.

* Remove vignettes due to R CMD check hanging.

# ggvis 0.4.2

* ggvis plots can now resize their width to the containing div, with
  `set_options(width = "auto")`. Height can be set automatically as well, but
  it will only work properly if the containing div has a fixed height, due to
  the way that web browsers do vertical layout. (#316, #374, #381)

* compatible with dplyr 0.4.2

# ggvis 0.4.1

* input_slider has been updated to work with Shiny 0.11.

* The parse spec and update events now happen in the correct order. This
  fixed an issue with plots flashing. (#351)

* Pointer events are now allowed in tooltips (#349)

* Updated to Vega 1.4.3 and D3 3.5.2.

* Startup messages are now shown only one in ten times. (#302)

* Added new dplyr verbs: `distinct`, `rename`, `slice`, and `transmute`. (#299)

* ggvis now gives a warning when key prop values are not unique. (#295)

# ggvis 0.4

## Usability improvements

* Boxplots are now supported, with `layer_boxplots()` and `compute_boxplot()`.

* Much better support for data objects with zero rows.

* Added support for displaying ggvis plots in dynamic UI in Shiny apps. (#165)

* `compute_bin()` uses `width` instead of `binwidth`, and `boundary` instead
  of `origin`. (#268)

* `compute_bin()` now defaults to `pad = FALSE`

* `compute_model_predictions()` always returns a result, even if there's an
  error (#102).

* `filter()` is no longer imported and re-exported from dplyr. This
  means that to use `filter()` with ggvis object you'll need to 
  make sure to load dplyr first.

* `compute_smooth()` supports more complex formulas. (#209)

* `compute_bin()` and `compute_count()` now preserve date and time properties.
  (#235)

* `export_png()` and `export_svg()` now work. This requires node.js, and vega
  must be installed via npm.

* Legend hiding is fixed. (#218)

* `count_vector()` preserves the order of factor levels. (#223)

* `compute_bin()` now ignores NA's. (#148)

* `layer_bars()` now uses correctly uses `fill` prop when it is passed to the
  function, and not inherited. (#201)

* `compute_count()` drops unused factor levels. (#201)

* `compute_bin()` and `compute_stack()` no longer give warnings and errors for
  zero-row data frames. (#211)

* Range calculation for zero-length vectors now returns NULL instead of
  throwing an error.

* Objects imported from the magritter and dplyr packages are now properly
  re-exported.

* Using "." in column names now works. (#246)

* Un-exported `:=`, to avoid possible conflict with data.table.

## Internal changes

* Updated to Vega 1.4.2. (#193 and #217)

* Switched from RJSONIO to jsonlite.

* Switched to the new non-standard argument evaluation strategy from dplyr 0.3,
  using the new lazyeval package.

# ggvis 0.3.0.1

* Reconcile shiny dependency at run-time, not build time.

# ggvis 0.3

* `add_guide_axis()` and `add_guide_legend()` have been replaced by
  `add_axis()` and `add_legend()`. Also, the interface for `add_legend()` has
  been simplified.

* Added `hide_axis()` and `hide_legend()` functions.

* When marks with a `band()` prop are added, the appropriate scale is
  automatically set to have `points = FALSE`. (#128)

* Continuous scales have a multiplicative expansion factor added by default,
  with the `expand` parameter of scale functions.

* Relative x and y scales for positioning of graphical elements can be added
  with `add_relative_scales()`.

* Added support for `strokeDash` property.

* Added support for controlling width and height of image marks.

* `prop()` objects have been modified so that they always record which scale
  they use.

* Removed `qvis()`: now the default behaviour of `ggvis()` is to add 
  `layer_guess()` if there are no layers on the plot already.

* `add_dscale()` has been replaced with `scale_quantitative()`,
  `scale_nominal()`, `scale_ordinal()`, and similar.

* Reactive expressions can be used for scale domains. This allows the scale
  domain to change dynamically.

* Axis and legend properties are fixed. (#90)

* Histograms allow stacking.

* Dynamic plots now with with by_group. (#71)

* Gear icon displays properly in Windows. (#159)

* `layer_bars()` are now symmetrical about the x tick positions.

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
