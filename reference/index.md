# Package index

## Setting up an app

Entry points you call once to build the interface and wire its server.

- [`shinypal_setup()`](http://williamgearty.com/shinypal/reference/shinypal_setup.md)
  : Setup shinypal
- [`shinypal_ui()`](http://williamgearty.com/shinypal/reference/shinypal_ui.md)
  : UI for ShinyPal

## Defining workflow steps

The builder API for registering a step, plus the per-step index helper.

- [`add_shinypal_data_step()`](http://williamgearty.com/shinypal/reference/add_shinypal_data_step.md)
  : Register a complete data-producing step
- [`add_shinypal_plot_step()`](http://williamgearty.com/shinypal/reference/add_shinypal_plot_step.md)
  : Register a complete plot-producing step
- [`add_shinypal_step()`](http://williamgearty.com/shinypal/reference/add_shinypal_step.md)
  : Add a step to the report, workflow, and the code chain
- [`next_step_index()`](http://williamgearty.com/shinypal/reference/next_step_index.md)
  : Get the next workflow step index
- [`step_varname()`](http://williamgearty.com/shinypal/reference/step_varname.md)
  : Build a data step's internal id and generated variable name

## Step UI components

Inputs and buttons a module drops into a step’s panel.

- [`accordion_panel_remove_button()`](http://williamgearty.com/shinypal/reference/accordion_panel_remove_button.md)
  : Accordion panel that includes a remove button
- [`df_modal_button()`](http://williamgearty.com/shinypal/reference/df_modal_button.md)
  : Button to show dataframe modal
- [`select_column_input()`](http://williamgearty.com/shinypal/reference/select_column_input.md)
  : Select input to choose a column from a specified dataset
- [`select_dataset_input()`](http://williamgearty.com/shinypal/reference/select_dataset_input.md)
  : Select input to choose a shinypal intermediate dataset
- [`varname_input()`](http://williamgearty.com/shinypal/reference/varname_input.md)
  : Text input to give a step's dataset a custom name
- [`verbatimTextOutput_copy()`](http://williamgearty.com/shinypal/reference/verbatimTextOutput_copy.md)
  : Text element with a button to copy the code to the clipboard

## Step server observers

Wiring that keeps each component live and in sync.

- [`clip_observe()`](http://williamgearty.com/shinypal/reference/clip_observe.md)
  : Add an observer to a copy button

- [`column_select_observe()`](http://williamgearty.com/shinypal/reference/column_select_observe.md)
  :

  Keep a `varSelectInput` of data.frame column names up-to-date

- [`df_modal_observe()`](http://williamgearty.com/shinypal/reference/df_modal_observe.md)
  : Show a modal with a reactable data.frame

- [`df_select_observe()`](http://williamgearty.com/shinypal/reference/df_select_observe.md)
  :

  Keep a `selectInput` of intermediate data.frames up-to-date

- [`file_observe()`](http://williamgearty.com/shinypal/reference/file_observe.md)
  : Observe a file input to be included in the download bundle

- [`var_name_observe()`](http://williamgearty.com/shinypal/reference/var_name_observe.md)
  : Validate and store a custom name for a step's dataset

## Intermediate data

Storing and retrieving the datasets passed between steps.

- [`get_int_data()`](http://williamgearty.com/shinypal/reference/get_int_data.md)
  : Get an intermediate data object
- [`get_int_dfs()`](http://williamgearty.com/shinypal/reference/get_int_dfs.md)
  : Get the names of all intermediate data.frames for a given step
- [`set_int_data()`](http://williamgearty.com/shinypal/reference/set_int_data.md)
  : Set an intermediate data object

## Generated code & reports

Inspecting the code a workflow produces.

- [`get_chunk()`](http://williamgearty.com/shinypal/reference/get_chunk.md)
  : Get the expanded code chunk for a registered step
- [`workflow_has_errors()`](http://williamgearty.com/shinypal/reference/workflow_has_errors.md)
  : Check whether the workflow has incomplete or errored steps

## Utilities

Environment detection and other helpers.

- [`get_colors()`](http://williamgearty.com/shinypal/reference/get_colors.md)
  : Generate step colors from a step index
- [`is_shinylive()`](http://williamgearty.com/shinypal/reference/is_shinylive.md)
  : Detect a shinylive (webR) session
