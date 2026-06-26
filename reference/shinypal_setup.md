# Setup shinypal

A function to set up the shinypal environment. This function should be
called at the top of the server function of your shiny app.

## Usage

``` r
shinypal_setup(
  input,
  output,
  session,
  modules,
  download_filename = "shinypal_script.zip",
  download_template = "./modules/test_report.qmd",
  prefix = "data_"
)
```

## Arguments

- input:

  The shiny input object.

- output:

  The shiny output object.

- session:

  The shiny session object.

- modules:

  A character vector of paths to independent modules.

- download_filename:

  The name of the file to download when the user clicks the download
  button.

- download_template:

  The path to the Quarto markdown template file that will be used to
  generate the report.

- prefix:

  Prefix for each data step's internal id and the variable name it gets
  in the generated script (default `"data_"`). Use
  [`step_varname()`](http://williamgearty.com/shinypal/reference/step_varname.md)
  to retrieve the full value for a given step index.

## Value

Called for its side effects and returns `NULL` invisibly. It initializes
shinypal's shared reactive state, renders the live report and wires the
download handler and workflow observers, then sources each module's
`ui-aux.R` and `server.R` so their steps become available.

## Details

Each module should have a `ui-aux.R` file that defines the UI elements
for that module in the "Workflow" accordion and a `server.R` file that
defines the server-side logic for the module.

## See also

Other app setup:
[`shinypal_ui()`](http://williamgearty.com/shinypal/reference/shinypal_ui.md)

## Examples

``` r
if (FALSE) { # \dontrun{
server <- function(input, output, session) {
  modules <- list.dirs("./modules", recursive = FALSE)
  shinypal_setup(input, output, session, modules)
}
} # }
```
