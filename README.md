# ConversionPath
Tools for Conversion Path Data Analysis

## Demo

Here is a flexdashboard that demonstrates some of the functionality and visualizations provided by the package

http://bdshaff.shinyapps.io/CP-FlexDash

## About

This package is inspired by the markov chain modeling used for Multi-Touch Attribution (MTA) analysis. MTA is a common task in the digital marketing industry. To learn more about attribution modeling and markov chain attribution modeling specifically you may find my blog post useful: https://bdshaff.github.io/blog/2021-04-02-markov-chain-attribution/

ConversionPath package aimes to bring together a number of useful tools for analysis of data typically used for MTA analysis. This includes simulation, estimation, and visualization. Best efforts will be made to make these tools also useful for users of the `ChannelAttribution` R package. 

When analyzing conversion paths using markov chains our transition matrix takes a specific form. Below is an example of what would be considered a "proper" transition matrix within the ConversionPath package. 

* first column is a row of probabilities for the starting point of the path.
* first column is all 0s indicating that the starting position is not accessible from any state in the markov chain.
* the second and third row/column form a 2x2 identity sub matrix designating the absorbing conversion and non-conversion states.
* the matrix is a proper transition matrix i.e. square and all rows sum to 1.

|               | start|  conv|  drop| Online-Video| Organic Search| Organic Social| Paid Search| Paid Social| Programmatic|
|:--------------|-----:|-----:|-----:|------------:|--------------:|--------------:|-----------:|-----------:|------------:|
|start          |     0| 0.000| 0.000|        0.188|          0.141|          0.164|       0.189|       0.154|        0.164|
|conv           |     0| 1.000| 0.000|        0.000|          0.000|          0.000|       0.000|       0.000|        0.000|
|drop           |     0| 0.000| 1.000|        0.000|          0.000|          0.000|       0.000|       0.000|        0.000|
|Online-Video   |     0| 0.044| 0.361|        0.000|          0.151|          0.071|       0.087|       0.133|        0.154|
|Organic Search |     0| 0.047| 0.352|        0.119|          0.000|          0.048|       0.057|       0.166|        0.211|
|Organic Social |     0| 0.045| 0.315|        0.076|          0.081|          0.000|       0.182|       0.134|        0.167|
|Paid Search    |     0| 0.053| 0.347|        0.031|          0.028|          0.121|       0.000|       0.190|        0.230|
|Paid Social    |     0| 0.045| 0.348|        0.168|          0.201|          0.045|       0.047|       0.000|        0.147|
|Programmatic   |     0| 0.052| 0.393|        0.111|          0.120|          0.063|       0.092|       0.170|        0.000|

## Download

```r
devtools::install_github("bdshaff/conversionpath")

```


## Functions

`fit_transition_matrix`

`compose_transition_matrix`

`extract_path_list`

`simulate_path_list`

`simulate_path_table`

`simulate_path`

`generate_path_data`

`conversion_flow_diagram`

`plot_path_lengths`

`plot_touchpoint_frequency`

`plot_transition_matrix`

`remove_rep_tchp`

`transition_matrix_from_markov_model`
