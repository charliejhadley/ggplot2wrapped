# get_geoms_from_code_file

`get_geoms_from_code_file()` uses `{astgrepr}` to both extract and count
geom usage from a code file.

## Usage

``` r
get_geoms_from_code_file(file_path, geoms_dataset)
```

## Arguments

- file_path:

  Path to a code file.

- geoms_dataset:

  A data.frame containing a column 'geom_name' with the name of a geom
  of interest.

## Value

A tibble.
