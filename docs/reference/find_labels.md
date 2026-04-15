# Find a .Rmd chunk label in a code chunk

Find a .Rmd chunk label in a code chunk

## Usage

``` r
find_labels(chunk_code)
```

## Arguments

- chunk_code:

  Character vector of code from a .Rmd code chunk. Each element is a
  line of the code chunk.

## Value

List where chunk_idx is a logical vector for each line of the chunk
corresponding to whether a chunk label of the form `<<label>>` was found
and chunk_ids is a character vector of chunk label was found in that
chunk.
