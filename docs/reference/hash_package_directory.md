# Hash package directory

Gets an identifier that can be used to uniquely (whp) identify the
current state of the package. This is formed by ignoring the `LitrId`
field of the DESCRIPTION file, which is the location where the output of
this function is stored when [`litr::render`](render.md) generates the
package.

## Usage

``` r
hash_package_directory(package_dir)
```

## Arguments

- package_dir:

  Path to package
