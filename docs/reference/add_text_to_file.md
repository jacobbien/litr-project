# Add Some Text to a File

The text will be added to the file at a particular line specified by
`location`. The first line of `txt` will be on line `location` of the
modified file. If `location` is NULL, then text is added to end of file.
If file does not exist, it is created and `location` is ignored (unless
`req_exist` is `TRUE`, in which case an error is thrown).

## Usage

``` r
add_text_to_file(
  txt,
  filename,
  location = NULL,
  req_exist = FALSE,
  pad = FALSE,
  msg = NULL
)
```

## Arguments

- txt:

  Character vector to add to file

- filename:

  Name of file

- location:

  Specifies where text should be added. See description for more.

- req_exist:

  If TRUE, then throws an error if file doesn't exist

- pad:

  If TRUE, then when text is being added to a preexisting file, it adds
  a newline

- msg:

  An optional message to put at top of file if this is a new file
