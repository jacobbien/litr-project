# Contributing to `litr`

## Debugging process

Debugging issues with code run in `knitr` hooks can be tricky. We suggest the following workflow:

1. Temporarily modify the litr package (directly in a .R file, adding a browser call in the hook).
2. Then install that version of litr with `devtools::install('litr', dependencies=False)` assuming you're at the root of the project directory. Using `dependencies=False` helps ensure you're not prompted about updating packages each time you install the package.
3. Once you're installed this modified version of litr and have restarted your R session, then call `litr::render()` on the file you are working on with `fresh_session=FALSE` to make sure the debugger hits the breakpoint as desired.

Repeat this process until you (hopefully) find a fix for the issue.

## Github API Rate Limiting

Knitting `create-litr/index.Rmd` makes quite a few calls to the GitHub API and it is easy to quickly reach the GitHub rate limit knitting `index.Rmd` repeatedly in a short period of time. To increase your GitHub API rate limit
- Use `usethis::create_github_token()` to create a Personal Access Token.
- Use `usethis::edit_r_environ()` and add the token as `GITHUB_PAT`.