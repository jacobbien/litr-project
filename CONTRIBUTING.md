# Contributing to `litr`

## Debugging process

Debugging issues with code run in `knitr` hooks can be tricky. We suggest the following workflow:

1. Temporarily modify the litr package (directly in a .R file, adding a browser call in the hook).
2. Then install that version of litr with `devtools::install('litr', dependencies=FALSE)` assuming you're at the root of the project directory. Using `dependencies=FALSE` helps ensure you're not prompted about updating packages each time you install the package.
3. Once you're installed this modified version of litr and have restarted your R session, then call `litr::render()` on the file you are working on with `fresh_session=FALSE` to make sure the debugger hits the breakpoint as desired.

Repeat this process until you (hopefully) find a fix for the issue.

## Bootstrapping a fix to `litr`'s own rendering machinery

`create-litr/index.Rmd` checks that the installed `litr` is the one from the latest release, so that we never use new functionality to build itself. But if the bug you fixed is in `litr`'s *rendering* machinery — something that runs while `litr::render()` is knitting `index.Rmd`, such as the post-processing that adds hyperlinks to the .html output — then the released version cannot render the document at all, and there is no way to produce the release except with the fix in hand.

For that case only:

1. `devtools::install('litr', dependencies=FALSE)` and restart R, so that the fixed version is the one loaded.
2. `Sys.setenv(LITR_ALLOW_UNRELEASED = "true")` to waive the check that the installed version is the latest release. The variable is inherited by the fresh R session that `litr::render()` spawns.
3. `litr::render("index.Rmd")` and confirm it completes cleanly.
4. Commit, tag, and publish the release. From then on `install_old()` restores a version that contains the fix, and the check can go back to doing its job.

Note that step 3 ends by reinstalling the *released* (still unfixed) version, so a second render before the release is published would need step 1 again.

## Github API Rate Limiting

Knitting `create-litr/index.Rmd` makes quite a few calls to the GitHub API and it is easy to quickly reach the GitHub rate limit knitting `index.Rmd` repeatedly in a short period of time. To increase your GitHub API rate limit
- Use `usethis::create_github_token()` to create a Personal Access Token.
- Use `usethis::edit_r_environ()` and add the token as `GITHUB_PAT`.