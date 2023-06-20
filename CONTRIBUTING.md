# Contributing to `litr`

## Github API Rate Limiting

Knitting `create-litr/index.Rmd` makes quite a few calls to the GitHub API and it is easy to quickly reach the GitHub rate limit knitting `index.Rmd` repeatedly in a short period of time. To increase your GitHub API rate limit
- Use `usethis::create_github_token()` to create a Personal Access Token.
- Use `usethis::edit_r_environ()` and add the token as `GITHUB_PAT`.