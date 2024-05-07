## Release v0.17.0

- Fix in `Fixed_hg_environment`: use the correct variable name `HGUSER` to pass 
  the user name to hg.

## Release v0.16.0

- Add optional parameter `include_files_in_changeset_info` to functions that return changesets
  * Allows including the list of changed files in the `Changeset_info` structure.
  
- Add `get_default_url` function
  * Retrieves the default URL using `hg show paths.default`
