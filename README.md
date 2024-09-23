# Cabal Project File Parser Test

This repository can be used to 

- Download all repositories of a Stackage snapshot
- Parse all `cabal.project` files in the repositories of the snapshot and see whether it succeeds

## Prerequisites

- Python3
- A local development version of [Cabal](https://github.com/haskell/cabal), you need to clone the repository and reference it via `cabal.project.local`. You need to reference its packages`cabal-install`, `cabal-install-solver`, `Cabal-syntax` and `Cabal`

Here is the `cabal.project.local` I've used:
```cabal
packages: ./ ../cabal/cabal-install ../cabal/cabal-install-solver ../cabal/Cabal-syntax ../cabal/Cabal
```

## Clone Stackage Snapshot

Usage: `clone-stackage-snapshot.py <target_dir>`

Where `target_dir` is the directory to clone the repositories into.

Example: `./clone-stackage-snapshot.py ~/snapshot-stackage`

The script will clone all repositories listed in a Stackage snapshot into directory `./snapshot-stackage` and log the results into file `./clone.log`
It is currently hardcoded to download snapshot [LTS Haskell 22.33](https://www.stackage.org/lts-20.12/cabal.config) published on 2024-08-12.
Overwrite variable `snapshot_url` in the script if you want to clone another snapshot.

Note:
The script may require `git config --global url."https://".insteadOf git://` to work as lots of packages have set `git://github.com` instead of `http://github.com` as their source repo urls.

## Parse files of projects in a directory

You can parse the project files of all projects in a directory via

``` sh
cabal run parse-project-files -- <target_dir>
```

As printing the results to the console takes much time, pipe the outputs into a file.
Here is an example to parse all project files in directory `~/snapshot-stackage`, piping the output to `parse-files.txt`:

``` sh
cabal run parse-project-files -- ~/snapshot-stackage/ >> parse-files.txt
```


## Parse a project file

You can also parse just one project file via

``` sh
cabal run parse-project-file -- <path-to-dir-containing-the-file>
```

Example:

``` sh
cabal run parse-project-file -- ~/snapshot-stackage/aeson
```


## Download all cabal.project files from GitHub

```sh
GITHUB_TOKEN="your_github_token" ./dl-github-cabal-project-files.py
```