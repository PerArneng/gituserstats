
# Git User Stats

GitUserStats is a small script written in F# to summarise data about what users are
active in git repos. It takes multiple different git repository folders as arguments
and then create a summary for them all. The script parses the gitlog for whatever
branch the git repo folders might be on.

## Required Software
To be able to run this script you need to have an F# script interpreter installed
and in your path.

* [Linux](http://fsharp.org/use/linux/)
* [Windows](https://docs.microsoft.com/en-us/dotnet/articles/fsharp/tutorials/fsharp-interactive/)

## Usage

```
$ fsi GitUserStats.fsx <git repo folders...>
```

