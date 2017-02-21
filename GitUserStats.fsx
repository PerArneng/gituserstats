#!/usr/bin/env fsharpi

#load "GitUtils.fsx"
open GitUtils

#load "Utils.fsx"
open Utils

open System
open System.IO

let findGitRepos args = 
    args |> List.filter Directory.Exists
         |> List.filter Git.isAGitDirectory
         |> List.map Git.GitRepo

let formatChangeStats fileCount insertions deletions netRows percentage =
    sprintf "%10s %10s %10s %10s %5s%%" fileCount insertions deletions netRows percentage

let formatSummary (summary:Git.Summary) = 
        let a = summary.Author
        let c = summary.ChangeStats
        sprintf "%s %s <%s>"
            (formatChangeStats (string c.FileCount) (string c.Insertions) (string c.Deletions) (string c.NetRows) (string summary.PercentNetRows)) a.Name a.EMail

let printSummary summary =
    printfn "%s User" (formatChangeStats "FileCount" "Insertions" 
                                         "Deletions" "NetRows" "Prcnt") 
    printfn "-----------------------------------------------------------"
    summary
        |> Seq.map formatSummary 
        |> Seq.iter (fun str -> printfn "%s" str)
    printfn "Count: %d" (Seq.length summary)
    

let printGroups groups =
        groups 
            |> Seq.iter (fun group ->
                            let header = fst group
                            printfn "\n%s" header
                            let summary = Git.summaryByEMail (snd group)
                            printSummary summary
                         )

let printTitle title =
    printfn "\n----------------------\n%s\n----------------------\n" title

let main args =
    
    let includeFileFilter (fileName:string):bool = true

    let commits = args 
                    |> findGitRepos
                    |> Seq.cast<Git.GitRepo>
                    |> Seq.map (Git.getCommits includeFileFilter)
                    |> Seq.collect (fun c -> c)
   

    printTitle "All Time"

    commits
        |> Git.groupCommits (fun c -> "All Time")
        |> printGroups

    printTitle "Weekly"

    commits 
        |> Git.groupCommits (fun c -> (Utils.weekOfYear c.Date.Year c.Date.DayOfYear))
        |> printGroups
    
    printTitle "Monthly"

    commits
        |> Git.groupCommits (fun c -> (Utils.monthOfYear c.Date.Year c.Date.Month))
        |> printGroups

    0

// start the script
main (fsi.CommandLineArgs |> Array.toList |> List.tail)
