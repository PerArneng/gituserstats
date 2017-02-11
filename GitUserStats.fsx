#!/usr/bin/env fsharpi

#load "GitUtils.fsx"
open GitUtils

open System
open System.IO

let findGitRepos args = 
    args |> List.filter Directory.Exists
         |> List.filter Git.isAGitDirectory
         |> List.map Git.GitRepo

let formatChangeStats fileCount insertions deletions netRows =
    sprintf "%10s %10s %10s %10s" fileCount insertions deletions netRows

let formatSummary (summary:Git.Summary) = 
        let a = summary.Author
        let c = summary.ChangeStats
        sprintf "%s %s <%s>"
            (formatChangeStats (string c.FileCount) (string c.Insertions) (string c.Deletions) (string c.NetRows)) a.Name a.EMail

let printSummary summary =
    printfn "%s User" (formatChangeStats "FileCount" "Insertions" 
                                         "Deletions" "NetRows") 
    printfn "------------------------------------------------"
    summary
        |> Seq.map formatSummary 
        |> Seq.iter (fun str -> printfn "%s" str)
    printfn "Count: %d" (Seq.length summary)


let main args =
    
    let commits = args 
                    |> findGitRepos
                    |> Seq.cast<Git.GitRepo>
                    |> Seq.map Git.getCommits
                    |> Seq.collect (fun c -> c)

    printfn "\nALL TIME"
    let summaryByEmail = Git.summaryByEMail commits
    printSummary summaryByEmail

    let commitsPerMonth = 
        commits 
            |> Seq.groupBy (fun c -> DateTime(c.Date.Year, c.Date.Month, 1))
            |> Seq.sortBy (fun group -> fst group)
    
    
    commitsPerMonth |> Seq.iter (fun group ->
        let date = fst group
        printfn "\n%d-%s" date.Year (date.ToString "MMMM")
        let summary = Git.summaryByEMail (snd group)
        printSummary summary
    )

// start the script
main (fsi.CommandLineArgs |> Array.toList |> List.tail)
