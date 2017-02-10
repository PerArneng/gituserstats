#!/usr/bin/env fsharpi

#load "GitUtils.fsx"
open GitUtils
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
        sprintf "%s <- %s %s"
            (formatChangeStats (string c.FileCount) (string c.Insertions) (string c.Deletions) (string c.NetRows)) a.Name a.EMail

let main args =
    
    let commitsx = args 
                    |> findGitRepos
                    |> Seq.cast<Git.GitRepo>
                    |> Seq.map Git.getCommits
                    |> Seq.collect (fun c -> c)

    let summaryByEmail = Git.summaryByEMail commitsx

    printfn "%s" (formatChangeStats "FileCount" "Insertions" "Deletions" "NetRows") 
    printfn "----------------------------------------"

    summaryByEmail
        |> Seq.map formatSummary 
        |> Seq.iter (fun str -> printfn "%s" str)

// start the script
main (fsi.CommandLineArgs |> Array.toList |> List.tail)
