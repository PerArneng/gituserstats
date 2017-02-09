#!/usr/bin/env fsharpi

open System.IO
open System.Text.RegularExpressions

let containsPattern pattern str =
    Regex.Matches(str, pattern).Count > 0

let isChangesLine =
    containsPattern "\d+ files? changed"

let isAGitDirectory directory =
    directory 
        |> Directory.GetDirectories 
        |> Array.toSeq
        |> Seq.tryFind (fun dir -> dir.EndsWith ".git")
        |> (fun item -> item.IsSome)

[<StructuredFormatDisplay("GitRepo({Name}, {FullPath})")>]
type GitRepo (path:string) =
    let dirInfo = new DirectoryInfo(path)
    member this.Name = dirInfo.Name
    member this.FullPath = dirInfo.FullName

[<StructuredFormatDisplay("Author({Name}, {EMail})")>]
type Author (name:string, email:string) =
    member this.Name = name
    member this.EMail = email

[<StructuredFormatDisplay("ChangeStats({FileCount}, {Insertions}, {Deletions})")>]
type ChangeStats (fileCount:int, insertions:int, deletions:int) =
    member this.FileCount = fileCount
    member this.Insertions = insertions
    member this.Deletions = deletions

[<StructuredFormatDisplay("Commit({Author}, {ChangeStats})")>]
type Commit (repo:GitRepo, author:Author, changeStats:ChangeStats) =
    member this.Repo = repo
    member this.Author = author
    member this.ChangeStats = changeStats

let exec program workingDirectory args  =
    let p = new System.Diagnostics.Process();
    //printfn "executing: %s %A" program args
    p.StartInfo.FileName <- program;
    p.StartInfo.Arguments <- (args)
    p.StartInfo.RedirectStandardOutput <- true
    p.StartInfo.UseShellExecute <- false
    p.StartInfo.WorkingDirectory <- workingDirectory
    p.Start() |> ignore 
    p.StandardOutput.ReadToEnd()

let gitProgram = exec "git"

let findGitRepos args = 
    args |> List.filter Directory.Exists
         |> List.filter isAGitDirectory
         |> List.map GitRepo

let parseChangStat str =
    let items = Regex.Matches(str, "(\d+) ([a-z]*)")
                        |> Seq.cast<Match> 
                        |> Seq.map (fun m -> (m.Groups.Item(1).Value, m.Groups.Item(2).Value))
    
    let mutable files = 0
    let mutable insertions = 0
    let mutable deletions = 0

    for item in items do
        let amount = fst item |> int
        let qualifier = snd item
        if qualifier.StartsWith("file") then
            files <- amount
        elif qualifier.StartsWith("insertion") then
            insertions <- amount
        elif qualifier.StartsWith("deletion") then
            deletions <- amount
    ChangeStats(files, insertions, deletions)

let parseCommitLog (repo:GitRepo) (commitLog:string) = seq {
    let rows = (commitLog.Split [|'\n'|])
    let mutable author:Option<Author> = None 
    
    for row in rows do
        
        if row.StartsWith "info|" then
            let items = row.Split [|'|'|]
            author <- Some(Author(items.[1], items.[2]))
        elif isChangesLine row then
            let changeStats = parseChangStat row
            let commit = Commit(repo, author.Value, changeStats)
            author <- None
            yield commit        
}            
            
let processGitRepo (repo:GitRepo):seq<Commit> =
    printfn "repo: %s (%s)" repo.Name  repo.FullPath
    let git = gitProgram repo.FullPath
    let branch = git "rev-parse --abbrev-ref HEAD"
    printfn "branch: %s" branch
    let commitLog = git "--no-pager log --shortstat --pretty=\"format:info|%an|%ae\""
    parseCommitLog repo commitLog


let args = fsi.CommandLineArgs |> Array.toList |> List.tail
let gitRepos = findGitRepos args

let mutable commits:List<Commit> = List.empty<Commit>

for repo in gitRepos do
    let repoCommits = processGitRepo repo
    commits <- List.append commits (Seq.toList repoCommits)

for c in commits do
    printfn "%A" c
 