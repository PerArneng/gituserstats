
#load "OSUtils.fsx"
open OSUtils

open System
open System.IO
open System.Text.RegularExpressions

module Git =

    [<StructuredFormatDisplay("GitRepo({Name}, {FullPath})")>]
    type GitRepo (path:string) =
        let dirInfo = new DirectoryInfo(path)
        member this.Name = dirInfo.Name
        member this.FullPath = dirInfo.FullName

    [<StructuredFormatDisplay("Author({Name}, {EMail})")>]
    type Author (name:string, email:string) =
        member this.Name = name
        member this.EMail = email

    [<StructuredFormatDisplay("ChangeStats({FileCount}, {Insertions}, {Deletions}, {NetRows})")>]
    type ChangeStats (fileCount:int, insertions:int, deletions:int) =
        member this.FileCount = fileCount
        member this.Insertions = insertions
        member this.Deletions = deletions
        member this.NetRows = insertions - deletions

    [<StructuredFormatDisplay("Commit({Repo}, {Branches}, {Id}, {Author}, {ChangeStats}, {Date})")>]
    type Commit (repo:GitRepo,  branches:seq<string>, id:string, author:Author, 
                 changeStats:ChangeStats, date:DateTime) =
        member this.Repo = repo
        member this.Author = author
        member this.ChangeStats = changeStats
        member this.Date = date
        member this.Branches = branches
        member this.Id = id;

    [<StructuredFormatDisplay("Summary({Author}, {ChangeStats})")>]
    type Summary (author:Author, changeStats:ChangeStats) =
        member this.Author = author
        member this.ChangeStats = changeStats

    let runGitInDir dir args =
        OS.execAndGetLines dir "git" args

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

    let parseCommitLog (repo:GitRepo) (branch:string) (rows:seq<string>) = seq {
        let mutable author:Option<Author> = None 
        let mutable date:Option<DateTime> = None 
        let mutable id:Option<string> = None

        for row in rows do
        
            if row.StartsWith "info|" then
                let items = row.Split [|'|'|]
                id <- Some(items.[1])
                author <- Some(Author(items.[2], items.[3]))
                date <- Some(DateTime.Parse(items.[4]))
            elif isChangesLine row then
                let changeStats = parseChangStat row
                let commit = Commit(repo, [branch], id.Value, author.Value, 
                                    changeStats, date.Value)
                id<- None
                author <- None
                date <- None
                yield commit  
    }            
           
    /// distinct per commit id. branches are collected in to the one
    /// commit that is chosen   
    let distinctCommits (commits:seq<Commit>) =
        commits |> Seq.groupBy (fun c -> c.Id)
                |> Seq.map (fun group -> snd group)
                |> Seq.map (fun commitList ->
                    let branches = commitList 
                                        |> Seq.collect (fun c -> c.Branches)
                                        |> Seq.distinct
                    let head = commitList |> Seq.head
                    Commit(head.Repo, branches, head.Id, head.Author, 
                           head.ChangeStats, head.Date)
                )

    let getCommits (repo:GitRepo):seq<Commit> =
        let git = runGitInDir repo.FullPath
        let branches = git "branch -a" 
                                |> Seq.map (fun s -> s.Trim())
                                |> Seq.filter (fun s -> s.Contains "remotes")
                                |> Seq.filter (fun s -> not (s.Contains "->"))
        branches 
            |> Seq.collect (fun branch ->
                let command = (sprintf "--no-pager log %s --shortstat --pretty=\"format:info|%%h|%%an|%%ae|%%ai\"" branch)
                parseCommitLog repo branch (git command) 
            ) |> distinctCommits

    let commitsToChangeStats (commits:seq<Commit>) =
        commits |> Seq.map (fun c -> c.ChangeStats)

    let changeSummaryFromChangeStats (changeStats:seq<ChangeStats>) =
        ChangeStats(
            (changeStats |> Seq.sumBy (fun c -> c.FileCount)),
            (changeStats |> Seq.sumBy (fun c -> c.Insertions)),
            (changeStats |> Seq.sumBy (fun c -> c.Deletions))
        )

    let changeSummaryFromCommits (commits:seq<Commit>) =
        changeSummaryFromChangeStats (commits |> commitsToChangeStats) 

    let groupByEMail (commits:seq<Commit>) =
        commits 
            |> Seq.groupBy (fun c -> c.Author.EMail)
            |> Seq.map (fun group -> snd group)
    

    let summaryByEMail (commits:seq<Commit>) =
        commits 
            |> groupByEMail
            |> Seq.map (fun g -> Summary((Seq.head g).Author, 
                                  (g |> changeSummaryFromCommits)
                                 )
                       )
            |> Seq.sortBy (fun s -> -s.ChangeStats.NetRows)
