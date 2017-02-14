
#load "OSUtils.fsx"
open OSUtils

open System
open System.IO
open System.Collections.Generic
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

    [<StructuredFormatDisplay("FileChange({File}, {ChangeStats})")>]
    type FileChange (file:string, changeStats:ChangeStats) =
        member this.File = file
        member this.ChangeStats = changeStats

    let sumChangStats (changeStatsx:seq<ChangeStats>):ChangeStats =
        let folder (acc:ChangeStats) (cs:ChangeStats) : ChangeStats = 
            ChangeStats(acc.FileCount + cs.FileCount, acc.Insertions + cs.Insertions, acc.Deletions + cs.Deletions)
        Seq.fold folder (ChangeStats(0, 0, 0)) changeStatsx

    [<StructuredFormatDisplay("Commit({Repo}, {Branches}, {Id}, {Author}, {ChangeStats}, {Date}, {FileChanges})")>]
    type Commit (repo:GitRepo,  branches:seq<string>, id:string, author:Author, 
                 date:DateTime, fileChanges:seq<FileChange>) =

        member this.Repo = repo
        member this.Author = author
        member this.ChangeStats = fileChanges
                                        |> Seq.map (fun fc -> fc.ChangeStats) 
                                        |> sumChangStats
        member this.Date = date
        member this.Branches = branches
        member this.Id = id;
        member this.FileChanges = fileChanges

    [<StructuredFormatDisplay("Summary({Author}, {ChangeStats}, {PercentNetRows})")>]
    type Summary (author:Author, changeStats:ChangeStats, percentNetRows:int) =
        member this.Author = author
        member this.ChangeStats = changeStats
        member this.PercentNetRows = percentNetRows

    let fileChangeRegex =
        Regex("^(?<ins>\d+|-)\s+(?<del>\d+|-)\s+(?<file>.*)", RegexOptions.Compiled)

    let runGitInDir dir args =
        OS.execAndGetLines dir "git" args

    let containsPattern pattern str =
        Regex.Matches(str, pattern).Count > 0

    let isChangesLine =
        containsPattern "\d+ files? changed"
    
    let isFileChangeLine =
        containsPattern "^\d+\s+\d+\s+"

    let isAGitDirectory directory =
        directory 
            |> Directory.GetDirectories 
            |> Array.toSeq
            |> Seq.tryFind (fun dir -> dir.EndsWith ".git")
            |> (fun item -> item.IsSome)

    let parseFileChange row : Option<FileChange> =
        let fileChangeMatch = fileChangeRegex.Match row
        if not fileChangeMatch.Success then
            None
        else
            let fileChangeRowGroups = fileChangeMatch.Groups
            let grp (name:string):string = (fileChangeRowGroups.Item name).Value
            let grpi name = (grp name) |> (fun s -> if (s.Equals "-") then "0" else s) |> int 
            Some(FileChange((grp "file"), ChangeStats(1 , (grpi "ins") , (grpi "del"))))

    let parseCommitLog (repo:GitRepo) (branch:string) (rows:seq<string>) (includeFile: string->bool)= seq {
        let mutable author:Option<Author> = None 
        let mutable date:Option<DateTime> = None 
        let mutable id:Option<string> = None
        let mutable fileChangeList:List<FileChange> = List<FileChange>();

        for row in rows do
        
            let fileChange = parseFileChange row

            if row.StartsWith "info|" then
                let items = row.Split [|'|'|]
                id <- Some(items.[1])
                author <- Some(Author(items.[2], items.[3]))
                date <- Some(DateTime.Parse(items.[4]))
            elif fileChange.IsSome then
                if (includeFile fileChange.Value.File) then
                    fileChangeList.Add fileChange.Value    
            elif isChangesLine row then
                let commit = Commit(repo, [branch], id.Value, author.Value, 
                                    date.Value, fileChangeList |> Seq.cast<FileChange>)
                id<- None
                author <- None
                date <- None
                fileChangeList <- List<FileChange>()
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
                           head.Date, head.FileChanges)
                )

    let getCommits (includeFile: string->bool) (repo:GitRepo):seq<Commit> =
        let git = runGitInDir repo.FullPath
        let branches = git "branch -a" 
                                |> Seq.map (fun s -> s.Trim())
                                |> Seq.filter (fun s -> s.Contains "remotes")
                                |> Seq.filter (fun s -> not (s.Contains "->"))
        branches 
            |> Seq.collect (fun branch ->
                let command = (sprintf "--no-pager log %s --numstat --shortstat --pretty=\"format:info|%%h|%%an|%%ae|%%ai\"" branch)
                parseCommitLog repo branch (git command) includeFile
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
    

    let percent (part:int) (whole:int):int =
        int (
            round (((float (abs part))/(float (abs whole)))*100.0)
        )

    let summaryByEMail (commits:seq<Commit>) =
        let summary = commits 
                        |> groupByEMail
                        |> Seq.map (fun g -> Summary((Seq.head g).Author, 
                                              (g |> changeSummaryFromCommits), 0
                                             )
                                   )
                        |> Seq.sortBy (fun s -> -s.ChangeStats.NetRows)
        let totalChanges:int = summary |> Seq.sumBy (fun s-> (abs s.ChangeStats.NetRows))
        summary |> Seq.map (fun s -> Summary(s.Author, s.ChangeStats, 
                                             (percent (s.ChangeStats.NetRows) totalChanges) )
                           )
                |> Seq.sortBy (fun s -> -s.PercentNetRows)
