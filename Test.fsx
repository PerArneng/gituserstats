
#load "GitUtils.fsx"
open GitUtils

let testFileChange =
    let change = Git.parseFileChange "36      12      GitUtils.fsx"
    let actual = Git.FileChange("GitUtils.fsx", Git.ChangeStats(1, 36, 12))
    printfn "%A\n%A" change.Value actual

    let ch1 = (Git.parseFileChange "36      12      GitUtils.fsx").Value.ChangeStats
    let ch2 = (Git.parseFileChange "1       5       Test.fsx").Value.ChangeStats
    let ch3 = (Git.parseFileChange "4       0       Flask.fsx").Value.ChangeStats
    let summary = Git.sumChangStats [ch1; ch2; ch3]
    printfn "%A" summary

testFileChange
