
module OS =

    let execAndGetStdOutAsString workingDirectory program args  =
        let p = new System.Diagnostics.Process()
        p.StartInfo.FileName <- program
        p.StartInfo.Arguments <- args
        p.StartInfo.RedirectStandardOutput <- true
        p.StartInfo.UseShellExecute <- false
        p.StartInfo.WorkingDirectory <- workingDirectory
        p.Start() |> ignore 
        p.StandardOutput.ReadToEnd()

    let execAndGetLines workingDirectory program args =
        execAndGetStdOutAsString workingDirectory program args
            |> fun s -> s.Split [|'\n'|]
            |> Seq.cast<string>
