#r "paket: groupref FakeBuild //"
#load ".fake/build.fsx/intellisense.fsx"

#if !FAKE
#r "netstandard"
#r "Facades/netstandard" // https://github.com/ionide/ionide-vscode-fsharp/issues/839#issuecomment-396296095
#endif

open System
open Fake.Core
open Fake.Core.TargetOperators
open Fake.DotNet
open Fake.IO
open Fake.IO.Globbing.Operators
open Fake.IO.FileSystemOperators
open Fake.JavaScript

let solutionFileName = 
    !! "*.sln"
    |> Seq.toList
    |> (function
        | [] -> failwithf """build.fsx expects to find a solution file './*.sln' - no file was found. This can be \
                             created automatically by Visual Studio or Visual Studio Code"""
        | [soln] -> 
            printfn "solution file '%s' found" soln
            soln
        | files -> failwithf "Build.fsx expects to find a single '*.sln' file but there are two: '%A'" files)
        

Target.create "Clean" (fun _ ->
  !! "src/**/bin"
  ++ "src/**/obj"
  ++ "dist"
  ++ ".fable"
  |> Shell.cleanDirs
)

Target.create "CleanFableJS" <| fun _ ->
    !! (__SOURCE_DIRECTORY__  @@ "src/**/*.fs.js")
    |> Seq.toList
    |> File.deleteAll

Target.create "CleanNode" <| fun _ ->
    Shell.cleanDir (__SOURCE_DIRECTORY__  @@ "node_modules")
    File.delete (__SOURCE_DIRECTORY__ @@ "package-lock.json")

Target.create "DotnetRestore" (fun _ ->
  DotNet.restore
    (DotNet.Options.withWorkingDirectory __SOURCE_DIRECTORY__)
    solutionFileName
)

Target.create "NpmInstall" (fun _ ->
  Npm.install id
)

Target.create "Build" (fun _ ->
  Npm.run "compile" id
)

Target.create "Dev" (fun _ ->
  Npm.run "dev" id
)

Target.create "Dist" (fun _ ->
  Npm.run "dist" id
)

Target.create "DistDir" (fun _ ->
  Npm.run "dist:dir" id
)

Target.create "KillZombies" <| fun _ ->
    Fake.Core.Process.killAllByName <| String.replace ".sln" ".exe" solutionFileName
    Fake.Core.Process.killAllByName "node"
    Fake.Core.Process.killAllByName "dotnet"

// Build order
"Clean"
  ==> "DotnetRestore"
  ==> "NpmInstall"
  ==> "Build"

"NpmInstall"
  ==> "Dev"

"NpmInstall"
  ==> "Dist"

"NpmInstall"
  ==> "DistDir"

// start build
Target.runOrDefaultWithArguments "Dev"
