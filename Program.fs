open System
open FSharp.Data
open System.IO

type ControlXMI = XmlProvider<"control.xmi">
type PlanningXMI = XmlProvider<"planning.xmi">

type GraphLink = {
    source: string
    target: string
    linkType: string
}

let extractControlLinks () =
    let file = ControlXMI.GetSample()

    let controlLinks = 
        file.Extension.Connectors 
        |> Array.map (fun item -> {
            source = item.Source.Model.Name
            target = item.Target.Model.Name
            linkType = item.Properties.EaType
        })

    controlLinks

let extractPlanningLinks () =
    let file = PlanningXMI.GetSample()

    let planningLinks = 
        file.Extension.Connectors 
        |> Array.map (fun item -> {
            source = item.Source.Model.Name
            target = item.Target.Model.Name
            linkType = item.Properties.EaType
        })

    planningLinks

let saveGraph name links =
    let aggregateLinks (n:GraphLink) = sprintf "%s,%s,%s" n.source n.target n.linkType
    File.WriteAllText (name, String.Join ("\n", links |> Array.map aggregateLinks |> Array.distinct))

let saveControl = extractControlLinks() |> saveGraph "control.edges.csv"
let savePlanning = extractPlanningLinks() |> saveGraph "planning.edges.csv" 

let loadLinks fileName = 
    let content = File.ReadAllLines fileName
    let getLink (line:string) =
        let tokens = line.Split(",")

        { source = tokens.[0]; target = tokens.[1]; linkType = tokens.[2] }
    
    content |> Array.map getLink

[<EntryPoint>]
let main argv =
    
    
    0 // return an integer exit code
