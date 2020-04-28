open System
open FSharp.Data
open System.IO

type ControlXMI = XmlProvider<"control.xmi">
type PlanningXMI = XmlProvider<"planning.xmi">

type GraphNodes = {
    source: string
    target: string
    linkType: string
}

let doForControl =
    let file = ControlXMI.GetSample()

    let controlNodes = 
        file.Extension.Connectors 
        |> Array.map (fun item -> {
            source = item.Source.Model.Name
            target = item.Target.Model.Name
            linkType = item.Properties.EaType
        })

    let aggregateNode (n:GraphNodes) =
        sprintf "%s,%s,%s" n.source n.target n.linkType

    File.WriteAllText ("control.edges.csv", String.Join ("\n", controlNodes |> Array.map aggregateNode |> Array.distinct))

let doForPlanning =
    let file = PlanningXMI.GetSample()

    let controlNodes = 
        file.Extension.Connectors 
        |> Array.map (fun item -> {
            source = item.Source.Model.Name
            target = item.Target.Model.Name
            linkType = item.Properties.EaType
        })

    let aggregateNode (n:GraphNodes) =
        sprintf "%s,%s,%s" n.source n.target n.linkType

    File.WriteAllText ("planning.edges.csv", String.Join ("\n", controlNodes |> Array.map aggregateNode |> Array.distinct))



[<EntryPoint>]
let main argv =
    doForControl
    
    0 // return an integer exit code
