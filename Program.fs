open System
open FSharp.Data
open System.IO

[<Literal>]
let ControlEdgesFileName = "control.edges.csv"
[<Literal>]
let PlanningEdgesFileName = "planning.edges.csv"

type ApolloModule =
    | Planning
    | Control

type ControlXMI = XmlProvider<"control.xmi">
type PlanningXMI = XmlProvider<"planning.xmi">

type GraphLink = {
    source: string
    target: string
    linkType: string
}

let extractControlLinks () =
    ControlXMI.GetSample().
        Extension.Connectors 
        |> Array.map (fun item -> {
            source = item.Source.Model.Name
            target = item.Target.Model.Name
            linkType = item.Properties.EaType
        })

let extractPlanningLinks () =
    PlanningXMI.GetSample().
        Extension.Connectors 
        |> Array.map (fun item -> {
            source = item.Source.Model.Name
            target = item.Target.Model.Name
            linkType = item.Properties.EaType
        })

let saveGraphByNameAndLink name links =
    let aggregateLinks (n:GraphLink) = sprintf "%s,%s,%s" n.source n.target n.linkType
    File.WriteAllText (name, String.Join ("\n", links |> Array.map aggregateLinks |> Array.distinct))

let saveControl() = extractControlLinks() |> saveGraphByNameAndLink ControlEdgesFileName
let savePlanning() = extractPlanningLinks() |> saveGraphByNameAndLink PlanningEdgesFileName 

let save = function
| Control -> saveControl()
| Planning -> savePlanning()

let loadLinksByFileName fileName = 
    let content = File.ReadAllLines fileName
    let getLink (line:string) =
        let tokens = line.Split(",")

        { source = tokens.[0]; target = tokens.[1]; linkType = tokens.[2] }
    
    content |> Array.map getLink

let load = function
| Control -> loadLinksByFileName ControlEdgesFileName
| Planning -> loadLinksByFileName PlanningEdgesFileName

let safeLoad = function
| Planning ->
    if File.Exists PlanningEdgesFileName |> not then save Planning
    load Planning

| Control ->
    if File.Exists ControlEdgesFileName |> not then save Control
    load Control

type 'a Graph = 'a GraphNode list
and 'a GraphNode = {
    value: 'a
    outLinks: 'a GraphNode list
}

type ProjectGraph = string Graph

let transformLinksToGraph (links:GraphLink[]) =
    let rec buildGraph  (currentGraph:ProjectGraph) (remainingLinks:GraphLink list) =
        match remainingLinks with
        | l::rem ->
            let sourceOption = currentGraph |> List.tryFind (fun n -> n.value = l.source)
            let targetOption = currentGraph |> List.tryFind (fun n -> n.value = l.target)
            match sourceOption, targetOption with
            | None, None ->
                let source = { value = l.source; outLinks = [] }
                let target = { value = l.target; outLinks = [] }
                remainingLinks |> buildGraph (currentGraph@[source; target])
            | None, Some _ ->
                let source = { value = l.source; outLinks = [] }
                remainingLinks |> buildGraph (currentGraph@[source])
            | Some _, None ->
                let target = { value = l.target; outLinks = [] }
                remainingLinks |> buildGraph (currentGraph@[target])
            | Some source, Some target ->
                let newGraph = currentGraph |> List.except [source]
                let newSource = { source with outLinks = (source.outLinks)@[target]}
                newGraph@[newSource]
         | [] -> currentGraph
            
        
    links |> Array.toList |> buildGraph []

[<EntryPoint>]
let main argv =
    let controlEdges = safeLoad Control
    let controlNodes = safeLoad Planning
    
    0 // return an integer exit code
