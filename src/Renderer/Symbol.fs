module Symbol
open Fable.React
open Fable.React.Props
open Browser
open Elmish
open Elmish.React
open Helpers
open type CommonTypes.ComponentType
//------------------------------------------------------------------------//
//-------------------------------Symbol Types-----------------------------//
//------------------------------------------------------------------------//

/// Model to generate one symbol (skeleton). Id is a unique Id 
/// for the symbol shared with Issie Component type.
/// The real type will obviously be much larger.
/// Complex information that never changes (other than Id) should 
/// probably not be here, but looked up via some function
/// from a more compact form, so that comparison of two Symbols to
/// determine are they the same is fast.



//The main issue to solve in order to be able to use Arie's functions
//is to add the vertices field to the record type Symbol.
//It is especially critical to solve the problem of overlapping symbols, 
//so we should use Arie's fnunction below instead of the function actually used:

//I think the best approach is to try and replace the boundingBox field with Vertices, 
//and change its type from XYPos*XYPos*XYPos*XYPos to XYPos list
//A part form this type change, not much more should be modified than is necessary because the data itself would actually be the same
//If this is accomplished without compilation errors then we should be fine to use Arie's functions

//A second option to solve the problem is to add a Vertices field where we would have the same data as in boundingBox but with type XYPos
//This solution is easier to implement but isn't so elegant

//For the second problem (ports only showing when hovering over them), we have to add the following match statement
//let inputRadius, outputRadius, portFillInput, portFillOutput = 
//                match props.Shape.ExpandedPort, props.Shape.IsHovered with
//                | None, true -> portRadius, portRadius, "#2f5e5e", "#2f5e5e"
//                | None, false -> portRadius, portRadius, "none", "none"
//                | Some CommonTypes.PortType.Input, true -> 
//                    expandedPortRadius, portRadius, "#2f5e5e", "#2f5e5e"
//                | Some CommonTypes.PortType.Input, false -> 
//                    expandedPortRadius, portRadius, "#2f5e5e", "none"
//                | Some CommonTypes.PortType.Output, true ->
//                    portRadius, expandedPortRadius, "#2f5e5e", "#2f5e5e"
//                | Some CommonTypes.PortType.Output, false ->
//                    portRadius, expandedPortRadius, "none", "#2f5e5e" 
                 
                   
//or we can simply replace the renderSymbol function with Arnau's renderShape, which looks less repetitive and more elegant, more "functional"
//the later option is faster to implement so I'd recommend that but we might run into further merging problems

//Possible extra features to add: symbols highlighted in red when they are overlapping after dragging them

//above is all completed

//To do:
//remove event listeners and call dragging messages from sheet module
//fix dragging bug with first component
//reduce lag of ports following symbol - I think will fix itself once we remove event listener, 
//as atm it is only on the polygon but not the text or port shaps
//make code more efficient/ remove redundant copying of code if possible



type Symbol = 
    {
        Pos: XYPos
        LastDragPos : XYPos
        IsDragging : bool
        IsSelected: bool
        IsHovered: bool
        IsOverlapped: bool
        Id : CommonTypes.ComponentId
        Type: CommonTypes.ComponentType
        Label: string
        InputPorts : CommonTypes.Port list
        OutputPorts : CommonTypes.Port list
        ExpandedPort: CommonTypes.PortType Option
        Vertices: XYPos list
        H: int
        W: int
    }    

type Model = Symbol list


//----------------------------Message Type-----------------------------------//

/// Messages to update symbol model
/// These are OK for the demo - but possibly not the correct messages for
/// a production system, where we need to drag groups of symbols as well,
/// and also select and deselect symbols, and specify real symbols, not circles
type Msg =
    /// Mouse info with coords adjusted form top-level zoom
    | MouseMsg of MouseT
    | StartDraggingSymbol of sId : CommonTypes.ComponentId * pagePos: XYPos 
    /// coords not adjusted for top-level zoom
    | DraggingSymbol of sId : CommonTypes.ComponentId * pagePos: XYPos
        /// coords not adjusted for top-level zoom
    | EndDraggingSymbol of sId : CommonTypes.ComponentId
    | AddSymbol of CommonTypes.ComponentType * busWidth: int * XYPos 
    | DeleteSymbol
    | UpdateSymbolModelWithComponent of CommonTypes.Component // Issie interface
    | BoxSelected of XYPos * XYPos
    | SymbolHovering of XYPos //not done
    | SymbolOverlap
    | ExpandPort of CommonTypes.PortType * int //not done

//---------------------------------helper types and functions----------------//
/// Radius of ports
let portRadius = 
    5.

/// Radius of expanded ports
let expandedPortRadius = 
    7.

let posDiff a b =
    {X=a.X-b.X; Y=a.Y-b.Y}

let posAdd a b =
    {X=a.X+b.X; Y=a.Y+b.Y}

let posOf x y = {X=x;Y=y}

let calcVertices (pos: XYPos) (h: int) (w: int) : XYPos List =
    let bottomLeft = {
        X = pos.X - 0.5*(float w)
        Y = pos.Y + 0.5*(float h)
    }
    let topLeft = {
        X = pos.X - 0.5*(float w)
        Y = pos.Y - 0.5*(float h)
    }
    let topRight = {
        X = pos.X + 0.5*(float w)
        Y = pos.Y - 0.5*(float h)
    }
    let bottomRight = {
        X = pos.X + 0.5*(float w)
        Y = pos.Y + 0.5*(float h)
    }
    [bottomLeft; topLeft; topRight; bottomRight]

let verticesToStr (vertices: XYPos List) : string =
    vertices
    |> List.map (fun pos -> (sprintf "%f,%f " pos.X pos.Y))
    |> List.fold (+) ""

let symInModel (symMod: Model) (sId: CommonTypes.ComponentId) : Symbol= 
    List.tryFind (fun sym -> sym.Id = sId) symMod
    |> function 
        | Some symbol -> symbol
        | None -> failwithf "Symbol not found in model"

let portInModel (symMod: Model) (pId: string) : CommonTypes.Port = 
    symMod
    |> List.collect (fun sym -> sym.InputPorts @ sym.OutputPorts)
    |> List.tryFind (fun port -> port.Id = pId)
    |> function
       | Some port -> port
       | None -> failwithf "Port not found in model"

let unwrapCompId (CommonTypes.ComponentId x) = x

let posInSymbol (sym: Symbol) (pos:XYPos) : bool= 
    let vertices = sym.Vertices
    (pos.X >= vertices.[0].X)&&(pos.Y <= vertices.[0].Y)&&(pos.X <= vertices.[2].X)&&(pos.Y >= vertices.[2].Y)

let checkIfSymbolsOverlap (symModel: Model) (sym1: Symbol) : bool = 
    let overlappedSymbol = 
        symModel
        |> List.tryFind (fun sym2 -> 
            let sym1Vertices = sym1.Vertices
            let cornerInSymbol =
                sym1Vertices
                |> List.tryFind (fun vertex -> ((posInSymbol sym2 vertex)&& (sym1 <> sym2)))
            match cornerInSymbol with 
            | Some x -> true
            | None -> false
        )

    match overlappedSymbol with 
    | Some x -> true
    | None -> false
//-----------------------------Skeleton Model Type for symbols----------------//

//-----------------------Skeleton Message type for symbols---------------------//

/// Symbol creation: a unique Id is given to the symbol, found from uuid.
/// The parameters of this function must be enough to specify the symbol completely
/// in its initial form. This is called by the AddSymbol message and need not be exposed.

let portPos 
    (inOrOut: CommonTypes.PortType) 
    (pos: XYPos) (h: int) (w:int) 
    (numberOfPorts: int) (portNumber: int): XYPos =
    {
            X = if (inOrOut = CommonTypes.PortType.Input) then 
                    pos.X - (float w/2.)
                else
                    pos.X + (float w/2.)
            Y = pos.Y - (float h/2.) + (float (h/(numberOfPorts + 1) * (portNumber+1)))
    }    

let newPortTemplate 
    (inOrOut: CommonTypes.PortType) 
    (hostId: CommonTypes.ComponentId) 
    (portNumber: int) (busWidth: int) 
    (pos: XYPos) (h:int) (w:int)
    (numberOfPorts: int): CommonTypes.Port=
    {
        Id = Helpers.uuid()
        // For example, an And would have input ports 0 and 1, and output port 0.
        // If the port is used in a Connection record as Source or Target, the Number is None. 
        PortNumber = Some portNumber
        PortType = inOrOut
        HostId = hostId
        Pos = portPos inOrOut pos h w numberOfPorts portNumber
        Width = busWidth //new field used to add bus width of ports
    }

let newSymbolTemplate 
    (comptype: CommonTypes.ComponentType) (pos:XYPos) 
    (h: int) (w: int) (numberOfInputs: int) 
    (numberOfOutputs: int) (busWidth: int): Symbol =
    let id = 
        CommonTypes.ComponentId (Helpers.uuid())
    {
        Pos = pos
        LastDragPos = {X=0. ; Y=0.} // initial value can always be this
        IsDragging = false // initial value can always be this
        IsSelected = false
        IsHovered = false
        IsOverlapped = false
        Id = id // create a unique id for this symbol
        Type = comptype
        Label = ""
        InputPorts = 
            [0..(numberOfInputs-1)]
            |> List.map (fun x -> 
                (newPortTemplate 
                    CommonTypes.PortType.Input 
                    id
                    x busWidth pos h w numberOfInputs))
        OutputPorts = 
            [0..(numberOfOutputs-1)]
            |> List.map (fun x -> 
                (newPortTemplate 
                    CommonTypes.PortType.Output 
                    id
                    x busWidth pos h w numberOfOutputs))
        ExpandedPort = None
        Vertices = calcVertices pos h w
        H = h
        W = w
    }

let createNewSymbol (comptype: CommonTypes.ComponentType) (busWidth: int) (pos:XYPos)  : Symbol =
    match comptype with 
    | Input busWidthx | Output busWidthx -> 
        newSymbolTemplate comptype pos 40 50 1 1 busWidth
    | BusSelection (outWidth, outLSBit) ->
        newSymbolTemplate comptype pos 40 50 1 1 busWidth
    | Constant (busWidthx, value) -> 
        newSymbolTemplate comptype pos 40 50 1 1 busWidth
    | IOLabel | Not ->
        newSymbolTemplate comptype pos 40 50 1 1 busWidth
    | And | Or | Xor | Nand | Nor | Xnor ->
        newSymbolTemplate comptype pos 60 50 2 1 busWidth
    | Decode4 ->
        newSymbolTemplate comptype pos 200 80 2 4 busWidth
    | Mux2 -> 
        newSymbolTemplate comptype pos 60 50 2 1 busWidth
    | Demux2 ->
        newSymbolTemplate comptype pos 60 50 1 2 busWidth
    | NbitsAdder busWidthx -> 
        newSymbolTemplate comptype pos 150 100 3 2 busWidth
    | MergeWires -> 
        newSymbolTemplate comptype pos 60 70 2 1 busWidth
    | SplitWire busWidthx ->
        newSymbolTemplate comptype pos 60 70 1 2 busWidth
    | DFF ->
        newSymbolTemplate comptype pos 60 50 2 1 busWidth
    | DFFE -> 
        newSymbolTemplate comptype pos 100 80 3 1 busWidth
    | Register busWidthx ->
        newSymbolTemplate comptype pos 100 120 2 1 busWidth
    | RegisterE busWidthx -> 
        newSymbolTemplate comptype pos 100 120 3 1 busWidth
    | AsyncROM mem ->
        newSymbolTemplate comptype pos 150 100 1 1 busWidth
    | ROM mem ->
        newSymbolTemplate comptype pos 150 100 2 1 busWidth
    | RAM mem ->
        newSymbolTemplate comptype pos 150 160 4 1 busWidth
    |_ -> failwith "Invalid Component Type"    

/// Dummy function for test. The real init would probably have no symbols.
let init () =
    [], Cmd.none


/// update function which displays symbols
let update (msg : Msg) (model : Model): Model*Cmd<'a>  =
    match msg with
    | AddSymbol (comptype, busWidth, pos) -> 
        createNewSymbol comptype busWidth pos :: model, Cmd.none
    | DeleteSymbol -> 
        List.filter (fun sym -> not sym.IsSelected) model, Cmd.none
    | BoxSelected (pos1, pos2) -> 
        model
        |> List.map (fun sym ->
            let vertices = sym.Vertices
            let x1 = pos1.X
            let y1 = pos1.Y
            let x2 = pos2.X
            let y2 = pos2.Y
            let cor1x, cor1y, cor2x, cor2y =
                if (x1 < x2)&&(y1 < y2) then
                    x1, y1, x2, y2
                elif (x1 > x2)&&(y1 > y2) then
                    x2, y2, x1, y1
                elif (x1 < x2)&&(y1 > y2)then
                    x1, y2, x2, y1
                else
                    x2, y1, x1, y2
            let inRegion = 
                (vertices.[1].X >= cor1x)&&(vertices.[1].Y >= cor1y)&&(vertices.[3].X <= cor2x)&&(vertices.[3].Y <= cor2y)
            if inRegion then 
                { sym with 
                    IsSelected = true
                }
            else
                { sym with 
                    IsSelected = false
                }
        )
        ,Cmd.none
    | StartDraggingSymbol (sId, pagePos) ->
        model
        |> List.map (fun sym ->
            if (sId <> sym.Id && not sym.IsSelected)then
                {sym with 
                    IsDragging = false
                }
            else
                { sym with
                    LastDragPos = pagePos
                    IsDragging = true
                }
        )
        , Cmd.none

    | DraggingSymbol (rank, pagePos) ->
        model
        |> List.map (fun sym ->
            if (rank <> sym.Id && not sym.IsDragging) then
                sym
            else
                let diff = posDiff pagePos sym.LastDragPos
                let numberOfInputs = List.length sym.InputPorts
                let numberOfOutputs = List.length sym.OutputPorts
                { sym with
                    Pos = posAdd sym.Pos diff
                    Vertices = calcVertices (posAdd sym.Pos diff) sym.H sym.W
                    LastDragPos = pagePos
                    InputPorts = 
                        sym.InputPorts
                        |> List.mapi (fun i port -> 
                            {port with 
                                Pos = (portPos CommonTypes.PortType.Input sym.Pos sym.H sym.W numberOfInputs i)
                            })
                    OutputPorts = 
                        sym.OutputPorts
                        |> List.mapi (fun i port -> 
                            {port with 
                                Pos = (portPos CommonTypes.PortType.Output sym.Pos sym.H sym.W numberOfOutputs i)
                            })
                }
        )
        , Cmd.none

    | EndDraggingSymbol sId ->
        model
        |> List.map (fun sym ->
            if (sId <> sym.Id && not sym.IsDragging) then 
                sym
            else
                { sym with
                    IsDragging = false 
                }
        )
        , Cmd.none
    | SymbolHovering pos ->
        model
        |> List.map (fun sym ->
            if (posInSymbol sym pos) then
                { sym with
                    IsHovered = true
                }
            else
                { sym with
                    IsHovered = false 
                }
        )
        , Cmd.none
    | SymbolOverlap ->
        model
        |> List.map (fun sym ->
            if (checkIfSymbolsOverlap model sym) then
                { sym with
                    IsOverlapped = true
                }
            else
                { sym with
                    IsOverlapped = false 
                }
        )
        , Cmd.none

    | ExpandPort (portType, portWidth) ->
        model
        |> List.map (fun sym -> 
            if (sym.InputPorts.[0].Width = portWidth) || (sym.OutputPorts.[0].Width = portWidth) then
                { sym with
                    ExpandedPort = Some portType //assuming all ports of that type within that symbol have the same bus width
                }
            else
                sym
        )
        , Cmd.none

    | MouseMsg _ -> model, Cmd.none // allow unused mouse messags
    | _ -> failwithf "Not implemented"

//----------------------------View Function for Symbols----------------------------//

/// Input to react component (which does not re-evaluate when inputs stay the same)
/// This generates View (react virtual DOM SVG elements) for one symbol
type private RenderSymbolProps =
    {
        Symbol : Symbol // name works for the demo!
        Dispatch : Dispatch<Msg>
        Key: string // special field used by react to detect whether lists have changed, set to symbol Id
    }


let notTriangle (vertices: XYPos List) (pos: XYPos) (color: string): ReactElement List =
    [polygon
        [
            SVGAttr.Points 
                (sprintf "%f, %f %f, %f %f, %f" 
                    vertices.[3].X pos.Y 
                    vertices.[3].X (pos.Y - 10.)
                    (vertices.[3].X + 10.) pos.Y 
                )
            SVGAttr.Fill color
            SVGAttr.Stroke "black"
            SVGAttr.StrokeWidth 1
        ][]
    ]

let wireLines (vertices: XYPos List) (pos: XYPos) (h: int) (splitOrMerge: bool): ReactElement List= 
    [line
        [
            X1 (if splitOrMerge then
                    vertices.[3].X; 
                else 
                    vertices.[0].X;
                )
            Y1 (vertices.[0].Y - (float h)/3.); 
            X2 pos.X; 
            Y2 (vertices.[0].Y - (float h)/3.); 
            Style [Stroke "Black"]
        ][];
    line
        [
            X1 (if splitOrMerge then
                    vertices.[3].X; 
                else 
                    vertices.[0].X;
                ) 
            Y1 (vertices.[0].Y - (float h)*2./3.); 
            X2 pos.X; 
            Y2 (vertices.[0].Y - (float h)*2./3.); 
            Style [Stroke "Black"]
        ][];
    line
        [
            X1 pos.X; 
            Y1 (vertices.[0].Y - (float h)*2./3.);
            X2 pos.X; 
            Y2 (vertices.[0].Y - (float h)/3.); 
            Style [Stroke "Black"]
            ][];
    line
        [
            X1 pos.X;
            Y1 pos.Y;
            X2 (if splitOrMerge then
                    vertices.[0].X; 
                else 
                    vertices.[3].X;
                )
            Y2 pos.Y;
            Style [Stroke "Black"]
        ][]
    ]

let private renderSymbol =
    FunctionComponent.Of(
        fun (props : RenderSymbolProps) ->
            /////////////////////Event Listener to be Removed////////////////////
            let handleMouseMove =
                Hooks.useRef(fun (ev : Types.Event) ->
                    let ev = ev :?> Types.MouseEvent
                    // x,y coordinates here do not compensate for transform in Sheet
                    // and are wrong unless zoom=1.0 MouseMsg coordinates are correctly compensated.
                    DraggingSymbol(props.Symbol.Id, posOf ev.pageX ev.pageY)
                    |> props.Dispatch
                )
            ////////////////////////////////////////////////////////////////////
            let symbolColor =
                if props.Symbol.IsDragging then
                    "lightblue"
                elif props.Symbol.IsSelected then
                    "lightgreen"
                elif props.Symbol.IsOverlapped then
                    "#ff4033"
                else
                    "lightgrey"
            let vertices = props.Symbol.Vertices
            let verticesStr =
                vertices
                |> verticesToStr

            let inputRadius, outputRadius, portFillInput, portFillOutput = 
               match props.Symbol.ExpandedPort, props.Symbol.IsHovered with
               | None, true -> portRadius, portRadius, "#2f5e5e", "#2f5e5e"
               | None, false -> portRadius, portRadius, "none", "none"
               | Some CommonTypes.PortType.Input, true -> 
                   expandedPortRadius, portRadius, "#2f5e5e", "#2f5e5e"
               | Some CommonTypes.PortType.Input, false -> 
                   expandedPortRadius, portRadius, "#2f5e5e", "none"
               | Some CommonTypes.PortType.Output, true ->
                   portRadius, expandedPortRadius, "#2f5e5e", "#2f5e5e"
               | Some CommonTypes.PortType.Output, false ->
                   portRadius, expandedPortRadius, "none", "#2f5e5e" 

            let createPortShapeAndLabel 
                (i:int) (el: string) (color: string) 
                (radius: float) (portType: CommonTypes.PortType)
                : ReactElement = 
                g[][circle [
                            if portType = CommonTypes.PortType.Input then
                                Cx props.Symbol.InputPorts.[i].Pos.X; 
                                Cy props.Symbol.InputPorts.[i].Pos.Y;
                            else
                                Cx props.Symbol.OutputPorts.[i].Pos.X; 
                                Cy props.Symbol.OutputPorts.[i].Pos.Y;
                            R radius
                            SVGAttr.Fill color
                            SVGAttr.Stroke color
                            SVGAttr.StrokeWidth 1
                        ][];

                        text [
                            if portType = CommonTypes.PortType.Input then
                                X (props.Symbol.InputPorts.[i].Pos.X + 5.); 
                                Y props.Symbol.InputPorts.[i].Pos.Y; 
                            else
                                X (props.Symbol.OutputPorts.[i].Pos.X - 5.); 
                                Y props.Symbol.OutputPorts.[i].Pos.Y; 
                            Style [
                                if portType = CommonTypes.PortType.Input then
                                    TextAnchor "start" // left/right/middle: horizontal algnment vs (X,Y)
                                else
                                    TextAnchor "end" // left/right/middle: horizontal algnment vs (X,Y)
                                DominantBaseline "middle" // auto/middle/hanging: vertical alignment vs (X,Y)
                                FontSize "10px"
                                FontWeight "Bold"
                                Fill "Black" 
                                UserSelect UserSelectOptions.None]
                        ][str <| (string el)]
                ]
            let portLabels (lst: 'T List) (inOrOut: CommonTypes.PortType): ReactElement List = 
                lst
                |> List.map (fun el -> (string el))
                |> List.mapi 
                    (fun i el -> 
                        if (inOrOut = CommonTypes.PortType.Input) then 
                            createPortShapeAndLabel i el portFillInput inputRadius inOrOut
                        else
                            createPortShapeAndLabel i el portFillOutput outputRadius inOrOut
                    )       
            let title (symbolTitle: string) : ReactElement List= 
                [text 
                    [
                        X props.Symbol.Pos.X; 
                        Y (vertices.[1].Y + 5.0); 
                        Style [
                            TextAnchor "middle" // left/right/middle: horizontal algnment vs (X,Y)
                            DominantBaseline "hanging" // auto/middle/hanging: vertical alignment vs (X,Y)
                            FontSize "10px"
                            FontWeight "Bold"
                            Fill "Black" 
                            UserSelect UserSelectOptions.None
                        ]
                    ] [str <| symbolTitle]
                ]
            
            let labels : ReactElement List = 
                match (props.Symbol.Type: CommonTypes.ComponentType) with
                | Input width -> 
                    (portLabels [0] CommonTypes.PortType.Input) 
                    @ (portLabels [0] CommonTypes.PortType.Output) 
                    @ (title "Input")
                | Output width -> 
                    (portLabels [0] CommonTypes.PortType.Input) 
                    @ (portLabels [0] CommonTypes.PortType.Output) 
                    @ (title "Output")
                | IOLabel -> 
                    (portLabels [""] CommonTypes.PortType.Input) 
                    @ (portLabels [""] CommonTypes.PortType.Output) 
                    @ (title "")
                | BusSelection (outWidth, outLSBit) ->
                    (portLabels [0] CommonTypes.PortType.Input) 
                    @ (portLabels [0] CommonTypes.PortType.Output) 
                    @ (title (sprintf "(%d:0)" (outWidth - 1)))                                    
                | Constant (width, value) -> 
                    (portLabels [0] CommonTypes.PortType.Input) 
                    @ (portLabels [0] CommonTypes.PortType.Output) 
                    @ (title (string value))
                | Not ->
                    (notTriangle vertices props.Symbol.Pos symbolColor) 
                    @ (portLabels [0] CommonTypes.PortType.Input) 
                    @ (portLabels [0] CommonTypes.PortType.Output) 
                    @ (title "1") 
                | And ->
                    (portLabels [0; 1] CommonTypes.PortType.Input) 
                    @ (portLabels [0] CommonTypes.PortType.Output) 
                    @ (title "&")
                | Or ->
                    (portLabels [0; 1] CommonTypes.PortType.Input) 
                    @ (portLabels [0] CommonTypes.PortType.Output) 
                    @ (title ">=1")
                | Xor ->
                    (portLabels [0; 1] CommonTypes.PortType.Input) 
                    @ (portLabels [0] CommonTypes.PortType.Output) 
                    @ (title "=1")
                | Nand ->
                    notTriangle vertices props.Symbol.Pos symbolColor 
                    @ (portLabels [0; 1] CommonTypes.PortType.Input) 
                    @ (portLabels [0] CommonTypes.PortType.Output) 
                    @ (title "&")
                | Nor ->
                    notTriangle vertices props.Symbol.Pos symbolColor 
                    @ (portLabels [0; 1] CommonTypes.PortType.Input) 
                    @ (portLabels [0] CommonTypes.PortType.Output) 
                    @ (title ">=1")
                | Xnor ->
                    notTriangle vertices props.Symbol.Pos symbolColor 
                    @ (portLabels [0; 1] CommonTypes.PortType.Input) 
                    @ (portLabels [0] CommonTypes.PortType.Output) 
                    @ (title "=1") 
                | Decode4 ->
                    (portLabels ["Sel"; "Data"] CommonTypes.PortType.Input) 
                    @ (portLabels [0..3] CommonTypes.PortType.Output) 
                    @ (title "decode")
                | Mux2 ->
                    (portLabels [0; 1] CommonTypes.PortType.Input) 
                    @ (portLabels [0] CommonTypes.PortType.Output) 
                    @ (title "MUX2")
                | Demux2 ->
                    (portLabels [0] CommonTypes.PortType.Input) 
                    @ (portLabels [0; 1] CommonTypes.PortType.Output) 
                    @ (title "DMUX2")
                | NbitsAdder busWidth ->
                    (portLabels ["Cin"; "A"; "B"] CommonTypes.PortType.Input) 
                    @ (portLabels ["Sum"; "Cout"] CommonTypes.PortType.Output) 
                    @ (title (sprintf "adder (%d:0)" (busWidth - 1)))
                | MergeWires -> 
                    (portLabels [0; 1] CommonTypes.PortType.Input) 
                    @ (portLabels [0] CommonTypes.PortType.Output) 
                    @ (title "MergeWires") 
                    @ wireLines vertices props.Symbol.Pos props.Symbol.H false
                | SplitWire busWidth ->
                    (portLabels [0] CommonTypes.PortType.Input) 
                    @ (portLabels [0; 1] CommonTypes.PortType.Output) 
                    @ (title "SplitWire") 
                    @ wireLines vertices props.Symbol.Pos props.Symbol.H true
                | DFF ->
                    (portLabels ["D"; "clk"] CommonTypes.PortType.Input) 
                    @ (portLabels ["Q"] CommonTypes.PortType.Output) 
                    @ (title "DFF")
                | DFFE ->
                    (portLabels ["D"; "clk"; "EN"] CommonTypes.PortType.Input) 
                    @ (portLabels ["Q"] CommonTypes.PortType.Output) 
                    @ (title "DFFE")
                | Register busWidth -> 
                    (portLabels ["data-in"; "clk"] CommonTypes.PortType.Input) 
                    @ (portLabels ["data-out"] CommonTypes.PortType.Output) 
                    @ (title (sprintf "REG%d" busWidth))
                | RegisterE busWidth ->
                    (portLabels ["data-in"; "clk"; "EN"] CommonTypes.PortType.Input) 
                    @ (portLabels ["data-out"] CommonTypes.PortType.Output) 
                    @ (title (sprintf "REG%d" busWidth))                    
                | AsyncROM mem -> 
                    (portLabels ["addr"] CommonTypes.PortType.Input) 
                    @ (portLabels ["data"] CommonTypes.PortType.Output) 
                    @ (title "Async-ROM")
                | ROM mem ->
                    (portLabels ["addr"; "clk"] CommonTypes.PortType.Input) 
                    @ (portLabels ["data"] CommonTypes.PortType.Output) 
                    @ (title "ROM")
                | RAM mem ->
                    (portLabels ["addr"; "data-in"; "write"; "clk"] CommonTypes.PortType.Input) 
                    @ (portLabels ["data-out"] CommonTypes.PortType.Output) 
                    @ (title "RAM")
                |_ -> failwith "Invalid Component Type"
            
            [
                polygon
                    [ 
                        /////////////////////Event Listener to be Removed////////////////////
                        OnMouseUp (fun ev -> 
                            document.removeEventListener("mousemove", handleMouseMove.current)
                            EndDraggingSymbol props.Symbol.Id
                            |> props.Dispatch
                        )
                        OnMouseDown (fun ev -> 
                            // See note above re coords wrong if zoom <> 1.0
                            StartDraggingSymbol (props.Symbol.Id, posOf ev.pageX ev.pageY)
                            |> props.Dispatch
                            document.addEventListener("mousemove", handleMouseMove.current)
                        )
                        /////////////////////////////////////////////////////////////////////
                        SVGAttr.Points verticesStr
                        SVGAttr.Fill symbolColor
                        SVGAttr.Stroke "black"
                        SVGAttr.StrokeWidth 1
                    ][ ]
            ] @ labels
            |> ofList     
    , "Symbol"
    , equalsButFunctions
    )

/// View function for symbol layer of SVG
let view (model : Model) (dispatch : Msg -> unit) = 
    model
    |> List.map (fun ({Id = CommonTypes.ComponentId id} as symbol) ->
        renderSymbol 
            {
                Symbol = symbol
                Dispatch = dispatch
                Key = id
            }
    )
    |> ofList


//---------------Other interface functions--------------------//

// let symbolBoundingBox (symModel: Model) (sId: CommonTypes.ComponentId) : XYPos * XYPos * XYPos * XYPos =
//     List.find (fun sym -> sym.Id = sId) symModel
//     |> (fun sym -> 
//         (sym.BoundingBox.BottomLeft, sym.BoundingBox.TopLeft, sym.BoundingBox.BottomRight, sym.BoundingBox.TopRight)
//     )

let symbolPortType (symModel: Model) (pId: string): CommonTypes.PortType = 
    portInModel symModel pId
    |> (fun chosenPort -> chosenPort.PortType)

let symbolPortWidth (symModel: Model) (pId: string): int = 
    portInModel symModel pId
    |> (fun chosenPort -> chosenPort.Width)

let symbolPortPos (symModel: Model) (pId: string): XYPos = 
    portInModel symModel pId
    |> (fun chosenPort -> chosenPort.Pos)

let findSelectedSymbol (symModel: Model): CommonTypes.ComponentId Option= //obtains symbol currently being dragged -> returns None if no symbol being dragged
    symModel
    |> List.tryFind (fun sym -> sym.IsDragging)
    |> function 
       | Some sym -> Some sym.Id
       | None -> None

let symbolPos (symModel: Model) (sId: CommonTypes.ComponentId) : XYPos = 
    symInModel symModel sId
    |> (fun sym -> sym.Pos)

let findNextAvailablePos (symModel: Model) (dimensions: float * float) = 
    let width = 
        fst dimensions

    let height = 
        snd dimensions 
    
    // checks if position available
    let checkIfPosAvailable pos = 
        let overlappedSymbol = 
            symModel
            |> List.tryFind (fun sym -> 
                                let l1 =    
                                    {X = pos.X - width - 20.; Y = pos.Y - height - 20.} // add extra gap 

                                let r1 = 
                                    {X = pos.X + width + 20.; Y = pos.Y + height + 20.} // add extra gap

                                let vertices = sym.Vertices

                                let l2 = 
                                    vertices.[1]
                               
                                let r2 = 
                                    vertices.[3]

                                if (l1.X >= r2.X || l2.X >= r1.X) then false
                                elif (l1.Y >= r2.Y || l2.Y >= r1.Y) then false
                                else true
                            )

        match overlappedSymbol with 
        | Some x -> false
        | None -> true
    
    let nextAvailablePos = 
        let listX = 
            [1..10]
            |> List.map (fun x -> float(x * 100))
       
        let listY = 
            [1..10]
            |> List.map (fun y -> float(y * 100))

        List.allPairs listX listY
        |> List.tryFind (fun (x, y) -> checkIfPosAvailable {X=x; Y=y})
    
    match nextAvailablePos with
    // if position found, create symbol in this position
    | Some avPosition -> {X = fst avPosition; Y = snd avPosition}

    // otherwise insert symbol in default position (100, 100)
    | None -> {X=100.; Y=100.;}


let findPortByPosition (symModel: Model) (pos: XYPos) : CommonTypes.Port Option =
    let cursorInPort (cursorPos: XYPos) (port: CommonTypes.Port) = 
        abs(port.Pos.X - cursorPos.X) < 7. && abs(port.Pos.Y - cursorPos.Y) < 7.
    symModel
    |> List.collect (fun sym -> sym.InputPorts @ sym.OutputPorts)
    |> List.tryFind (cursorInPort pos)
    //returns the port given a mouse position. If no port is found, return None.

let isSymbolHoveredAndSelected (symModel: Model) (pos: XYPos) : bool =   
    symModel
    |> List.tryFind (fun sym -> posInSymbol sym pos)
    |> function
       | Some sym -> true
       | None -> false 

let isSymbolSelected (symModel: Model) (sId: CommonTypes.ComponentId) : bool = 
    symInModel symModel sId
    |> (fun sym -> sym.IsSelected)
    // returns whether or not a symbol is selected

let isAnySymbolDragging (symModel: Model) : bool =
    symModel
    |> List.tryFind (fun sym -> sym.IsDragging)
    |> function
       | Some sym -> true
       | None -> false
    //check if ANY symbol is currently being dragged

/// Update the symbol with matching componentId to comp, or add a new symbol based on comp.
let updateSymbolModelWithComponent (symModel: Model) (comp:CommonTypes.Component) : Model =
    let pos = {
                X = (float comp.X)
                Y = (float comp.Y)
            }
    let newSym = 
        {
            Pos = pos
            LastDragPos = {X=0. ; Y=0.}
            IsDragging = false
            IsSelected = false
            IsHovered = false
            IsOverlapped = false
            Id = CommonTypes.ComponentId(comp.Id)
            Type = comp.Type
            Label = comp.Label
            InputPorts = comp.InputPorts
            OutputPorts = comp.OutputPorts
            Vertices = calcVertices pos comp.H comp.W
            ExpandedPort = None
            H = comp.H
            W = comp.W
        }
    let model = List.filter (fun sym -> sym.Id <> CommonTypes.ComponentId(comp.Id)) symModel
    newSym :: model

//----------------------interface to Issie-----------------------------//

let symToComp (sym: Symbol) : CommonTypes.Component= {
        Id = (unwrapCompId sym.Id)
        Type = sym.Type
        Label = sym.Label
        InputPorts = sym.InputPorts
        OutputPorts = sym.OutputPorts
        X = (int sym.Pos.X)
        Y = (int sym.Pos.Y)
        H = sym.H
        W = sym.W
    }

let extractComponent 
        (symModel: Model) 
        (sId:CommonTypes.ComponentId) : CommonTypes.Component= 

    List.tryFind (fun sym -> sym.Id = sId) symModel
    |> function
        | Some x -> symToComp x 
        | None -> failwithf "What? Symbol not found in model"

let extractComponents (symModel: Model) : CommonTypes.Component list = 
    List.map symToComp symModel
