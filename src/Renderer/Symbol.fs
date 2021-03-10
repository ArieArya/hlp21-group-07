module Symbol
open Fable.React
open Fable.React.Props
open Browser
open Elmish
open Elmish.React
open Helpers

//------------------------------------------------------------------------//
//-------------------------------Symbol Types-----------------------------//
//------------------------------------------------------------------------//

type Symbol =
    {
        Pos: XYPos
        LastDragPos : XYPos
        IsDragging : bool
        isSelected: bool
        isHovered: bool
        Id : CommonTypes.ComponentId
        Label: string
        InputPorts: CommonTypes.Port list
        OutputPorts: CommonTypes.Port list
        ExpandedPort: CommonTypes.PortType option
        Vertices: XYPos list
    }



//------------------------------------------------------------------------//
//------------------Skeleton Model Type for symbols-----------------------//
//------------------------------------------------------------------------//

type Model = Symbol list



//------------------------------------------------------------------------//
//----------------------------Message Type--------------------------------//
//------------------------------------------------------------------------//

type Msg =
    | MouseMsg of MouseT
    | StartDragging of sId : CommonTypes.ComponentId * pagePos: XYPos
    | Dragging of sId : CommonTypes.ComponentId * pagePos: XYPos
    | EndDragging of sId : CommonTypes.ComponentId
    | AddSymbol of XYPos * int * string * XYPos list * int * int 
    | DeleteSymbol 
    | UpdateSymbolModelWithComponent of CommonTypes.Component 
    | BoxSelected of XYPos * XYPos
    | SymbolHovering of XYPos
    | ExpandPort of CommonTypes.PortType * int



//------------------------------------------------------------------------//
//-----------------------Helper types and functions-----------------------//
//------------------------------------------------------------------------//

/// Radius of ports
let portRadius = 
    5.

/// Radius of expanded ports
let expandedPortRadius = 
    7.

/// Finds the difference between two XYPos components
let posDiff a b =
    {X=a.X-b.X; Y=a.Y-b.Y}

/// Adds two XYPos components
let posAdd a b =
    {X=a.X+b.X; Y=a.Y+b.Y}

/// Converts x and y integers to XYPos
let posOf x y = {X=x;Y=y}

/// Calculates the input port position for a symbol
let posOfInput (vertices: XYPos list) (index: int) (numInputs: int) = 
    let v1 = vertices.[0]
    let v4 = vertices.[3]

    let curX = v1.X
    let curY = 
        (float(index) / float(numInputs + 1)) * (v4.Y - v1.Y) + v1.Y

    {X = curX; Y=curY}

/// Calculates the output port position for a symbol
let posOfOutput (vertices: XYPos list) (index: int) (numInputs: int) = 
    let v2 = vertices.[1]
    let v3 = vertices.[2]

    let curX = v2.X
    let curY = 
        (float(index) / float(numInputs + 1)) * (v3.Y - v2.Y) + v2.Y

    {X = curX; Y=curY}

/// Converts list of coordinates to a string for SVG Polygon or Polyline. For example,
/// [(1, 2); (3, 4); (5, 6)] becomes "1,2 3,4 5,6", which is the correct SVG format
let convertPoly inpList = 
    string(inpList)
    |> String.collect (fun c -> if c = '(' || c = ')' || c = '[' || c = ']' then ""
                                elif c = ';' then " "
                                elif c = ' ' then ""
                                else string c)



//------------------------------------------------------------------------//
//---------------------Skeleton Message type for symbols------------------//
//------------------------------------------------------------------------//

/// Creates New Input Port
let createNewInputPort (portNumber: int) (pos: XYPos) (hostId: CommonTypes.ComponentId) (portWidth: int) : CommonTypes.Port = 
    {
        Id = (Helpers.uuid()) 
        PortType = CommonTypes.PortType.Input
        PortNumber = Some portNumber
        HostId = hostId
        Pos = pos
        Width = portWidth
    }

/// Creates New Output Port
let createNewOutputPort (portNumber: int) (pos: XYPos) (hostId: CommonTypes.ComponentId) (portWidth: int) : CommonTypes.Port = 
    {
        Id = (Helpers.uuid()) 
        PortType = CommonTypes.PortType.Output
        PortNumber = Some portNumber
        HostId = hostId
        Pos = pos
        Width = portWidth
    }

/// Creates New Symbol
let createNewSymbol (pos:XYPos) (portWidth:int) (compLabel:string) (vertices:XYPos list) (inputPortCount: int) (outputPortCount: int)=
    let symbolId = CommonTypes.ComponentId (Helpers.uuid())
    let totalPortCount = inputPortCount + outputPortCount
    {
        Pos = pos
        LastDragPos = {X=0. ; Y=0.} // initial value can always be this
        IsDragging = false // initial value can always be this
        isSelected = false
        isHovered = false
        Id = symbolId // create a unique id for this symbol
        Label = compLabel
        InputPorts = 
            [1..inputPortCount]
            |> List.map (fun idx -> createNewInputPort idx (posOfInput vertices idx inputPortCount) symbolId portWidth) // input port id

        OutputPorts = 
            [(inputPortCount+1)..totalPortCount]
            |> List.map (fun idx -> createNewOutputPort idx (posOfOutput vertices (idx - inputPortCount) outputPortCount) symbolId portWidth) // output port id

        ExpandedPort = None
        Vertices = vertices
    }

/// Initialization
let init () =
    [], Cmd.none

/// Update function to update Symbol models
let update (msg : Msg) (model : Model): Model*Cmd<'a>  =
    match msg with

    // Adds new symbol to model
    | AddSymbol (pos, portWidth, compName, vertices, numInputPorts, numOutputPorts) -> 
        (createNewSymbol pos portWidth compName vertices numInputPorts numOutputPorts) :: model, Cmd.none

    // Deletes symbol from model
    | DeleteSymbol -> 
        List.filter (fun sym -> not sym.isSelected) model, Cmd.none

    // Begin dragging symbol in model
    | StartDragging (sId, pagePos) ->
        model
        |> List.map (fun sym ->
            if (sId <> sym.Id && not sym.isSelected) then
                sym
            else
                { sym with
                    LastDragPos = pagePos
                    IsDragging = true
                }
        )
        , Cmd.none

    // Drag symbol to page position pagePos
    | Dragging (rank, pagePos) ->
        model
        |> List.map (fun sym ->
            if (rank <> sym.Id && not sym.IsDragging) then
                sym
                
            else
                // finds position difference
                let diff = posDiff pagePos sym.LastDragPos

                // finds new position of symbol
                let curPos = posAdd sym.Pos diff                

                // update vertices of symbol
                let curVertices = 
                    sym.Vertices
                    |> List.map (fun x -> posAdd x diff)

                // obtain number of input ports in the symbol
                let inputPortLength = 
                    sym.InputPorts.Length

                // obtain number of output ports in the symbol
                let outputPortLength = 
                    sym.OutputPorts.Length

                // obtains new position of the input ports
                let newInputPorts = 
                    sym.InputPorts
                    |> List.mapi (fun i inpPort -> {inpPort with Pos = posOfInput curVertices (i+1) inputPortLength})

                // obtains new position of the output ports
                let newOutputPorts = 
                    sym.OutputPorts
                    |> List.mapi (fun i outPort -> {outPort with Pos = posOfOutput curVertices (i+1) outputPortLength})

                // update symbol with new positions and vertices
                { sym with
                    Pos = curPos
                    InputPorts = newInputPorts
                    OutputPorts = newOutputPorts
                    LastDragPos = pagePos
                    Vertices = curVertices
                }
        ), Cmd.none

    // Stop dragging symbol
    | EndDragging sId ->
        model
        |> List.map (fun sym ->
            if (sId <> sym.Id && not sym.isSelected) then 
                sym
            else
                { sym with
                    IsDragging = false
                }
        ), Cmd.none

    // Unused Mouse Messages
    | MouseMsg _ -> model, Cmd.none 

    // Mark all symbols covered by the mouse select box as isSelected = true
    | BoxSelected (pos1, pos2) ->
        let startX, startY, endX, endY = 
            let x1 = pos1.X
            let x2 = pos2.X
            let y1 = pos1.Y
            let y2 = pos2.Y

            if x1 <= x2 && y1 <= y2 then x1, y1, x2, y2
            elif x1 <= x2 && y1 > y2 then x1, y2, x2, y1
            elif x1 > x2 && y1 <= y2 then x2, y1, x1, y2
            else x2, y2, x1, y1

        let symbolSelected (symPos: XYPos) = 
            startX <= symPos.X && endX >= symPos.X && startY <= symPos.Y && endY >= symPos.Y

        model
        |> List.map (fun sym -> 
                        if symbolSelected sym.Pos then {sym with isSelected=true} 
                        else {sym with isSelected=false}), Cmd.none

    // Mark all symbols that are being hovered by mouse at pos as isHovered = true
    | SymbolHovering pos ->
        model
        |> List.map (fun sym -> 
                        let vertices = sym.Vertices

                        // assume square vertices
                        let v1 = vertices.[0]
                        let v2 = vertices.[1]
                        let v3 = vertices.[2]

                        // perform calculation on bounding box
                        if pos.X >= v1.X && pos.X <= v2.X && pos.Y >= v2.Y && pos.Y <= v3.Y
                        then {sym with isHovered=true}
                        else {sym with isHovered=false}
                    ), Cmd.none    

    // Set the corresponding expanded port as the value of ExpandedPorts. This is used for Wire Dragging, in
    // which if an output port is dragged, input ports with the same bus width will be expanded to indicate 
    // possible connection, and vice versa.
    | ExpandPort (portType, portWidth) ->
        model
        |> List.map (fun sym -> 
                        // assume all input ports in the same symbol has the same width
                        if portWidth = sym.InputPorts.[0].Width then {sym with ExpandedPort = Some portType} 
                        else sym
                    ), Cmd.none



    // No other messages
    | _ -> model, Cmd.none



//------------------------------------------------------------------------//
//-------------------------View Function for Symbols----------------------//
//------------------------------------------------------------------------//

/// Props information for rendering shape
type private RenderShapeProps =
    {
        Shape : Symbol 
        Dispatch : Dispatch<Msg>
        key: string 
    }

/// Renders a single symbol including its ports, portNumber, and label
let private renderShape =
    FunctionComponent.Of(
        fun (props : RenderShapeProps) ->
            let handleMouseMove =
                Hooks.useRef(fun (ev : Types.Event) ->
                    let ev = ev :?> Types.MouseEvent
                    Dragging(props.Shape.Id, posOf ev.pageX ev.pageY)
                    |> props.Dispatch
                )

            // if symbol is dragged or selected, set the color to "#4fbdbd" (blue-green), else "#c7c9c9" (gray)
            let color =
                if props.Shape.IsDragging || props.Shape.isSelected then
                    "#4fbdbd"
                else
                    "#c7c9c9"
            
            // input and output radius corresponds to the radius of the input and output ports. If an input port
            // is being dragged, all output ports with the same width will have a bigger radius to indicate
            // possible connection, and vice versa.
            // portFillInput and portFillOutput shows the fill color of the ports in a symbol. If the symbol is 
            // hovered, the fill color is "#2f5e5e" (dark blue), otherwise it is none (hidden). Thus, ports are 
            // only shown if the symbol is hovered.
            let inputRadius, outputRadius, portFillInput, portFillOutput = 
                match props.Shape.ExpandedPort, props.Shape.isHovered with
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

            // obtains the list of coordinates of each vertices of symbol in a single string (for SVG Polygon)
            let polyString = 
                props.Shape.Vertices
                |> List.map (fun xypos -> (xypos.X, xypos.Y))
                |> convertPoly

            // find position of symbol label
            let textPosition = 
                let v1 = props.Shape.Vertices.[0]
                let v2 = props.Shape.Vertices.[1]

                {X=(v1.X + v2.X)/2.; Y=v2.Y + 15.}

            // obtains the label of the symbol (or component)
            let compLabel = 
                props.Shape.Label
            
            // obtains the number of input ports and output ports in the symbol
            let inpPortsLength = props.Shape.InputPorts.Length
            let outPortsLength = props.Shape.OutputPorts.Length

            [   
                // draw main component using SVG polygon               
                polygon [
                    OnMouseUp (fun ev -> 
                            document.removeEventListener("mousemove", handleMouseMove.current)
                            EndDragging props.Shape.Id
                            |> props.Dispatch
                    )

                    OnMouseDown (fun ev -> 
                        // See note above re coords wrong if zoom <> 1.0
                        StartDragging (props.Shape.Id, posOf ev.pageX ev.pageY)
                        |> props.Dispatch
                        document.addEventListener("mousemove", handleMouseMove.current)
                    )

                    Points polyString

                    SVGAttr.Fill color
                    SVGAttr.Stroke color
                    SVGAttr.StrokeWidth 2
                ][]

                // place symbol label
                text [
                        X textPosition.X
                        Y textPosition.Y
                        

                        Style [
                            TextAnchor "middle"
                            FontSize "14px"
                            FontWeight "Bold"
                            UserSelect UserSelectOptions.None
                        ]
                    ][str compLabel]
               
                // draw input ports of component
                for i in 0..(inpPortsLength-1) do
                    // draws circular ports
                    circle
                        [ 
                            Cx props.Shape.InputPorts.[i].Pos.X
                            Cy props.Shape.InputPorts.[i].Pos.Y
                            R inputRadius
                           
                            SVGAttr.Fill portFillInput
                        ]
                        []
                    // draw port number
                    text [
                        X (props.Shape.InputPorts.[i].Pos.X + 7.)
                        Y (props.Shape.InputPorts.[i].Pos.Y + 2.)

                        Style [
                            TextAnchor "middle"
                            FontSize "12px"
                            UserSelect UserSelectOptions.None
                        ]
                    ][str (string props.Shape.InputPorts.[i].PortNumber)]
                    

                // draw output ports of component
                for i in 0..(outPortsLength-1) do
                    // draws circular ports
                    circle
                        [ 
                            Cx props.Shape.OutputPorts.[i].Pos.X
                            Cy props.Shape.OutputPorts.[i].Pos.Y
                            R outputRadius
                           
                            SVGAttr.Fill portFillOutput
                        ]
                        []
                    // draw port number
                    text [
                        X (props.Shape.OutputPorts.[i].Pos.X - 7.)
                        Y (props.Shape.OutputPorts.[i].Pos.Y + 2.)

                        Style [
                            TextAnchor "middle"
                            FontSize "12px"
                            UserSelect UserSelectOptions.None
                        ]
                    ][str (string props.Shape.OutputPorts.[i].PortNumber)]
                ]
            |> ofList
    , "Shape"
    , equalsButFunctions
    )

/// View function for Symbol layer of SVG
let view (model : Model) (dispatch : Msg -> unit) = 
    model
    |> List.map (fun ({Id = CommonTypes.ComponentId id} as Shape) ->
        renderShape
            {
                Shape = Shape
                Dispatch = dispatch
                key = id
            }
    )
    |> ofList



//------------------------------------------------------------------------//
//-------------------------Other interface functions----------------------//
//------------------------------------------------------------------------//

/// Finds port position
let findPortPos (symModel: Model) (sId: CommonTypes.ComponentId) (portId: string): XYPos = 
    let selectedSymbol = 
        List.find (fun sym -> sym.Id = sId) symModel
    
    let inputPort = 
        selectedSymbol.InputPorts
        |> List.tryFind (fun port -> port.Id = portId)

    let outputPort = 
        selectedSymbol.OutputPorts
        |> List.tryFind (fun port -> port.Id = portId)

    match inputPort, outputPort with
    | Some port, None -> port.Pos
    | None, Some port -> port.Pos
    | _, _ -> failwithf "should not happen"
   
/// Finds port position without symbol id
let symbolPortPos (symModel: Model) (portId: string): XYPos = 
    // find symbol with which the port belongs to
    let selectedSymbol = 
        symModel
        |>  List.find (fun sym ->
                let findInputPort = 
                    sym.InputPorts
                    |>  List.tryFind (fun port -> portId = port.Id)
                
                let findOutputPort = 
                    sym.OutputPorts
                    |>  List.tryFind(fun port -> portId = port.Id)

                match findInputPort, findOutputPort with
                | Some _, _ -> true
                | _, Some _ -> true
                | _ -> false
            )
    
    // find the port position
    let findInputPort = 
        selectedSymbol.InputPorts
        |>  List.tryFind (fun port -> portId = port.Id)
    
    let findOutputPort = 
        selectedSymbol.OutputPorts
        |>  List.tryFind(fun port -> portId = port.Id)

    match findInputPort, findOutputPort with
    | Some port, _ -> port.Pos
    | _, Some port -> port.Pos
    | _, _ -> {X=0.; Y=0.} // this should never happen

    

/// Finds if there is a port at a certain position
let findPortByPosition (symModel: Model) (pos: XYPos) : CommonTypes.Port option = 
    let checkIfPortSelected (mousePos: XYPos) (port: CommonTypes.Port) = 
            abs(port.Pos.X - mousePos.X) < portRadius && abs(port.Pos.Y - mousePos.Y) < portRadius

    // find symbol with which the port is selected
    let selectedSymbol = 
        symModel
        |> List.tryFind (fun sym -> 
                            let checkInputPorts = 
                                sym.InputPorts
                                |> List.tryFind (fun inpPort -> checkIfPortSelected pos inpPort)
                                |> (fun x -> x <> None)
                            
                            let checkOutputPorts = 
                                sym.OutputPorts
                                |> List.tryFind (fun outPort -> checkIfPortSelected pos outPort)
                                |> (fun x -> x <> None)
                            
                            checkInputPorts || checkOutputPorts)
    
    // if no symbol is found, return None, otherwise return the port
    match selectedSymbol with
    | None -> None
    | Some sym ->
        let inputPort = 
            sym.InputPorts
            |> List.tryFind (fun inpPort -> checkIfPortSelected pos inpPort)
        
        let outputPort = 
            sym.OutputPorts
            |> List.tryFind (fun outPort -> checkIfPortSelected pos outPort)

        match inputPort, outputPort with 
        | Some inpPort, _ -> Some inpPort
        | _, Some outPort -> Some outPort
        | _, _ -> None
        
/// Finds if there is a symbol at a certain hovered position and whether it is selected
let isSymbolHoveredAndSelected (symModel: Model) (pos: XYPos) = 
    // find if there is a symbol that is hovered or selected
    let selectedSymbol = 
        symModel
        |> List.tryFind (fun sym -> 
                            let vertices = sym.Vertices

                            // assume square vertices for symbol
                            let v1 = vertices.[0]
                            let v2 = vertices.[1]
                            let v3 = vertices.[2]

                            // perform bounding box calculation
                            pos.X >= v1.X && pos.X <= v2.X && pos.Y >= v2.Y && pos.Y <= v3.Y)

    // if no symbol is found, return false, else return whether the symbol is selected               
    match selectedSymbol with
    | None -> false
    | Some sym ->
        sym.isSelected

/// Checks if symbol is selected
let isSymbolSelected (symModel: Model) (sId: CommonTypes.ComponentId) : bool = 
    let curSymbol = 
        symModel
        |> List.find (fun sym -> sym.Id = sId)
    curSymbol.isSelected

/// Checks if any symbol is dragging
let isAnySymbolDragging (symModel: Model) : bool = 
    symModel
    |> List.exists (fun sym -> sym.IsDragging)

/// Finds next available position to insert new symbol
let findNextAvailablePos (symModel: Model) (dimensions: float * float) = 
    // obtains width of new inserted symbol
    let width = 
        fst dimensions

    // obtains height of new inserted symbol
    let height = 
        snd dimensions 
    
    // checks if position available
    let checkIfPosAvailable pos = 
        let overlappedSymbol = 
            symModel
            |> List.tryFind (fun sym -> 
                                // perform bounding box calculation to see whether there is any symbol overlapping
                                let l1 =    
                                    {X = pos.X - width - 20.; Y = pos.Y - height - 20.} 

                                let r1 = 
                                    {X = pos.X + width + 20.; Y = pos.Y + height + 20.} 

                                let vertices = sym.Vertices

                                let l2 = 
                                    vertices.[0]
                               
                                let r2 = 
                                    vertices.[2]

                                if (l1.X >= r2.X || l2.X >= r1.X) then false
                                else not (l1.Y >= r2.Y || l2.Y >= r1.Y)
                            )

        // if there is an overlapping, return false (i.e. position not available), otherwise true
        match overlappedSymbol with 
        | Some x -> false
        | None -> true
    
    // finds next available position in the canvas
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



//------------------------------------------------------------------------//
//---------------------------interface to Issie---------------------------//
//------------------------------------------------------------------------//

// interface for Issie currently not used
let extractComponent 
        (symModel: Model) 
        (sId:CommonTypes.ComponentId) : CommonTypes.Component= 
    failwithf "Not implemented"
let extractComponents (symModel: Model) : CommonTypes.Component list = 
    failwithf "Not implemented"
