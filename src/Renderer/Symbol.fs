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

type Symbol =
    {
        Pos: XYPos
        LastDragPos : XYPos
        IsDragging : bool
        IsSelected: bool
        IsHovered: bool
        IsCopied: bool
        IsOverlapped: bool
        Id : CommonTypes.ComponentId
        Type: CommonTypes.ComponentType
        Label: string
        InputPorts: CommonTypes.Port list
        OutputPorts: CommonTypes.Port list
        ExpandedPort: (CommonTypes.PortType option * int option)
        Vertices: XYPos list
        SymbolNumber: int
        H: int
        W: int
        OriginCopiedId: CommonTypes.ComponentId
    }



//------------------------------------------------------------------------//
//---------------------- Model Type for symbols --------------------------//
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
    | AddSymbol of CommonTypes.ComponentType * XYPos * string
    | DeleteSymbol 
    | BoxSelected of XYPos * XYPos * bool
    | SymbolHovering of XYPos
    | SymbolOverlap
    | ExpandPort of CommonTypes.PortType * int option
    | CopySymbols
    | PasteSymbols of XYPos
    | ClickSymbol of (XYPos * bool)
    | ClearOriginCopiedId
    | SelectAllSymbols
    | SaveModel
    | ErrorHighlightPorts of CommonTypes.Port List
    | UpdateComponentLabel of CommonTypes.Component * string
    | UpdateComponentWidth of CommonTypes.Component * int



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

/// Finds the position of ports on top of Symbol
let portPos 
    (inOrOut: CommonTypes.PortType) 
    (vertices: XYPos list)
    (numberOfPorts: int) (portNumber: int): XYPos =
    let v1 = vertices.[0]
    let v2 = vertices.[1]
    let v3 = vertices.[2]

    let curX = 
            if (inOrOut = CommonTypes.PortType.Input) then 
                v1.X
            else
                v2.X
    let curY = 
        (float(portNumber) / float(numberOfPorts + 1)) * (v3.Y - v2.Y) + v2.Y
    {
            X = curX
            Y = curY
    }  

/// Calculates vertices position
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
    [topLeft; topRight; bottomRight; bottomLeft]
 
/// Converts vertices to string
let verticesToStr (vertices: XYPos List) : string =
    vertices
    |> List.map (fun pos -> (sprintf "%f,%f " pos.X pos.Y))
    |> List.fold (+) ""

/// Get unique symbol number (for automated symbol numbering)
let getNextSymNumber (symModel: Model) (compType: CommonTypes.ComponentType) : int = 
    // obtain list of symbols with the same component type
    let sameCompSymNumber = 
        symModel 
        |> List.filter (fun sym ->
                            match sym.Type, compType with
                            | Input _, Input _ -> true
                            | Output _, Output _ -> true
                            | BusSelection _, BusSelection _ -> true
                            | Constant _, Constant _ -> true
                            | NbitsAdder _, NbitsAdder _ -> true
                            | Register _, Register _ -> true
                            | RegisterE _, RegisterE _ -> true
                            | AsyncROM _, AsyncROM _ -> true
                            | ROM _, ROM _ -> true
                            | RAM _, RAM _ -> true
                            | SplitWire _, SplitWire _ -> true
                            | Custom _, Custom _ -> true
                            | _ ->
                                sym.Type = compType
                            )
        |> List.map (fun x -> x.SymbolNumber)
    
    // finds the minimum number available
    match sameCompSymNumber.Length with 
    | 0 -> 1
    | _ -> 
        [1..(sameCompSymNumber.Length+1)]
        |> List.find (fun x -> not (List.contains x sameCompSymNumber))


/// Obtains new numbered symbol label
let getNextLabel (compType: CommonTypes.ComponentType) (symNumber: int) : string = 
    match compType with 
    | Input _ -> "IN" + (string symNumber)
    | Output _ -> "OUT" + (string symNumber)
    | IOLabel -> ""
    | BusSelection _ ->  ""                                
    | Constant _ -> "CONST" + (string symNumber)
    | Not -> "NOT" + (string symNumber)
    | And -> "AND" + (string symNumber)
    | Or -> "OR" + (string symNumber)
    | Xor -> "XOR" + (string symNumber)
    | Nand -> "NAND" + (string symNumber)
    | Nor -> "NOR" + (string symNumber)
    | Xnor -> "XNOR" + (string symNumber)
    | Decode4 -> "DEC" + (string symNumber)
    | Mux2 -> "MUX" + (string symNumber)
    | Demux2 -> "DEMUX" + (string symNumber)
    | NbitsAdder _ -> "ADDR" + (string symNumber)
    | MergeWires -> ""
    | SplitWire _ -> ""
    | DFF -> "DFF" + (string symNumber)
    | DFFE -> "DFFE" + (string symNumber)
    | Register _ -> "REG" + (string symNumber)
    | RegisterE _ -> "REGE" + (string symNumber)                   
    | AsyncROM _ -> "Async-ROM" + (string symNumber)
    | ROM _ -> "Sync-ROM" + (string symNumber)
    | RAM _ -> "RAM" + (string symNumber)
    | Custom _ -> "CUST" + (string symNumber)

// checks if a given position lies within a given symbol
let posInSymbol (sym: Symbol) (pos:XYPos) : bool= 
    let vertices = sym.Vertices

    // assume square vertices for symbol
    let v1 = vertices.[0]
    let v2 = vertices.[1]
    let v3 = vertices.[2]
    
    // perform bounding box calculation
    (pos.X >= v1.X) && 
    (pos.X <= v2.X) && 
    (pos.Y >= v2.Y) && 
    (pos.Y <= v3.Y)

// checks if two symbols bounding boxes overlap
let checkIfSymbolsOverlap (symModel: Model) (sym1: Symbol) : bool = 
    let overlappedSymbol = 
        symModel
        |> List.tryFind (fun sym2 -> 
            let sym1Vertices = sym1.Vertices
            let cornerInSymbol =
                sym1Vertices
                |> List.tryFind (fun vertex -> 
                    ((posInSymbol sym2 vertex)&& (sym1 <> sym2)))
            match cornerInSymbol with 
            | Some _ -> true
            | None -> false
        )

    match overlappedSymbol with 
    | Some _ -> true
    | None -> false

// obtains the string from the component id
let unwrapCompId (CommonTypes.ComponentId x) = x

// remove error highlighting from all ports
let removeErrorHighlight (symModel: Model) = 
    symModel
    |> List.map (fun sym -> 
                    let newInpPorts = 
                        sym.InputPorts
                        |> List.map (fun port -> {port with ErrorHighlight = false})

                    let newOutPorts = 
                        sym.OutputPorts
                        |> List.map (fun port -> {port with ErrorHighlight = false})

                    {sym with InputPorts=newInpPorts; OutputPorts=newOutPorts})


//--------------------------------------------------------------------//
//---------------------Skeleton Message type for symbols------------------//
//------------------------------------------------------------------------//

/// Template for new Ports
let newPortTemplate 
    (inOrOut: CommonTypes.PortType) 
    (hostId: CommonTypes.ComponentId) 
    (portNumber: int) (busWidth: int option) 
    (vertices: XYPos List)
    (numberOfPorts: int): CommonTypes.Port =
    
    {
        Id = uuid()
        // For example, an And would have input ports 0 and 1, and output port 0.
        // If the port is used in a Connection record as Source or Target, the Number is None. 
        PortNumber = Some portNumber
        PortType = inOrOut
        HostId = hostId
        Pos = portPos inOrOut vertices numberOfPorts portNumber
        Width = busWidth //new field used to add bus width of ports
        ErrorHighlight = false
    }

/// Template for new Symbols
let newSymbolTemplate 
    (compType: CommonTypes.ComponentType) (pos:XYPos) 
    (h: int) (w: int) (inputPortWidthList: (int option) list) 
    (outputPortWidthList: (int option) list) (compName: string) (model: Model): Symbol =
    
    // obtain unique id
    let id = 
        CommonTypes.ComponentId (uuid())
    
    // determine number of input and output ports
    let numberOfInputs = inputPortWidthList.Length
    let numberOfOutputs = outputPortWidthList.Length

    // determine unique symbol numbering
    let symNumber = getNextSymNumber model compType

    // obtain automatically numbered symbol label
    let compLabel = 
        match compType with 
        | IOLabel -> compName
        | BusSelection (outWidth, outLSBit) -> 
            (sprintf "(%d:%d)" ((outWidth - 1) + outLSBit) (outLSBit))
        | _ -> getNextLabel compType symNumber

    // obtain symbol vertices
    let vertices = calcVertices pos h w

    // create record to hold symbol model
    {
        Pos = pos
        LastDragPos = {X=0. ; Y=0.} 
        IsDragging = false 
        IsSelected = false
        IsHovered = false
        IsCopied = false
        IsOverlapped = false
        Id = id 
        Type = compType
        Label = compLabel
        InputPorts = 
            [0..(inputPortWidthList.Length-1)]
            |> List.map (fun x -> 
                (newPortTemplate 
                    CommonTypes.PortType.Input 
                    id
                    (x+1) inputPortWidthList.[x] vertices numberOfInputs))
        OutputPorts = 
            [0..(outputPortWidthList.Length-1)]
            |> List.map (fun x -> 
                (newPortTemplate 
                    CommonTypes.PortType.Output 
                    id
                    (x+1) outputPortWidthList.[x] vertices numberOfOutputs))
        ExpandedPort = (None, None)
        Vertices = vertices
        SymbolNumber = symNumber
        H = h
        W = w
        OriginCopiedId = CommonTypes.ComponentId "0" 
    }

/// Creates New Symbol
let createNewSymbol 
    (comptype: CommonTypes.ComponentType) (compName: string) (pos: XYPos) 
    (model: Model) (extraComps: Symbol List): Symbol =
    
    // create symbol based on its defined component type
    // port width is of type 'int option' to allow width inference
    match comptype with 
    | Input busWidthx ->
        newSymbolTemplate comptype pos 20 60 [Some 1] [Some busWidthx] (compName) (model @ extraComps)
    | Output busWidthx -> 
        newSymbolTemplate comptype pos 20 60 [Some busWidthx] [Some 1] (compName)(model @ extraComps)
    | BusSelection (outWidth, outLSBit) ->
        newSymbolTemplate comptype pos 20 60 [None] [Some outWidth] (compName) (model @ extraComps)
    | Constant (busWidthx, value) -> 
        newSymbolTemplate comptype pos 20 60 [Some 1] [Some busWidthx] (compName) (model @ extraComps)
    | IOLabel ->
        newSymbolTemplate comptype pos 20 60 [None] [None] (compName) (model @ extraComps)
    | Not ->
        newSymbolTemplate comptype pos 60 60 [Some 1] [Some 1] (compName) (model @ extraComps)
    | And | Or | Xor | Nand | Nor | Xnor ->
        newSymbolTemplate comptype pos 60 60 [Some 1; Some 1] [Some 1] (compName) (model @ extraComps)
    | Decode4 ->
        newSymbolTemplate comptype pos 180 100 [Some 1; Some 1] [Some 1; Some 1; Some 1; Some 1] (compName) (model @ extraComps)
    | Mux2 -> 
        newSymbolTemplate comptype pos 60 60 [Some 1; Some 1] [Some 1] (compName) (model @ extraComps)
    | Demux2 ->
        newSymbolTemplate comptype pos 60 60 [Some 1] [Some 1; Some 1] (compName) (model @ extraComps)
    | NbitsAdder busWidthx -> 
        newSymbolTemplate comptype pos 140 100 [Some 1; Some busWidthx; Some busWidthx] [Some busWidthx; Some 1] (compName) (model @ extraComps)
    | MergeWires -> 
        newSymbolTemplate comptype pos 100 100 [None; None] [None] (compName) (model @ extraComps)
    | SplitWire busWidthx ->
        newSymbolTemplate comptype pos 100 100 [None] [Some busWidthx; None] (compName) (model @ extraComps)
    | DFF ->
        newSymbolTemplate comptype pos 100 60 [Some 1; Some 1] [Some 1] (compName) (model @ extraComps)
    | DFFE -> 
        newSymbolTemplate comptype pos 100 60 [Some 1; Some 1; Some 1] [Some 1] (compName) (model @ extraComps)
    | Register busWidthx ->
        newSymbolTemplate comptype pos 140 100 [Some busWidthx; Some 1] [Some busWidthx] (compName) (model @ extraComps)
    | RegisterE busWidthx -> 
        newSymbolTemplate comptype pos 140 100 [Some busWidthx; Some 1; Some 1] [Some busWidthx] (compName) (model @ extraComps)
    | AsyncROM mem ->
        newSymbolTemplate comptype pos 140 100 [Some mem.AddressWidth] [Some mem.WordWidth] (compName) (model @ extraComps)
    | ROM mem ->
        newSymbolTemplate comptype pos 140 100 [Some mem.AddressWidth; Some 1] [Some mem.WordWidth] (compName) (model @ extraComps)
    | RAM mem ->
        newSymbolTemplate comptype pos 140 140 [Some mem.AddressWidth; Some mem.WordWidth; Some 1; Some 1] [Some mem.WordWidth] (compName) (model @ extraComps)
    | Custom custCompType ->
        // obtain variable input port widths
        let inpPortWidths = 
            custCompType.InputLabels
            |> List.map (fun x -> Some (snd x))
        
        // obtain variable output port widths
        let outPortWidths = 
            custCompType.OutputLabels
            |> List.map (fun x -> Some (snd x))

        let height = 
            match custCompType.InputLabels.Length >= custCompType.OutputLabels.Length with 
            | true -> custCompType.InputLabels.Length * 40 + 20
            | false -> custCompType.OutputLabels.Length * 40 + 20

        newSymbolTemplate comptype pos height 140 inpPortWidths outPortWidths (compName) (model @ extraComps)


/// Initialization
let init () =
    [], Cmd.none

/// Update function to update Symbol models
let update (msg : Msg) (model : Model): Model*Cmd<'a>  =
    match msg with

    // Adds new symbol to model
    | AddSymbol (comptype, pos, compName) -> 
        // reset all symbol to remove error highlighting
        let newModel = removeErrorHighlight model
            
        createNewSymbol comptype compName pos model [] :: newModel, Cmd.none

    // Deletes symbol from model
    | DeleteSymbol -> 
        // reset all symbol to remove error highlighting
        let newModel = removeErrorHighlight model

        List.filter (fun sym -> not sym.IsSelected) newModel, Cmd.none

    // Begin dragging symbol in model
    | StartDragging (sId, pagePos) ->
        model
        |> List.map (fun sym ->
            if (sId <> sym.Id && not sym.IsSelected) then
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
                    |> List.mapi (fun i inpPort -> 
                        {inpPort with Pos = portPos CommonTypes.PortType.Input curVertices inputPortLength (i+1)})

                // obtains new position of the output ports
                let newOutputPorts = 
                    sym.OutputPorts
                    |> List.mapi (fun i outPort -> 
                        {outPort with Pos = portPos CommonTypes.PortType.Output curVertices outputPortLength (i+1)})

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
            if (sId <> sym.Id && not sym.IsSelected) then 
                sym
            else
                let cornerPointX = sym.Vertices.[0].X
                let cornerPointY = sym.Vertices.[0].Y

                // perform vertical snap-to-grid
                let diffY = 
                    let nearestFloorGridValue = floor(cornerPointY / 20.) * 20.
                    if (cornerPointY - nearestFloorGridValue) > 10. then 
                        ((nearestFloorGridValue + 20.) - cornerPointY)
                    else 
                        (nearestFloorGridValue - cornerPointY)

                // perform horizontal snap-to-grid
                let diffX = 
                    let nearestFloorGridValue = floor(cornerPointX / 20.) * 20.
                    if (cornerPointX - nearestFloorGridValue) > 10. then 
                        ((nearestFloorGridValue + 20.) - cornerPointX)
                    else 
                        (nearestFloorGridValue - cornerPointX)

                let diff = 
                    {X = diffX; Y = diffY}

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
                    |> List.mapi (fun i inpPort -> 
                        {inpPort with Pos = portPos CommonTypes.PortType.Input curVertices inputPortLength (i+1)})

                // obtains new position of the output ports
                let newOutputPorts = 
                    sym.OutputPorts
                    |> List.mapi (fun i outPort -> 
                        {outPort with Pos = portPos CommonTypes.PortType.Output curVertices outputPortLength (i+1)})

                // update new positions after snap-to-grid
                { sym with
                    Pos = curPos
                    InputPorts = newInputPorts
                    OutputPorts = newOutputPorts
                    LastDragPos = curPos
                    Vertices = curVertices
                    IsDragging = false
                }                           

        ), Cmd.none

    // Unused Mouse Messages
    | MouseMsg _ -> model, Cmd.none 

    // Mark all symbols covered by the mouse select box as IsSelected = true
    | BoxSelected (pos1, pos2, isCtrlPressed) ->
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
                        if symbolSelected sym.Pos then 
                            {sym with IsSelected=true; ExpandedPort = (None, None)} 
                        else 
                            if isCtrlPressed then 
                                {sym with ExpandedPort = (None, None)}
                            else 
                                {sym with IsSelected=false; ExpandedPort = (None, None)}), Cmd.none

    // Mark all symbols that are being hovered by mouse at pos as IsHovered = true
    | SymbolHovering pos ->
        model
        |> List.map (fun sym ->
            if (posInSymbol sym pos) then // checks if the position is in the symbol
                { sym with
                    IsHovered = true
                }
            else
                { sym with
                    IsHovered = false 
                }
        )
        , Cmd.none  

    // Mark all symbols that overlap as IsOverlapped = true
    | SymbolOverlap ->
        model
        |> List.map (fun sym ->
            if (checkIfSymbolsOverlap model sym) then //check if the symbols overlap
                { sym with
                    IsOverlapped = true
                }
            else
                { sym with
                    IsOverlapped = false 
                }
        )
        , Cmd.none

    // Set the corresponding expanded port as the value of ExpandedPorts. This is used for Wire Dragging, in
    // which if an output port is dragged, input ports with the same bus width will be expanded to indicate 
    // possible connection, and vice versa.
    | ExpandPort (portType, portWidth) ->
        model
        |> List.map (fun sym -> 
                        match sym.Type with
                        | BusSelection _ -> sym
                        | MergeWires -> sym
                        | SplitWire _ -> sym
                        | _ ->
                            {sym with ExpandedPort = (Some portType, portWidth)}
                    ), Cmd.none
    
    // Mark all selected symbols (i.e. IsSelected=true) to isCopied=true
    | CopySymbols ->
        model 
        |> List.map (fun sym ->
                        if sym.IsSelected then 
                            {sym with IsCopied = true}
                        else 
                            {sym with IsCopied = false}
                        ), Cmd.none

    // Duplicate symbols with isCopied=true (with its own unique ID)
    | PasteSymbols pasteMargin ->
        // reset all symbol to remove error highlighting
        let newModel = removeErrorHighlight model

        // recursively create new symbols - recursion is required to automatically number symbols
        let rec getNewModel (curModel: Model) (newComponents: Symbol List) = 
            match curModel with
            | (sym::tl) when sym.IsCopied ->
                let newSymbol = 
                    createNewSymbol sym.Type sym.Label (posAdd sym.Pos pasteMargin) model newComponents
                
                // for variable port widths, ensure their widths are all the same when pasted
                let newInputPorts =    
                    [0..(newSymbol.InputPorts.Length-1)] 
                    |> List.map (fun i -> 
                        {newSymbol.InputPorts.[i] with Width=sym.InputPorts.[i].Width})
                
                // for variable port widths, ensure their widths are all the same when pasted
                let newOutputPorts =    
                    [0..(newSymbol.OutputPorts.Length-1)] 
                    |> List.map (fun i -> 
                        {newSymbol.OutputPorts.[i] with Width=sym.OutputPorts.[i].Width})

                {sym with IsSelected = false; IsCopied=false}::
                    (
                        {newSymbol with 
                            IsSelected=true; 
                            OriginCopiedId=sym.Id; 
                            InputPorts=newInputPorts; 
                            OutputPorts=newOutputPorts
                        }::(getNewModel tl (newComponents @ [newSymbol]))
                    )
            | (sym::tl) -> sym::(getNewModel tl newComponents)
            | [] -> []

        getNewModel newModel [], Cmd.none

    // Marks all symbols clicked (while ctrl is pressed) to IsSelected=true
    | ClickSymbol (mousePos, isCtrlPressed) -> 
        let isSymbolClicked symbol = 
            let vertices = symbol.Vertices
            // assume square vertices
            let v1 = vertices.[0]
            let v2 = vertices.[1]
            let v3 = vertices.[2]

            // perform calculation on bounding box
            mousePos.X >= v1.X && mousePos.X <= v2.X && mousePos.Y >= v2.Y && mousePos.Y <= v3.Y

        model
        |> List.map (fun sym ->
            if isCtrlPressed && (isSymbolClicked sym) then 
                if sym.IsSelected then 
                    {sym with IsSelected=false}
                else 
                    {sym with IsSelected=true}
            else 
                sym
            ), Cmd.none

    // Resets OriginCopiedId to 0 - used when copy-pasting symbols and wires
    | ClearOriginCopiedId ->
        model
        |> List.map (fun sym -> 
            {sym with OriginCopiedId=CommonTypes.ComponentId "0"}), Cmd.none

    // Marks all symbols as IsSelected - used when user presses Ctrl+A
    | SelectAllSymbols ->
        model
        |> List.map (fun sym -> 
            {sym with IsSelected=true}), Cmd.none

    // When user performs an action, the previous model is saved (for undo purposes) 
    // All symbol settings (IsDragging, IsHovered, IsSelected) is reset
    | SaveModel ->
        // reset all symbol to remove error highlighting
        let newModel = removeErrorHighlight model

        newModel
        |> List.map (fun sym -> 
            {sym with IsDragging=false; IsHovered=false; IsSelected=false}), Cmd.none

    // highlights all the ports on symbols which are not connected to anything, in red
    | ErrorHighlightPorts (conPortList: CommonTypes.Port List) ->
        model
        |> List.map (fun sym -> 
            // update all ports in list to be error highlighted if not in connected port list
            let updatePorts (portList: CommonTypes.Port List) = 
                portList
                |> List.map (fun port ->
                        // check if each port of the symbol is in the connected port list
                        let checkIfInPortList = 
                            conPortList
                            |> List.tryFind (fun unconPort -> unconPort.Id = port.Id)
                            |> function 
                                | Some x -> true
                                | None -> false

                        // store the current value of error highlight bool for the port
                        let firstErrorHighlight = port.ErrorHighlight

                        // if the port is not in the connected port list then set the errorhighlight to true
                        if (checkIfInPortList) then                          
                            port
                        else
                            {port with ErrorHighlight = not firstErrorHighlight}
                    )
            // update all input ports in symbol to be error highlighted if in unconnected port list
            let updatedInputPorts = 
                sym.InputPorts 
                |> updatePorts
            
            // update all output ports in symbol to be error highlighted if in unconnected port list
            let updatedOutputPorts = 
                sym.OutputPorts 
                |> updatePorts

            // update symbol with new updated ports       
            {sym with InputPorts = updatedInputPorts; OutputPorts = updatedOutputPorts}
           ), Cmd.none

    //updates the label for a given symbol
    | UpdateComponentLabel (comp, newLabel) ->
        model
        |> List.map (fun sym -> 
                        if (unwrapCompId sym.Id) = comp.Id then
                            {sym with Label=newLabel}
                        else
                            sym
                    ), Cmd.none

    //updates the width for a given symbol
    | UpdateComponentWidth (comp, newWidth) ->
        model
        |> List.map (fun sym -> 
            // update all ports in list to be error highlighted if not in connected port list
            let updatePorts (portList: CommonTypes.Port List)= 
                portList
                |> List.map (fun port ->
                    {port with Width=Some newWidth})
                 
            let updatedInputPorts = 
                sym.InputPorts 
                |> updatePorts
            
            let updatedOutputPorts = 
                sym.OutputPorts 
                |> updatePorts
            if (unwrapCompId sym.Id) = comp.Id then
                {sym with InputPorts = updatedInputPorts; OutputPorts = updatedOutputPorts}
            else 
                sym
           ), Cmd.none
           
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

/// Draws triangle for inverted output
let notTriangle 
    (vertices: XYPos List) (pos: XYPos) 
    (color: string) (strokeColor: string): ReactElement List =
    [polygon
        [
            SVGAttr.Points 
                (sprintf "%f, %f %f, %f %f, %f" 
                    vertices.[1].X pos.Y 
                    vertices.[1].X (pos.Y - 10.)
                    (vertices.[1].X + 10.) pos.Y 
                )
            SVGAttr.Fill color
            SVGAttr.Stroke strokeColor
            SVGAttr.StrokeWidth 1
        ][]
    ]

/// Draws lines for MergeWire and SplitWire
let wireLines 
    (vertices: XYPos List) (pos: XYPos) 
    (h: int) (splitOrMerge: bool): ReactElement List= 
    [line
        [
            X1 (if splitOrMerge then
                    vertices.[2].X; 
                else 
                    vertices.[3].X;
                )
            Y1 (vertices.[3].Y - (float h)/3.); 
            X2 pos.X; 
            Y2 (vertices.[3].Y - (float h)/3.); 
            Style [Stroke "Black"]
        ][];
    line
        [
            X1 (if splitOrMerge then
                    vertices.[2].X; 
                else 
                    vertices.[3].X;
                ) 
            Y1 (vertices.[3].Y - (float h)*2./3.); 
            X2 pos.X; 
            Y2 (vertices.[3].Y - (float h)*2./3.); 
            Style [Stroke "Black"]
        ][];
    line
        [
            X1 pos.X; 
            Y1 (vertices.[3].Y - (float h)*2./3.);
            X2 pos.X; 
            Y2 (vertices.[3].Y - (float h)/3.); 
            Style [Stroke "Black"]
            ][];
    line
        [
            X1 pos.X;
            Y1 pos.Y;
            X2 (if splitOrMerge then
                    vertices.[3].X; 
                else 
                    vertices.[2].X;
                )
            Y2 pos.Y;
            Style [Stroke "Black"]
        ][]
    ]
 
/// Draws triangle for CLK
let clockTriangle
    (vertices: XYPos List) (clkPortPos: XYPos): ReactElement List =
    [
        line
            [
                X1 clkPortPos.X; 
                Y1 (clkPortPos.Y + 5.);
                X2 (clkPortPos.X + 5.); 
                Y2 clkPortPos.Y; 
                Style [Stroke "Black"]
            ][];
        line
            [
                X1 (clkPortPos.X); 
                Y1 (clkPortPos.Y - 5.);
                X2 (clkPortPos.X + 5.); 
                Y2 clkPortPos.Y;
                Style [Stroke "Black"]
            ][]
    ]

/// Renders a single symbol
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
                if props.Shape.IsDragging || props.Shape.IsSelected then
                    "#4fbdbd"
                else
                    "#c7c9c9"

            let opacity = 
                match props.Shape.Type with
                    | MergeWires -> "0.0"
                    | SplitWire _ -> "0.0"
                    | _ -> "1.0"
            
            // input and output radius corresponds to the radius of the input and output ports. If an input port
            // is being dragged, all output ports with the same width will have a bigger radius to indicate
            // possible connection, and vice versa.
            // portFillInput and portFillOutput shows the fill color of the ports in a symbol. If the symbol is 
            // hovered, the fill color is "#2f5e5e" (dark blue), otherwise it is none (hidden). Thus, ports are 
            // only shown if the symbol is hovered.

            // obtains color of stroke and dash (when copied) or when the symbols overlap
            let strokeColor = 
                if props.Shape.IsCopied || props.Shape.IsOverlapped then 
                    "#133f6b"
                else color

            let strokeDashArray = 
                if props.Shape.IsCopied then "2, 2"
                else 
                    match props.Shape.Type with
                    | MergeWires -> "1, 1"
                    | SplitWire _ -> "1, 1"
                    | _ -> "none"

            let vertices = props.Shape.Vertices
            let verticesStr =
                vertices
                |> verticesToStr

            let createPortShapeAndLabel 
                (i:int) (el: string)
                (portType: CommonTypes.PortType)
                : ReactElement = 

                let curPort = 
                    match portType with 
                    | CommonTypes.PortType.Input -> 
                        props.Shape.InputPorts.[i]
                    | CommonTypes.PortType.Output -> 
                        props.Shape.OutputPorts.[i]

                let portRadius, portFill = 
                    match props.Shape.Type with
                    | IOLabel | SplitWire _ | BusSelection _ | MergeWires -> portRadius, "#1b80b3"
                    | _ ->
                        match props.Shape.IsHovered, portType, fst props.Shape.ExpandedPort, snd props.Shape.ExpandedPort with
                        | _, CommonTypes.PortType.Input, Some CommonTypes.PortType.Input, width when width = curPort.Width -> 
                            expandedPortRadius, "#2f5e5e"
                        | _, CommonTypes.PortType.Output, Some CommonTypes.PortType.Output, width when width = curPort.Width -> 
                            expandedPortRadius, "#2f5e5e"
                        | true, _, None, None -> 
                            portRadius, "#2f5e5e"
                        | _ -> 
                            portRadius, "none"
                
                //updates portfill color to be red if error highlight is true for that port
                let addErrorHighlightToColor = 
                    if portType = CommonTypes.PortType.Input then
                        if (props.Shape.InputPorts.[i].ErrorHighlight) then
                            "red"
                        else 
                            portFill
                    else
                        if (props.Shape.OutputPorts.[i].ErrorHighlight) then
                            "red"
                        else
                            portFill

                // draws port in symbol
                g[][circle [
                            if portType = CommonTypes.PortType.Input then
                                Cx props.Shape.InputPorts.[i].Pos.X; 
                                Cy props.Shape.InputPorts.[i].Pos.Y;
                            else
                                Cx props.Shape.OutputPorts.[i].Pos.X; 
                                Cy props.Shape.OutputPorts.[i].Pos.Y;
                            R portRadius
                            SVGAttr.Fill addErrorHighlightToColor
                            SVGAttr.Stroke addErrorHighlightToColor
                            SVGAttr.StrokeWidth 1
                        ][];

                        text [
                            if portType = CommonTypes.PortType.Input then
                                X (props.Shape.InputPorts.[i].Pos.X + 5.); 
                                Y props.Shape.InputPorts.[i].Pos.Y; 
                            else
                                X (props.Shape.OutputPorts.[i].Pos.X - 5.); 
                                Y props.Shape.OutputPorts.[i].Pos.Y; 
                            Style [
                                if portType = CommonTypes.PortType.Input then
                                    TextAnchor "start" // left/right/middle: horizontal algnment vs (X,Y)
                                else
                                    TextAnchor "end" // left/right/middle: horizontal algnment vs (X,Y)
                                DominantBaseline "middle" // auto/middle/hanging: vertical alignment vs (X,Y)
                                FontSize "10px"
                                Fill "Black" 
                                UserSelect UserSelectOptions.None]
                        ][str <| (string el)]
                ]

            // obtain port and label
            let portLabels (lst: 'T List) (inOrOut: CommonTypes.PortType): ReactElement List = 
                lst
                |> List.map (fun el -> (string el))
                |> List.mapi 
                    (fun i el -> 
                        if (inOrOut = CommonTypes.PortType.Input) then 
                            createPortShapeAndLabel i el inOrOut
                        else
                            createPortShapeAndLabel i el inOrOut
                    )
            
            // optain label on top of a symbol
            let topLabel (symLabel: string) : ReactElement List = 
                [text 
                    [
                        X props.Shape.Pos.X; 
                        Y (vertices.[1].Y - 25.0); 
                        Style [
                            TextAnchor "middle" // left/right/middle: horizontal algnment vs (X,Y)
                            DominantBaseline "hanging" // auto/middle/hanging: vertical alignment vs (X,Y)
                            FontSize "13px"
                            FontWeight "Bold"
                            Fill "Black" 
                            UserSelect UserSelectOptions.None
                        ]
                    ] [str <| symLabel]
                ]

            // obtain symbol title 
            let title (symbolTitle: string) : ReactElement List = 
                [text 
                    [
                        X props.Shape.Pos.X; 
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
            
            // obtain all symbol labels based on symbol component type
            let labels : ReactElement List = 
                match (props.Shape.Type: CommonTypes.ComponentType) with
                | Input width -> 
                    (portLabels [0] CommonTypes.PortType.Output) 
                    @ (title "") 
                | Output width -> 
                    (portLabels [0] CommonTypes.PortType.Input)
                    @ (title "") 
                | IOLabel -> 
                    (portLabels [""] CommonTypes.PortType.Input) 
                    @ (portLabels [""] CommonTypes.PortType.Output) 
                    @ (title "") 
                | BusSelection (outWidth, outLSBit) ->
                    (portLabels [0] CommonTypes.PortType.Input) 
                    @ (portLabels [1] CommonTypes.PortType.Output)                            
                | Constant (width, value) -> 
                    (portLabels [""] CommonTypes.PortType.Output) 
                    @ (title (string value)) 
                | Not ->
                    notTriangle vertices props.Shape.Pos color strokeColor
                    @ (portLabels [0] CommonTypes.PortType.Input) 
                    @ (portLabels [1] CommonTypes.PortType.Output) 
                    @ (title "1")
                | And ->
                    (portLabels [0; 1] CommonTypes.PortType.Input) 
                    @ (portLabels [2] CommonTypes.PortType.Output) 
                    @ (title "&")
                | Or ->
                    (portLabels [0; 1] CommonTypes.PortType.Input) 
                    @ (portLabels [2] CommonTypes.PortType.Output) 
                    @ (title ">=1")
                | Xor ->
                    (portLabels [0; 1] CommonTypes.PortType.Input) 
                    @ (portLabels [2] CommonTypes.PortType.Output) 
                    @ (title "=1")
                | Nand ->
                    notTriangle vertices props.Shape.Pos color strokeColor
                    @ (portLabels [0; 1] CommonTypes.PortType.Input) 
                    @ (portLabels [2] CommonTypes.PortType.Output) 
                    @ (title "&")
                | Nor ->
                    notTriangle vertices props.Shape.Pos color strokeColor
                    @ (portLabels [0; 1] CommonTypes.PortType.Input) 
                    @ (portLabels [2] CommonTypes.PortType.Output) 
                    @ (title ">=1")
                | Xnor ->
                    notTriangle vertices props.Shape.Pos color strokeColor 
                    @ (portLabels [0; 1] CommonTypes.PortType.Input) 
                    @ (portLabels [2] CommonTypes.PortType.Output) 
                    @ (title "=1")
                | Decode4 ->
                    (portLabels ["Sel"; "Data"] CommonTypes.PortType.Input) 
                    @ (portLabels [0..3] CommonTypes.PortType.Output) 
                    @ (title "decode")
                | Mux2 ->
                    (portLabels [0; 1] CommonTypes.PortType.Input) 
                    @ (portLabels [2] CommonTypes.PortType.Output) 
                    @ (title "MUX2")
                | Demux2 ->
                    (portLabels [0] CommonTypes.PortType.Input) 
                    @ (portLabels [1; 2] CommonTypes.PortType.Output) 
                    @ (title "DMUX2")
                | NbitsAdder busWidth ->
                    (portLabels ["Cin"; "A"; "B"] CommonTypes.PortType.Input) 
                    @ (portLabels ["Sum"; "Cout"] CommonTypes.PortType.Output) 
                    @ (title (sprintf "adder (%d:0)" (busWidth - 1)))
                | MergeWires -> 
                    (portLabels [""; ""] CommonTypes.PortType.Input) 
                    @ (portLabels [""] CommonTypes.PortType.Output) 
                    @ (title "MergeWires") 
                    @ wireLines vertices props.Shape.Pos props.Shape.H false
                | SplitWire busWidth ->
                    (portLabels [""] CommonTypes.PortType.Input) 
                    @ (portLabels [""; ""] CommonTypes.PortType.Output) 
                    @ (title "SplitWire") 
                    @ wireLines vertices props.Shape.Pos props.Shape.H true
                | DFF ->
                    clockTriangle vertices props.Shape.InputPorts.[1].Pos 
                    @ (portLabels ["D"; "clk"] CommonTypes.PortType.Input) 
                    @ (portLabels ["Q"] CommonTypes.PortType.Output) 
                    @ (title "DFF")
                | DFFE ->
                    clockTriangle vertices props.Shape.InputPorts.[1].Pos 
                    @ (portLabels ["D"; "clk"; "EN"] CommonTypes.PortType.Input) 
                    @ (portLabels ["Q"] CommonTypes.PortType.Output) 
                    @ (title "DFFE")
                | Register busWidth -> 
                    clockTriangle vertices props.Shape.InputPorts.[1].Pos 
                    @ (portLabels ["data-in"; "clk"] CommonTypes.PortType.Input) 
                    @ (portLabels ["data-out"] CommonTypes.PortType.Output) 
                    @ (title (sprintf "REG%d" busWidth))
                | RegisterE busWidth ->
                    clockTriangle vertices props.Shape.InputPorts.[1].Pos 
                    @ (portLabels ["data-in"; "clk"; "EN"] CommonTypes.PortType.Input) 
                    @ (portLabels ["data-out"] CommonTypes.PortType.Output) 
                    @ (title (sprintf "REG%d" busWidth))                    
                | AsyncROM mem -> 
                    (portLabels ["addr"] CommonTypes.PortType.Input) 
                    @ (portLabels ["data"] CommonTypes.PortType.Output) 
                    @ (title "Async-ROM")
                | ROM mem ->
                    clockTriangle vertices props.Shape.InputPorts.[1].Pos 
                    @ (portLabels ["addr"; "clk"] CommonTypes.PortType.Input) 
                    @ (portLabels ["data"] CommonTypes.PortType.Output) 
                    @ (title "ROM")
                | RAM mem ->
                    clockTriangle vertices props.Shape.InputPorts.[3].Pos 
                    @ (portLabels ["addr"; "data-in"; "write"; "clk"] CommonTypes.PortType.Input) 
                    @ (portLabels ["data-out"] CommonTypes.PortType.Output) 
                    @ (title "RAM")
                | Custom custCompType ->
                    let inputNameList = 
                        custCompType.InputLabels
                        |> List.map (fun x -> fst x)
                   
                    let outputNameList = 
                        custCompType.OutputLabels
                        |> List.map (fun x -> fst x)

                    (portLabels inputNameList CommonTypes.PortType.Input) 
                    @ (portLabels outputNameList CommonTypes.PortType.Output) 
                    @ (title custCompType.Name)
            
            // draws shape for symbol
            [
                polygon
                    [ 
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
                        SVGAttr.Points verticesStr
                        SVGAttr.Fill color
                        SVGAttr.FillOpacity opacity
                        SVGAttr.StrokeWidth 1
                        SVGAttr.Stroke strokeColor
                        SVGAttr.StrokeDasharray strokeDashArray
                    ][ ]
            ] @ labels @ (topLabel props.Shape.Label)
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

// returns a list of vertices from top left, clockwise to bottom left
let symbolBoundingBox (symModel: Model) (sId: CommonTypes.ComponentId) : XYPos List =
    List.find (fun sym -> sym.Id = sId) symModel
    |> (fun sym -> 
        sym.Vertices
    ) 

/// Gets Symbol by Id
let findSymbolById (symModel: Model) (sId: CommonTypes.ComponentId) : Symbol = 
    symModel
    |> List.find (fun x -> x.Id = sId)

/// Gets Symbol by OriginCopiedId
let findSymbolByOriginCopiedId (symModel: Model) (sId: CommonTypes.ComponentId) : Symbol = 
    symModel
    |> List.find (fun x -> x.OriginCopiedId = sId)

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
    symModel
    |> List.tryFind (fun sym -> posInSymbol sym pos)
    |> function     
        | Some sym -> 
            sym.IsSelected
        | None -> false 

/// Checks if symbol is selected
let isSymbolSelected (symModel: Model) (sId: CommonTypes.ComponentId) : bool = 
    let curSymbol = 
        symModel
        |> List.find (fun sym -> sym.Id = sId)
    curSymbol.IsSelected

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
                                    {X = pos.X - width - 20.; Y = pos.Y - height - 35.} 

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
        | Some _ -> false
        | None -> true
    
    // finds next available position in the canvas
    let nextAvailablePos = 
        let listX = 
            [5..37]
            |> List.collect (fun x -> 
                                if x % 2 = 1 then [float(x * 30)]
                                else []
                                )
       
        let listY = 
            [5..240]
            |> List.collect (fun y -> 
                                if y % 2 = 1 then [float(y * 30)]
                                else []
                                )

        List.allPairs listY listX
        |> List.tryFind (fun (y, x) -> checkIfPosAvailable {X=x; Y=y})
    
    match nextAvailablePos with
    // if position found, create symbol in this position
    | Some avPosition -> {X = snd avPosition; Y = fst avPosition}

    // otherwise insert symbol in default position (100, 100)
    | None -> {X=100.; Y=100.;}


/// Check if two ports can be connected by width inference. If possible, return new symbol model
/// with modified ports and return true
let portInference (symModel: Model) (port: CommonTypes.Port) (portWidth: int) : (Model * bool) = 
    // find symbol with the host id of the port
    let hostSymbol = 
        symModel
        |> List.find (fun sym -> sym.Id = port.HostId)

    // create new symbol based on new port width
    let newHostSymbol = 
        let newInpPorts = 
            hostSymbol.InputPorts
            |> List.map (fun curPort -> 
                            if curPort.Id = port.Id then {curPort with Width = Some portWidth}
                            else curPort)
        let newOutPorts = 
            hostSymbol.OutputPorts
            |> List.map (fun curPort -> 
                            if curPort.Id = port.Id then {curPort with Width = Some portWidth}
                            else curPort)
        {hostSymbol with InputPorts=newInpPorts; OutputPorts=newOutPorts}

    // check if new symbol is valid
    let updatedSymbol, isValid = 
        match newHostSymbol.Type with
        // width inference for split-wire
        | SplitWire _ ->
            let inpPort = newHostSymbol.InputPorts.[0]
            let outPort2 = newHostSymbol.OutputPorts.[1]

            // this is user-defined, should always hold a value
            let outPort1 = newHostSymbol.OutputPorts.[0]

            // return updated symbol from width inference
            match outPort1.Width, inpPort.Width, outPort2.Width with
            | Some x, Some y, Some z -> 
                (newHostSymbol, y = x + z)
            | Some x, Some y, None -> 
                ({newHostSymbol with OutputPorts=[outPort1; {outPort2 with Width = Some(y - x)}]}, y > x)
            | Some x, None, Some z -> 
                ({newHostSymbol with InputPorts=[{inpPort with Width = Some(x + z)}]}, true)
            | _ -> 
                (newHostSymbol, false)
        
        // width inference for merge-wire
        | MergeWires ->
            let inpPort1 = newHostSymbol.InputPorts.[0]
            let inpPort2 = newHostSymbol.InputPorts.[1]
            let outPort = newHostSymbol.OutputPorts.[0]

            // return updated symbol from width inference
            match inpPort1.Width, inpPort2.Width, outPort.Width with
            | Some x, Some y, Some z -> 
                (newHostSymbol, z = x + y)
            | Some x, Some y, None -> 
                ({newHostSymbol with OutputPorts=[{outPort with Width = Some(x + y)}]}, true)
            | Some x, None, Some z -> 
                ({newHostSymbol with InputPorts=[inpPort1; {inpPort2 with Width = Some(z - x)}]}, z > x)
            | None, Some y, Some z -> 
                ({newHostSymbol with InputPorts=[{inpPort1 with Width = Some(z - y)}; inpPort2]}, z > y)
            | Some x, None, None -> 
                (newHostSymbol, true)
            | None, Some y, None -> 
                (newHostSymbol, true)
            | None, None, Some z -> 
                (newHostSymbol, true)
            | None, None, None -> 
                (newHostSymbol, false)

        // width inference for bus-selection
        | BusSelection (outWidth, outLSBit) ->
            let inpPort = newHostSymbol.InputPorts.[0]

            // return updated symbol from width inference
            match inpPort.Width with
            | Some width ->
                if width >= outLSBit + outWidth then (newHostSymbol, true)
                else (newHostSymbol, false)
            | None ->
                (newHostSymbol, false)

        // width inference for IOLabel
        | IOLabel ->
            let inpPort = newHostSymbol.InputPorts.[0]
            let outPort = newHostSymbol.OutputPorts.[0]

            // return updated symbol from width inference
            match inpPort.Width, outPort.Width with 
            | Some x, _ -> 
                ({newHostSymbol with OutputPorts=[{outPort with Width = Some(x)}]}, true)
            | _, Some x -> 
                ({newHostSymbol with InputPorts=[{inpPort with Width = Some(x)}]}, true)
            | _ -> 
                (newHostSymbol, false)

        // no width inference needed for any other symbol
        | _ ->
            (newHostSymbol, false)

    // if width inference is valid, return the new model and a bool stating it is valid
    match updatedSymbol, isValid with 
    | newSymbol, true ->
        let newSymModel = 
            symModel
            |> List.map (fun sym -> if sym.Id = hostSymbol.Id then newSymbol
                                    else sym)
        (newSymModel, true)

    | _ ->
        (symModel, false)


/// When deleting a symbol, variable port widths must be reset for width inference
let variablePortReset (symModel: Model) (port: CommonTypes.Port) : Model = 
    // find symbol with the host id of the port
    let hostSymbol = 
        symModel
        |> List.find (fun sym -> sym.Id = port.HostId)

    // obtain new symbol (with updated ports) after the connection from port is deleted
    let newSymbol = 
        match hostSymbol.Type with
        // width-inference for IOLabel
        | IOLabel -> hostSymbol

        // width-inference for BusSelection
        | BusSelection _ ->
            let inpPort = hostSymbol.InputPorts.[0]

            if inpPort.Id = port.Id then 
                {hostSymbol with InputPorts=[{inpPort with Width = None}]}
            else 
                hostSymbol

        // width-inference for MergeWires
        | MergeWires ->
            let inpPort1 = hostSymbol.InputPorts.[0]
            let inpPort2 = hostSymbol.InputPorts.[1]
            let outPort = hostSymbol.OutputPorts.[0]

            if inpPort1.Id = port.Id then 
                {hostSymbol with InputPorts=[{inpPort1 with Width = None}; inpPort2]}
            else if inpPort2.Id = port.Id then 
                {hostSymbol with InputPorts=[inpPort1; {inpPort2 with Width = None}]}
            else if outPort.Id = port.Id then 
                {hostSymbol with OutputPorts=[{outPort with Width = None}]}
            else 
                hostSymbol
        
        // width-inference for SplitWire
        | SplitWire _ ->
            let inpPort = hostSymbol.InputPorts.[0]
            let outPort2 = hostSymbol.OutputPorts.[1]

            // this is user-defined, should always hold a value
            let outPort1 = hostSymbol.OutputPorts.[0]

            if inpPort.Id = port.Id then 
                {hostSymbol with InputPorts=[{inpPort with Width = None}]}
            else if outPort2.Id = port.Id then 
                {hostSymbol with OutputPorts=[outPort1; {outPort2 with Width = None}]}
            else 
                hostSymbol
        
        // no width inference needed for any other symbol
        | _ -> hostSymbol

    // return new Symbol model
    symModel
    |> List.map (fun sym -> 
        if sym.Id = hostSymbol.Id then 
            newSymbol
        else 
            sym)
                            
/// Determines if a port belongs to a symbol
let doesPortBelongToSymbol (portId: string) (symbol: Symbol) = 
    let sameId (id: string) (port: CommonTypes.Port) = 
        id = port.Id 
    List.exists (sameId portId) (symbol.InputPorts) || List.exists (sameId portId) (symbol.OutputPorts)

/// Finds the symbol which the port belongs to
let findSymbolFromPort (symbolModel: Model) (portId: string) = 
    List.tryFind (doesPortBelongToSymbol portId) symbolModel 

/// Determines if a port's host symbol is being dragged
let isSymbolBeingDragged (symbolModel: Model) (portId: string) = 
    let symbolFound = findSymbolFromPort symbolModel portId
    match symbolFound with  
    | Some x -> x.IsDragging
    | None -> false


//------------------------------------------------------------------------//
//---------------------------interface to Issie---------------------------//
//------------------------------------------------------------------------//

/// Update the symbol with matching componentId to comp, or add a new symbol based on comp.
let updateSymbolModelWithComponent (symModel: Model) (comp:CommonTypes.Component) : Model =
    let pos = {
                X = (float comp.X)
                Y = (float comp.Y)
            }

    let checkIfSymbolInModel : Symbol option = 
        symModel
        |> List.tryFind (fun sym -> sym.Id = CommonTypes.ComponentId(comp.Id))
    
    match checkIfSymbolInModel with
    | Some sym ->
        // update the model by removing the existing symbol with that component Id
        let updatedSymModel = 
            symModel
            |> List.filter (fun sym -> sym.Id <> CommonTypes.ComponentId(comp.Id))

        // create updated symbol based on the component given
        let updatedSym =
            let symNumber = getNextSymNumber symModel comp.Type 
            {
                Pos = pos
                LastDragPos = {X=0. ; Y=0.}
                IsDragging = false
                IsSelected = false
                IsHovered = false
                IsCopied = false
                IsOverlapped = false
                Id = CommonTypes.ComponentId(comp.Id)
                Type = comp.Type
                Label = comp.Label
                InputPorts = comp.InputPorts
                OutputPorts = comp.OutputPorts
                Vertices = calcVertices pos comp.H comp.W
                SymbolNumber = symNumber
                ExpandedPort = (None, None)
                H = comp.H
                W = comp.W
                OriginCopiedId = CommonTypes.ComponentId "0" // default copied id
            }

        // add the updated symbol to the head of the model
        updatedSym :: updatedSymModel
    | None ->
        // create a new symbol based on the component given
        let newSym : Symbol = 
            createNewSymbol comp.Type comp.Label pos symModel []
        
        // add the new symbol to the head of the model
        newSym :: symModel

// converts the symbol into a component type, which is used in Issie
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

// extracts the symbol from the model and converts it to a component type before returning
let extractComponent 
        (symModel: Model) 
        (sId:CommonTypes.ComponentId) : CommonTypes.Component= 

    List.tryFind (fun sym -> sym.Id = sId) symModel
    |> function
        | Some x -> symToComp x 
        | None -> failwithf "What? Symbol not found in model"

// extracts the list of symbols from the model and converts them to the component type
let extractComponents (symModel: Model) : CommonTypes.Component list = 
    List.map symToComp symModel

// extracts component type
let extractComponentType (comp: CommonTypes.Component) : CommonTypes.ComponentType = 
    comp.Type

//------------------------------------------------------------------------//
//---------------------------For Properties View--------------------------//
//------------------------------------------------------------------------//

let findSelectedSymbol (symModel: Model) : CommonTypes.Component option = 
    let selcomp = 
        symModel
        |> List.tryFind (fun sym -> sym.IsSelected = true)  
    match selcomp with  
        | Some sym -> Some (symToComp sym) 
        | None -> None