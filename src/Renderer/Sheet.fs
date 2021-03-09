module Sheet
open Fable.React
open Fable.React.Props
open Browser
open Elmish
open Elmish.React
open Helpers

//------------------------------------------------------------------------//
//------------------------------Sheets Types------------------------------//
//------------------------------------------------------------------------//

type CompInfo = {
    PortWidth: int
    ComponentLabel: string
    NumInputPorts: int
    NumOutputPorts: int
    }

type Model = {
    Wire: BusWire.Model
    ComponentInfo: CompInfo
    DragBox: (XYPos * XYPos)
    }



//------------------------------------------------------------------------//
//---------------------------Message Type---------------------------------//
//------------------------------------------------------------------------//

type KeyboardMsg =
    | CtrlS | AltC | AltV | AltZ | AltShiftZ | DEL

type Msg =
    | Wire of BusWire.Msg
    | KeyPress of KeyboardMsg
    | CreateSymbol of float * float
    | ChangeWireWidth of int
    | ChangeCompLabel of string
    | ChangeNumInputPorts of int
    | ChangeNumOutputPorts of int
    | MouseMsg of MouseT



//------------------------------------------------------------------------//
//---------------------------Helper Functions-----------------------------//
//------------------------------------------------------------------------//

// obtains the right-side menu to obtain user inputs (e.g. symbol type, name
// of components, number of input and output ports, port width, etc.)
let rightColumn = 
    Style [
        Position PositionOptions.Fixed
        Right "0px"
        Top "0px"
        Height  "100vh"
        Width "25%"
        BorderTop "0.4vh solid lightgray"
        BorderLeft "0.4vh solid lightgray"
        BorderBottom "0.4vh solid lightgray"
        Margin "0"
        Padding "0"
        UserSelect UserSelectOptions.None
        ZIndex 31
        BackgroundColor "#f2f2f2"
    ]

// obtain canvas height and width from CommonTypes
let canvasHeight = CommonTypes.draw2dCanvasHeight
let canvasWidth = CommonTypes.draw2dCanvasWidth

// draws initial grid lines for the canvas
let drawGrid = 
    
    // obtain number of horizontal lines
    let gridHeightCount = canvasHeight / 20

    // obtain number of vertical lines
    let gridWidthCount = canvasWidth / 20

    // draw vertical grid lines
    let gridVertical = 
        [0..gridWidthCount]
        |> List.map (fun x -> 
            // draw vertical lines
            line [X1 (float(x * 20)); Y1 0.; X2 (float(x * 20)); Y2 (float(canvasHeight)); Style [Stroke "lightgray"]] [
            ]
        )

    // draw horizontal grid lines
    let gridHorizontal = 
        [0..gridHeightCount]
        |> List.map (fun x -> 
            // draw horizontal lines
            line [X1 0.; Y1 (float(x * 20)); X2 (float(canvasWidth)); Y2 (float(x * 20)); Style [Stroke "lightgray"]] [
            ]
        )
    
    // return a ReactElement list of the vertical and horizontal lines
    gridVertical @ gridHorizontal

/// Converts list of coordinates to a string for SVG Polygon or Polyline. For example,
/// [(1, 2); (3, 4); (5, 6)] becomes "1,2 3,4 5,6", which is the correct SVG format
let convertPoly inpList = 
    string(inpList)
    |> String.collect (fun c -> if c = '(' || c = ')' || c = '[' || c = ']' then ""
                                elif c = ';' then " "
                                elif c = ' ' then ""
                                else string c)



//------------------------------------------------------------------------//
//-------------------------View Function for Sheets-----------------------//
//------------------------------------------------------------------------//

let zoom = 1.0

/// Renders entire SVG canvas with zoom and scroll capability
let displaySvgWithZoom (model: Model) (zoom:float) (svgReact: ReactElement) (dispatch: Dispatch<Msg>)=
    let sizeInPixels = sprintf "%.2fpx" ((1100. * zoom))

    // check if mouse is currently down
    let mDown (ev:Types.MouseEvent) = 
        ev.buttons <> 0. 

    // dispatch a Sheet MouseMsg message if mouse event detected
    let mouseOp op (ev:Types.MouseEvent) = 
        let posX = (ev.pageX) / zoom
        let posY = (ev.pageY) / zoom
        dispatch (MouseMsg {Op = op ; Pos = {X = posX ; Y = posY}})

    // define the dragBox - the box in which users can select multiple symbols
    let dragBox = 
            // obtain the two corners of the dragging box
            let pos1 = fst model.DragBox
            let pos2 = snd model.DragBox

            // infer the four vertices of the dragging box
            let startingPos = (pos1.X, pos1.Y)
            let targetPos = (pos2.X, pos2.Y)
            let intermediatePos1 = (pos1.X, pos2.Y)
            let intermediatePos2 = (pos2.X, pos1.Y)
            
            // convert the four vertices into correct string format for SVG display
            let displayString = 
                convertPoly ([startingPos] @ [intermediatePos1] @ [targetPos] @ [intermediatePos2])

            // draw SVG polygon for the dragging box
            polygon [
                Points displayString
                SVGAttr.Fill "none"
                SVGAttr.Stroke "black"
                SVGAttr.StrokeDasharray "5, 5"
            ][]

    // obtains the canvas for drawing the canvas grid and the busWire view
    let baseCanvas = drawGrid @ [svgReact] @ [dragBox]

    // draws the SVG canvas
    div [ Style 
            [ 
                CSSProp.OverflowX OverflowOptions.Hidden
                CSSProp.OverflowY OverflowOptions.Auto
            ] 

          OnMouseDown (fun ev -> (mouseOp Down ev))
          OnMouseUp (fun ev -> (mouseOp Up ev))
          OnMouseMove (fun ev -> mouseOp (if mDown ev then Drag else Move) ev)
        ]
        
        [            
          svg
            [ Style 
                [
                    BorderTop "0.4vh solid lightgray"
                    Height canvasHeight
                    Width canvasWidth          
                ]
            ]
            [ g 
                [ Style [Transform (sprintf "scale(%f)" zoom)]] 

                // draws the base canvas
                baseCanvas
                
            ]

          // draws the right-side menu column (for demo purposes only - i.e. to input different symbols)
          div [ rightColumn ][
              
              div [ Style [Height "100%"; Width "100%"; TextAlign TextAlignOptions.Center]]
                  [
                      // module selection title
                      div [ Style [PaddingTop "5vh"]][
                            text [ 
                                Style [
                                    TextAnchor "middle" 
                                    DominantBaseline "middle" 
                                    FontSize "3vh"
                                    FontWeight "Bold"
                                    Fill "Gray" 
                                ]
                            ] [str "Module Selection"]
                      ] 
                      
                      // user input component name
                      div [ Style [PaddingTop "5vh"]][
                          input [   
                                    Type "text"
                                    Placeholder "component name"
                                    OnChange (fun ev -> dispatch (ChangeCompLabel (ev.Value)))

                                    Style [
                                        Width "50%"
                                        FontSize "1.5vh"
                                        Height "3vh"
                                    ]
                                ]
                      ]
                       
                      // user input port width
                      div [ Style [PaddingTop "2vh"]][
                          input [
                                    Type "number"
                                    Placeholder "port width"
                                    OnChange (fun ev -> dispatch (ChangeWireWidth (int ev.Value)))

                                    Style [
                                        Width "50%"
                                        FontSize "1.5vh"
                                        Height "3vh"
                                    ]
                                ]
                      ]
                      
                      // user input number of input ports
                      div [ Style [PaddingTop "2vh"]][
                          input [
                                    Type "number"
                                    Placeholder "number of input ports"
                                    OnChange (fun ev -> dispatch (ChangeNumInputPorts (int ev.Value)))

                                    Style [
                                        Width "50%"
                                        FontSize "1.5vh"
                                        Height "3vh"
                                    ]
                                ]
                      ]

                      // user input number of output ports
                      div [ Style [PaddingTop "2vh"]][
                          input [   
                                    Type "number"
                                    Placeholder "number of output ports"
                                    OnChange (fun ev -> dispatch (ChangeNumOutputPorts (int ev.Value)))

                                    Style [
                                        Width "50%"
                                        FontSize "1.5vh"
                                        Height "3vh"
                                    ]
                                ]
                      ]

                      // user select to create module 1
                      div [ Style [PaddingTop "4vh"; Margin "0"; PaddingBottom "0"]][
                            a [
                                Style [
                                    Height "5vh"
                                    TextAnchor "middle" 
                                    DominantBaseline "middle" 
                                    FontSize "2.4vh"
                                    FontWeight "Bold"
                                    Fill "Gray" 
                                ]
                                OnClick (fun _ -> dispatch (CreateSymbol (40., 60.)))
                            ][str "Module 1"]
                      ] 

                      // user select to create module 2
                      div [ Style [PaddingTop "4vh"; Margin "0"; PaddingBottom "0"]][
                            a [
                                Style [
                                    Height "5vh"
                                    TextAnchor "middle" // horizontal algnment vs (X,Y)
                                    DominantBaseline "middle" // vertical alignment vs (X,Y)
                                    FontSize "2.4vh"
                                    FontWeight "Bold"
                                    Fill "Gray" // font color
                                ]
                                OnClick (fun _ -> dispatch (CreateSymbol (60., 90.)))
                            ][str "Module 2"]
                      ]
                      
                      // user select to create module 3
                      div [ Style [PaddingTop "4vh"; Margin "0"; PaddingBottom "0"]][
                            a [
                                Style [
                                    Height "5vh"
                                    TextAnchor "middle" // horizontal algnment vs (X,Y)
                                    DominantBaseline "middle" // vertical alignment vs (X,Y)
                                    FontSize "2.4vh"
                                    FontWeight "Bold"
                                    Fill "Gray" // font color
                                ]
                                OnClick (fun _ -> dispatch (CreateSymbol (70., 70.)))
                            ][str "Module 3"]
                      ]
                  ]
            ]
        ]
        

/// View function for Sheets layer of SVG
let view (model:Model) (dispatch : Msg -> unit) =
    let wDispatch wMsg = dispatch (Wire wMsg)
    let wireSvg = BusWire.view model.Wire wDispatch
    displaySvgWithZoom model zoom wireSvg dispatch
       
/// Update function to update Sheets models
let update (msg : Msg) (model : Model): Model*Cmd<Msg> =
    match msg with
    // messages to update wires
    | Wire wMsg -> 
        let wModel, wCmd = BusWire.update wMsg model.Wire
        {model with Wire = wModel}, Cmd.map Wire wCmd

    // print and reset the performance statistics in dev tools window
    | KeyPress AltShiftZ -> 
        printStats() 
        model, Cmd.none

    // draws new symbol
    | CreateSymbol (width, height) ->

        // finds position to insert new symbol (i.e. ensuring no collision with existing symbols)
        let pos = Symbol.findNextAvailablePos (model.Wire.Symbol) (width, height)

        // define symbol vertices from: top left -> top right -> bottom right -> bottom left
        let vertices = [{X=pos.X - width; Y=pos.Y - height}; {X=pos.X + width; Y=pos.Y - height}; {X=pos.X + width; Y=pos.Y + height}; {X=pos.X - width; Y=pos.Y + height}]

        // all other information comes from user input
        let portWidth = model.ComponentInfo.PortWidth
        let label = model.ComponentInfo.ComponentLabel
        let inputPortLength = model.ComponentInfo.NumInputPorts
        let outputPortLength = model.ComponentInfo.NumOutputPorts

        // return updated model
        let newModel, newCmd = Symbol.update (Symbol.Msg.AddSymbol (pos, portWidth, label, vertices, inputPortLength, outputPortLength)) model.Wire.Symbol
        {model with Wire = {model.Wire with Symbol = newModel}}, newCmd

    // deletes symbol
    | KeyPress DEL ->
        // find whether a wire is selected
        let selectedWireId = BusWire.findSelectedWire (model.Wire)

        match selectedWireId with
        // if wire is selected, delete the wire
        | Some wireId ->
            let newModel, _ = BusWire.update (BusWire.Msg.DeleteWire wireId) model.Wire
            {model with Wire = newModel}, Cmd.none

        // otherwise, delete all symbols and wires connected to it
        | _ -> 
            let newModel, _ = BusWire.update (BusWire.Msg.DeleteWiresBySymbol) model.Wire
            {model with Wire = newModel}, Cmd.none

    // sets color for model
    | KeyPress s -> 
        let c =
            match s with
            | AltC -> CommonTypes.Blue
            | AltZ -> CommonTypes.Red
            | _ -> CommonTypes.Grey
        printfn "Key:%A" c
        model, Cmd.ofMsg (Wire <| BusWire.SetColor c)

    // change the port width to user-defined port width
    | ChangeWireWidth portWidth ->
        {model with ComponentInfo = {model.ComponentInfo with PortWidth = portWidth}}, Cmd.none

    // change component label to user-defined component label
    | ChangeCompLabel compName ->
        {model with ComponentInfo = {model.ComponentInfo with ComponentLabel = compName}}, Cmd.none

    // change number of input ports to user-defined number of input ports
    | ChangeNumInputPorts numInputPorts ->
        {model with ComponentInfo = {model.ComponentInfo with NumInputPorts = numInputPorts}}, Cmd.none

    // change number of output ports to user-defined number of output ports
    | ChangeNumOutputPorts numOutputPorts ->
        {model with ComponentInfo = {model.ComponentInfo with NumOutputPorts = numOutputPorts}}, Cmd.none

    // handles mouse operations
    | MouseMsg mMsg ->
        // obtain the mouse operation and its position
        let operation = mMsg.Op
        let pos = mMsg.Pos

        match operation with 
        // mouse down
        | Down -> 
            // initialize DragBox
            let newDragBox = (pos, pos)

            // try find a port corresponding to the mouse position
            let selectedPort = Symbol.findPortByPosition model.Wire.Symbol pos

            // if no port is selected, return the original model, otherwise update the ports
            // Note: for current implementation, Symbol uses event listeners and so no checking
            // performed for symbol being clicked. Once integrated, symbol will also be checked.
            match selectedPort with
            | None -> model, Cmd.none
            | Some port -> 
                // check if selected port already has a corresponding wire
                let selectedWire =
                    model.Wire.WX
                    |> List.tryFind (fun wire -> wire.SrcPort.Id = port.Id || wire.TargetPort.Id = port.Id)

                match selectedWire with
                // If wire found, set IsDragging to true
                | Some wire -> 
                    // set wire to dragging
                    let updatedWX = 
                        model.Wire.WX
                        |> List.map (fun x -> if x.Id = wire.Id then {wire with IsDragging = true} else x)

                    // update symbol to highlight available ports
                    let newSymbol = 
                        model.Wire.Symbol
                        |> Symbol.update (Symbol.Msg.ExpandPort (wire.DraggingPort, wire.SrcPort.Width))
                        |> fst

                    {model with Wire={model.Wire with WX=updatedWX; Symbol=newSymbol}; DragBox=newDragBox}, Cmd.none
                
                // if no wire exists, create a new one
                | None -> 
                    match port.PortType with
                    // start dragging from input port
                    | CommonTypes.PortType.Input ->
                        // create a temporary output port while dragging
                        let newPort : CommonTypes.Port = {
                            Id = (Helpers.uuid()) 
                            PortType = CommonTypes.PortType.Output
                            PortNumber = Some 1
                            HostId = CommonTypes.ComponentId (Helpers.uuid())
                            Pos = pos 
                            Width = port.Width
                        }

                        // create a new wire to connect the ports
                        let newWire : BusWire.Wire = {
                            Id = CommonTypes.ConnectionId (uuid())
                            SrcPort = port
                            TargetPort = newPort
                            IsDragging = true
                            DraggingPort = CommonTypes.PortType.Output 
                        }

                        // append the new wire to the model
                        let newWires = newWire::model.Wire.WX

                        // update all symbols to show expanded ports when dragging wires
                        let newSymbol = 
                            model.Wire.Symbol
                            |> Symbol.update (Symbol.Msg.ExpandPort (newWire.DraggingPort, newWire.SrcPort.Width))
                            |> fst
                        
                        // returns updated model
                        {model with Wire={model.Wire with WX=newWires; Symbol=newSymbol}; DragBox=newDragBox}, Cmd.none
                    
                    // start dragging from output port
                    | CommonTypes.PortType.Output -> 
                        // create a temporary input port while dragging
                        let newPort : CommonTypes.Port = {
                            Id = (Helpers.uuid()) 
                            PortType = CommonTypes.PortType.Input
                            PortNumber = Some 1
                            HostId = CommonTypes.ComponentId (Helpers.uuid())
                            Pos = pos
                            Width = port.Width
                        }

                        // create a new wire to connect the ports
                        let newWire : BusWire.Wire = {
                            Id = CommonTypes.ConnectionId (uuid())
                            SrcPort = newPort
                            TargetPort = port
                            IsDragging = true
                            DraggingPort = CommonTypes.PortType.Input // input is dragging
                        }

                        // append the new wire to the model
                        let newWires = newWire::model.Wire.WX

                        // update all symbols to show expanded ports when dragging wires
                        let newSymbol = 
                            model.Wire.Symbol
                            |> Symbol.update (Symbol.Msg.ExpandPort (newWire.DraggingPort, newWire.SrcPort.Width))
                            |> fst

                        // returns updated model
                        {model with Wire={model.Wire with WX=newWires; Symbol=newSymbol}; DragBox=newDragBox}, Cmd.none


        // mouse up
        | Up ->
            // reset dragBox (i.e. so it is not displayed when mouse up)
            let resetDragBox = (pos, pos)
           
            // make all ports not expanded / highlighted (since mouse is lifted)
            let newSymbol = 
                match Symbol.isSymbolHoveredAndSelected model.Wire.Symbol pos with
                // if mouse lifted on top of selected symbol, all symbols remain selected
                | true -> model.Wire.Symbol

                // otherwise, deselect all symbols
                | false -> fst (Symbol.update (Symbol.Msg.BoxSelected model.DragBox) model.Wire.Symbol)
                |> List.map (fun sym -> {sym with ExpandedPort = None})

            // find if mouse up occurs at any port
            let selectedPort = Symbol.findPortByPosition model.Wire.Symbol pos

            match selectedPort with
            // if no port selected, delete the temporary wire if it exists
            | None ->
                let newWX = 
                    model.Wire.WX
                    |> List.filter (fun wire -> not wire.IsDragging)
                
                {model with Wire={model.Wire with WX=newWX; Symbol=newSymbol}; DragBox=resetDragBox}, Cmd.none
            
            // if port selected, add new wire if port is valid (i.e. input -> input, output -> output, and equal bus width)
            | Some port ->
                let newWX = 
                    model.Wire.WX
                    |> List.collect (fun wire -> 
                                        if wire.IsDragging then
                                            match wire.DraggingPort with
                                            // input port dragged
                                            | CommonTypes.PortType.Input -> 

                                                // if valid port set new port, else remove the temporary wire
                                                if port.PortType = CommonTypes.PortType.Input && port.Width = wire.TargetPort.Width
                                                then [{wire with SrcPort = port; IsDragging = false}]
                                                else []
                                            
                                            // output port dragged
                                            | CommonTypes.PortType.Output -> 

                                                // if valid port set new port, else remove the temporary wire
                                                if port.PortType = CommonTypes.PortType.Output && port.Width = wire.SrcPort.Width
                                                then [{wire with TargetPort = port; IsDragging = false}]
                                                else []
                                        
                                        // if wire is not dragging, return the wire
                                        else 
                                            [wire]
                                    )
                
                // return updated model
                {model with Wire={model.Wire with WX=newWX; Symbol=newSymbol}; DragBox=resetDragBox}, Cmd.none


        // mouse drag
        | Drag ->
            // create new drag box
            let newDragBox = 
                // check if any symbol is dragging
                let isSymbolDragging = 
                    Symbol.isAnySymbolDragging model.Wire.Symbol
                
                // check if any wire is dragging
                let isWireDragging = 
                    model.Wire.WX
                    |> List.exists (fun wire -> wire.IsDragging)

                // check if either symbols or wires are dragging
                let isAnythingDragging = 
                    isSymbolDragging || isWireDragging

                match isAnythingDragging with
                // if anything is dragging, do not draw the drag box
                | true -> 
                    (pos, pos)

                // if nothing is dragging, then draw drag box
                | false ->
                    let pos1 = fst model.DragBox 
                    let pos2 = pos 
                    (pos1, pos2)

            // Update wire based on dragged wire
            let newWX = 
                model.Wire.WX
                |> List.map (fun wire -> 
                    // move new wire being dragged
                    if wire.IsDragging then 
                        match wire.DraggingPort with
                        // input is dragging
                        | CommonTypes.PortType.Input -> 
                            // move the temporary port to show wire dragging animation
                            let newSrcPort = {wire.SrcPort with Pos=pos}
                            {wire with SrcPort = newSrcPort}
                        
                        // output is dragging
                        | CommonTypes.PortType.Output -> 
                            // move the temporary port to show wire dragging animation
                            let newTargetPort = {wire.TargetPort with Pos=pos}
                            {wire with TargetPort = newTargetPort}

                    // move wire alongside its corresponding ports if symbol dragged
                    else 
                        // find new source and target symbol positions
                        let newSrcPos = Symbol.findPortPos model.Wire.Symbol wire.SrcPort.HostId wire.SrcPort.Id
                        let newTargetPos = Symbol.findPortPos model.Wire.Symbol wire.TargetPort.HostId wire.TargetPort.Id

                        // find new source and target ports with updated positions
                        let newSrcPort = {wire.SrcPort with Pos=newSrcPos}
                        let newTargetPort = {wire.TargetPort with Pos=newTargetPos}

                        {wire with SrcPort = newSrcPort; TargetPort = newTargetPort}
                    )

            // return updated model
            {model with Wire={model.Wire with WX=newWX}; DragBox=newDragBox}, Cmd.none


        // mouse move
        | Move -> 
            // reset dragBox
            let resetDragBox = (pos, pos)

            // obtain new symbol
            let newSymbol, _ = 
                Symbol.update (Symbol.Msg.SymbolHovering pos) model.Wire.Symbol

            // move wire alongside its corresponding ports if symbol moved
            let newWX = 
                model.Wire.WX 
                |> List.map (fun wire -> 
                    // find new source and target symbol positions
                    let newSrcPos = Symbol.findPortPos model.Wire.Symbol wire.SrcPort.HostId wire.SrcPort.Id
                    let newTargetPos = Symbol.findPortPos model.Wire.Symbol wire.TargetPort.HostId wire.TargetPort.Id

                    // find new source and target ports with updated positions
                    let newSrcPort = {wire.SrcPort with Pos=newSrcPos}
                    let newTargetPort = {wire.TargetPort with Pos=newTargetPos}

                    {wire with SrcPort = newSrcPort; TargetPort = newTargetPort}
                )

            // return updated model
            {model with Wire={model.Wire with WX=newWX; Symbol=newSymbol}; DragBox=resetDragBox}, Cmd.none

/// Initialization
let init() = 
    let model,cmds = (BusWire.init 1)()
    let dragBoxCoords = ({X=100.; Y=100.}, {X=100.; Y=100.}) 
    {
        Wire = model
        ComponentInfo = {PortWidth = 1; ComponentLabel = "C1"; NumInputPorts = 1; NumOutputPorts = 1}
        DragBox=dragBoxCoords
    }, Cmd.map Wire cmds
