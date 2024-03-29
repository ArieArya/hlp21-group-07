﻿module Sheet
open Fulma
open Fable.React
open Fable.React.Props
open Browser
open Elmish
open Elmish.React

open Model
open Helpers
open type CommonTypes.ComponentType
open CatalogueView
open PropertiesView


//------------------------------------------------------------------------//
//-------------------------------Symbol Types-----------------------------//
//------------------------------------------------------------------------//


// *************** Please see Model.fs for the Sheet model ***************//



//------------------------------------------------------------------------//
//---------------------------Helper Functions-----------------------------//
//------------------------------------------------------------------------//

// obtains the right-side menu to obtain user inputs (e.g. symbol type, name
// of components, number of input and output ports, port width, etc.)
let rightColumnStyle = 
    Style [
        Position PositionOptions.Fixed
        Right "0px"
        Top "0px"
        Height  "100vh"
        Width "25%"
        BorderLeft  "0.2vh solid lightgray"
        BorderTop "0.2vh solid lightgray"
        BorderBottom "0.2vh solid lightgray"
        BorderRight "0.2vh solid lightgray"
        Margin "0"
        Padding "0"
        UserSelect UserSelectOptions.None
        ZIndex 31
        BackgroundColor "#fcfcfc"
        OverflowY OverflowOptions.Scroll
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

/// Stores past wire models to memory
let storePastWireData (wireModel: BusWire.Model) (pastModelList: BusWire.Model list) = 
    // clean current model by preventing any dragging / hovered symbols
    let cleanModel, _ = BusWire.update (BusWire.Msg.SaveModel) wireModel
    
    // set maximum memory to 20 past operations 
    match pastModelList.Length with 
    | memLength when memLength < 20 ->
        [cleanModel] @ pastModelList

    | _ ->
        // slice from index 0 to 8 (i.e. 19 elements)
        let slicedList = 
            pastModelList.[..18] 

        [cleanModel] @ slicedList



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

    // dispatch a Sheet KeyMsg message if key message detected
    let keyOp (op: KeyOp) (ev:Types.KeyboardEvent) = 
        match ev.key, op, model.CtrlPressed with 
        | "Control", KeyDown, false | "Control", KeyUp, true -> dispatch (CtrlKeyPress op)
        | "Delete", KeyDown, _ -> dispatch (KeyPress DEL)
        | "a", KeyDown, true | "A", KeyDown, true -> dispatch (KeyPress CtrlA)
        | "c", KeyDown, true | "C", KeyDown, true -> dispatch (KeyPress CtrlC)
        | "v", KeyDown, true | "V", KeyDown, true -> dispatch (KeyPress CtrlV)
        | "z", KeyDown, true | "Z", KeyDown, true -> dispatch (KeyPress CtrlZ)
        | "y", KeyDown, true | "Y", KeyDown, true -> dispatch (KeyPress CtrlY)
        | _ -> ()

    // define the dragBox - the box in which users can select multiple symbols
    let dragBox = 
        // obtain the two corners of the dragging box
        let pos1 = model.DragBox.Edge1
        let pos2 = model.DragBox.Edge2

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

    // define the dragLine
    let dragLine = 
        let pos1 = model.DragWire.SrcEdge
        let pos2 = model.DragWire.TargetEdge

        let displayString = 
            convertPoly([(pos1.X, pos1.Y)] @ [pos2.X, pos2.Y])

        polyline [
            Points displayString
            SVGAttr.Fill "none"
            SVGAttr.Stroke "#326199"
            SVGAttr.StrokeDasharray "10, 10"
        ][]

    // obtains the canvas for drawing the canvas grid and the busWire view
    let baseCanvas = drawGrid @ [dragBox] @ [dragLine] @ [svgReact]

    // draws the SVG canvas
    div [ Style 
            [ 
                CSSProp.OverflowX OverflowOptions.Hidden
                CSSProp.OverflowY OverflowOptions.Auto
                Outline "none"
            ]
          TabIndex -1

          OnKeyDown (fun ev -> (keyOp KeyDown ev))
          OnKeyUp (fun ev -> (keyOp KeyUp ev))
          OnMouseDown (fun ev -> (mouseOp Down ev))
          OnMouseUp (fun ev -> (mouseOp Up ev))
          OnMouseMove (fun ev -> mouseOp (if mDown ev then Drag else Move) ev)
          
        ]
        
        [            
            svg [ Style 
                    [
                        Height canvasHeight
                        Width canvasWidth
                    ]
                ]
                [ g 
                    [ Style [Transform (sprintf "scale(%f)" zoom)]] 
                    // draws the base canvas
                    baseCanvas
                
                ]
            div [rightColumnStyle]
                [
                    div [ Style [Height "100%"]][ 
                        div [ Style [PaddingTop "0vh"; Margin "0"; PaddingBottom "0"]][
                            button [
                                Style [
                                    Height "3vh"
                                    Width "25%"
                                    TextAnchor "middle" // horizontal algnment vs (X,Y)
                                    DominantBaseline "middle" // vertical alignment vs (X,Y)
                                    FontSize "1.5vh"
                                    FontWeight "Bold"
                                    Fill "Gray" // font color
                                ]
                                OnClick (fun _ -> dispatch (ChangeRightTab Catalogue))
                            ][str "Catalogue"]
                            button [
                                Style [
                                    Height "3vh"
                                    Width "25%"
                                    TextAnchor "middle" // horizontal algnment vs (X,Y)
                                    DominantBaseline "middle" // vertical alignment vs (X,Y)
                                    FontSize "1.5vh"
                                    FontWeight "Bold"
                                    Fill "Gray" // font color
                                ]
                                OnClick (fun _ -> dispatch (ChangeRightTab Properties))
                            ][str "Properties"]
                            match model.RightPaneTabVisible with
                            | Catalogue ->
                                    viewCatalogue model dispatch
                            | Properties -> 
                                    viewProperties model dispatch
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

    // draws new symbol
    | CreateSymbol (compType, width, height) ->

        // finds position to insert new symbol (i.e. ensuring no collision with existing symbols)
        let pos = Symbol.findNextAvailablePos (model.Wire.Symbol) (width, height)

        let compName = 
            match compType with 
            | IOLabel -> model.ComponentInfo.IOLabelName
            | Custom _ -> model.ComponentInfo.CustComponentName
            | _ -> ""

        // return updated model
        let newModel, newCmd = Symbol.update (Symbol.Msg.AddSymbol (compType, pos, compName)) model.Wire.Symbol

        {model with Wire = {model.Wire with Symbol = newModel}; UndoWireModels=(storePastWireData model.Wire model.UndoWireModels); RedoWireModels=[]}, newCmd
    
    //Changes Right Tab to display based on button pressed
    | ChangeRightTab newTab ->
        {model with RightPaneTabVisible = newTab}, Cmd.none

    | ChangeSelectedComponent comp ->
        {model with SelectedComponent = comp}, Cmd.none
    
    | UpdateComponentLabel (comp, newLabel) ->
        // return updated model
        let newModel, newCmd = 
            Symbol.update (Symbol.Msg.UpdateComponentLabel (comp, newLabel)) model.Wire.Symbol
            
        {model with Wire = {model.Wire with Symbol = newModel}}, newCmd

    | UpdateComponentWidth (comp, newWidth) ->
        // return updated model
        let newModel, newCmd = 
            Symbol.update (Symbol.Msg.UpdateComponentWidth (comp, newWidth)) model.Wire.Symbol
            
        {model with Wire = {model.Wire with Symbol = newModel}}, newCmd

    // detects key presses from users
    | KeyPress s -> 
        match s with
        // select all symbols
        | CtrlA ->
            let newWires, _ = BusWire.update (BusWire.Msg.SelectAll) model.Wire
            {model with Wire=newWires}, Cmd.none

        // copy symbols & wires
        | CtrlC -> 
            let newWires, _ = BusWire.update (BusWire.Msg.CopyWires) model.Wire
            {model with Wire = newWires}, Cmd.none

        // paste symbols & wires
        | CtrlV -> 
            let newWires, _ = BusWire.update (BusWire.Msg.PasteWires) model.Wire
            {model with Wire = newWires; UndoWireModels=(storePastWireData model.Wire model.UndoWireModels); RedoWireModels=[]}, Cmd.none

        // undo
        | CtrlZ ->
            match model.UndoWireModels with 
            | (hd::tl) ->
                {model with Wire=hd; UndoWireModels=tl; RedoWireModels=(storePastWireData model.Wire model.RedoWireModels)}, Cmd.none

            | [] -> model, Cmd.none
        
        // redo
        | CtrlY ->
            match model.RedoWireModels with 
            | (hd::tl) ->
                {model with Wire=hd; RedoWireModels=tl; UndoWireModels=(storePastWireData model.Wire model.UndoWireModels)}, Cmd.none
            
            | [] -> model, Cmd.none

        // delete all selected symbols & wires
        | DEL ->
            let newModel, _ = BusWire.update (BusWire.Msg.DeleteWiresBySymbol) model.Wire
            {model with Wire = newModel; UndoWireModels=(storePastWireData model.Wire model.UndoWireModels); RedoWireModels=[]}, Cmd.none

        // print stats
        | AltShiftZ -> 
            printStats() 
            model, Cmd.none

        // toggle buswidth legend
        | AltV ->
            let newModel, _ = BusWire.update (BusWire.Msg.ToggleLegend) model.Wire
            {model with Wire = newModel}, Cmd.none
        
        // ignore all other key presses
        | _ ->
            model, Cmd.none
    
    // determines whether ctrl is currently being pressed
    | CtrlKeyPress op ->
        match op with 
        | KeyUp ->  
            {model with CtrlPressed = false}, Cmd.none
        | KeyDown ->
            {model with CtrlPressed = true}, Cmd.none

    // handles mouse operations
    | MouseMsg mMsg ->
        // obtain the mouse operation and its position
        let operation = mMsg.Op
        let pos = mMsg.Pos

        match operation with 
        // mouse down
        | Down -> 
            // check if any symbol is dragging
            let symbolDraggingCheck = Symbol.isAnySymbolDragging (model.Wire.Symbol)

            // check if any wire is hovered
            let wireDraggingCheck = BusWire.isAnyWireHovered (model.Wire) pos

            // check if anything is dragging
            let isAnythingDragging = 
                symbolDraggingCheck || wireDraggingCheck

            // try find a port corresponding to the mouse position
            let selectedPort = Symbol.findPortByPosition model.Wire.Symbol pos

            // if no port is selected, return the original model with DragBox, otherwise update the ports
            // Note: for current implementation, Symbol uses event listeners and so no checking
            // performed for symbol being clicked. Once integrated, symbol will also be checked.
            match selectedPort, isAnythingDragging with
            | _, true -> 
                let updatedWire, _ = BusWire.update (BusWire.Msg.MouseMsg (mMsg, model.CtrlPressed)) model.Wire
                {model with Wire=updatedWire; UndoWireModels=(storePastWireData model.Wire model.UndoWireModels); RedoWireModels=[]}, Cmd.none

            | None, false ->
                // initialize DragBox and DragWire
                let newDragBox = {Edge1 = pos; Edge2 = pos; isDragging = true}
                let newDragWire = {SrcEdge = pos; TargetEdge = pos; isDragging = false; DraggingPort=CommonTypes.PortType.Input}
                let updatedWire, _ = BusWire.update (BusWire.Msg.MouseMsg (mMsg, model.CtrlPressed)) model.Wire
                {model with Wire=updatedWire; DragBox=newDragBox; DragWire=newDragWire}, Cmd.none

            | Some port, false ->
                // initialize DragBox
                let newDragBox = {Edge1 = pos; Edge2 = pos; isDragging = false} 

                // check if selected port already has a corresponding wire
                let selectedWire =
                    model.Wire.WX
                    |> List.tryFind (fun wire -> wire.SrcPort.Id = port.Id || wire.TargetPort.Id = port.Id)

                match selectedWire with
                // If wire found, delete wire and initialize new DragWire
                | Some wire -> 
                    // initialize DragWire
                    let srcPos, targetPos, dragType = 
                        match wire.SrcPort.Id = port.Id, wire.TargetPort.Id = port.Id with
                        // src port being dragged
                        | true, false -> pos, wire.TargetPort.Pos, CommonTypes.PortType.Input

                        // target port being dragged
                        | _, _ -> wire.SrcPort.Pos, pos, CommonTypes.PortType.Output
                    
                    let newDragWire = {SrcEdge=srcPos; TargetEdge=targetPos; isDragging=true; DraggingPort=dragType}

                    // delete existing wire
                    let updatedWire, _ = 
                        model.Wire
                        |> BusWire.update (BusWire.Msg.DeleteWire (wire.Id))
                    
                    // update symbol to highlight available ports
                    let newSymbol = 
                        model.Wire.Symbol
                        |> Symbol.update (Symbol.Msg.ExpandPort (port.PortType, wire.SrcPort.Width))
                        |> fst

                    {model with Wire={model.Wire with WX=updatedWire.WX; Symbol=newSymbol}; DragBox=newDragBox; DragWire=newDragWire; UndoWireModels=(storePastWireData model.Wire model.UndoWireModels); RedoWireModels=[]}, Cmd.none
                
                // if no wire exists, visualize a new one
                | None -> 
                    let dragPortType = 
                        match port.PortType with
                        | CommonTypes.PortType.Input -> CommonTypes.PortType.Output
                        | CommonTypes.PortType.Output -> CommonTypes.PortType.Input

                    let newDragWire = {SrcEdge=port.Pos; TargetEdge=port.Pos; isDragging=true; DraggingPort=dragPortType}

                    // update all symbols to show expanded ports when dragging wires
                    let newSymbol = 
                        model.Wire.Symbol
                        |> Symbol.update (Symbol.Msg.ExpandPort (dragPortType, port.Width))
                        |> fst
                    
                    // returns updated model
                    {model with Wire={model.Wire with Symbol=newSymbol}; DragBox=newDragBox; DragWire=newDragWire; UndoWireModels=(storePastWireData model.Wire model.UndoWireModels); RedoWireModels=[]}, Cmd.none
                    
        // mouse up
        | Up ->
            // reset dragBox and dragWire (i.e. so it is not displayed when mouse up)
            let resetDragBox = {Edge1=pos; Edge2=pos; isDragging=false}
            let resetDragWire = {SrcEdge=pos; TargetEdge=pos; isDragging=false; DraggingPort=CommonTypes.PortType.Input}
           
            let updatedWire = 
                match Symbol.isSymbolHoveredAndSelected model.Wire.Symbol pos with
                // if mouse lifted on top of selected symbol, all symbols & wires remain selected
                | true -> 
                    let newSymbol = 
                        model.Wire.Symbol
                        // make all ports not expanded / highlighted (since mouse is lifted)
                        |> List.map (fun sym -> {sym with ExpandedPort = (None, None)})
                
                    {model.Wire with Symbol=newSymbol}

                // otherwise, trigger select using box
                | false -> fst (BusWire.update (BusWire.Msg.BoxSelected (model.DragBox.Edge1, model.DragBox.Edge2, model.CtrlPressed)) model.Wire)

            // find if mouse up occurs at any port
            let selectedPort = Symbol.findPortByPosition model.Wire.Symbol pos

            match selectedPort with
            // if no port selected, return model with updated symbol
            | None ->
                {model with Wire=updatedWire; DragBox=resetDragBox; DragWire=resetDragWire}, Cmd.none
            
            // if port selected, add new wire between two ports
            | Some port ->
                match model.DragWire.isDragging with
                | false -> {model with Wire=updatedWire; DragBox=resetDragBox}, Cmd.none

                | true -> 
                    let startDragPort = 
                        match model.DragWire.DraggingPort with
                        // src port being dragged, so return target port
                        | CommonTypes.PortType.Input ->
                            Symbol.findPortByPosition (model.Wire.Symbol) (model.DragWire.TargetEdge)

                        // target port being dragged, so return source port
                        | CommonTypes.PortType.Output ->
                            Symbol.findPortByPosition (model.Wire.Symbol) (model.DragWire.SrcEdge)

                    // should always return Some (not None)
                    match startDragPort with
                    | Some originPort ->
                        // perform width inference
                        let port1 = originPort
                        let port2 = port

                        let matchPorts = 
                            // if both ports are inputs or both ports are output, return None, i.e.
                            // it is not possible to connect an input port to an input port, and an
                            // output port to an output port
                            match port1, port2 with
                            | p1, p2 when p1.PortType = p2.PortType ->
                                None
                            | p1, p2 when p1.PortType = CommonTypes.PortType.Input ->
                                Some(p1, p2)
                            | p1, p2 when p1.PortType = CommonTypes.PortType.Output ->
                                Some(p2, p1)
                            | _ -> None

                        let matchWidths = 
                            match port1.Width, port2.Width with
                            | Some a, Some b when a = b -> true
                            | Some _, None -> true
                            | None, Some _ -> true
                            | _ -> false

                        // if no port combinations possible, return the original model, otherwise
                        // add new wire between the ports to the new model
                        match matchPorts, matchWidths with
                        | None, _ -> model, Cmd.none
                        | _, false -> model, Cmd.none

                        | Some (targetPort, srcPort), true ->
                            match targetPort.Width, srcPort.Width with
                            // if both ports have a fixed width, add wire between the ports
                            | Some a, Some b ->
                                let newWire, _ = BusWire.update (BusWire.Msg.AddWire (srcPort, targetPort)) (updatedWire)
                                {model with Wire=newWire; DragBox=resetDragBox}, Cmd.none

                            // if one port has variable port widths, perform width inference
                            | Some a, None ->
                                let newSymModel, isValid = Symbol.portInference updatedWire.Symbol srcPort a
                                if isValid then
                                    let newSrcPort = {srcPort with Width = Some a}
                                    let newWire, _ = BusWire.update (BusWire.Msg.AddWire (newSrcPort, targetPort)) (updatedWire)
                                    let symUpdatedWire = {newWire with Symbol=newSymModel}
                                    {model with Wire=symUpdatedWire; DragBox=resetDragBox}, Cmd.none

                                else 
                                    model, Cmd.none

                            // if one port has variable port widths, perform width inference
                            | None, Some a ->
                                let newSymModel, isValid = Symbol.portInference updatedWire.Symbol targetPort a
                                if isValid then
                                    let newTargetPort = {targetPort with Width = Some a}
                                    let newWire, _ = BusWire.update (BusWire.Msg.AddWire (srcPort, newTargetPort)) (updatedWire)
                                    let symUpdatedWire = {newWire with Symbol=newSymModel}
                                    {model with Wire=symUpdatedWire; DragBox=resetDragBox}, Cmd.none
                                else 
                                    {model with Wire=updatedWire; DragBox=resetDragBox}, Cmd.none

                            | None, None ->
                                {model with Wire=updatedWire; DragBox=resetDragBox}, Cmd.none

                    | None ->
                        // return updated model
                        {model with Wire=updatedWire; DragBox=resetDragBox}, Cmd.none

        // mouse drag
        | Drag ->
            // create new drag box
            let newDragBox = 
                match model.DragBox.isDragging with
                // if dragbox is not dragging, reset the position
                | false -> 
                    {model.DragBox with Edge1=pos; Edge2=pos}

                // else place new dragbox coordinates
                | true ->
                    {model.DragBox with Edge2=pos}

            // create new drag wire
            let newDragWire = 
                match model.DragWire.isDragging, model.DragWire.DraggingPort with
                // if wire is not dragging, reset the position
                | false, _ ->
                    {model.DragWire with SrcEdge=pos; TargetEdge=pos}

                // else place new dragwire coordinates
                | true, CommonTypes.PortType.Input ->
                    {model.DragWire with SrcEdge=pos}

                | true, CommonTypes.PortType.Output ->
                    {model.DragWire with TargetEdge=pos}
            
            let updatedWire, _ = BusWire.update (BusWire.Msg.MouseMsg (mMsg, model.CtrlPressed)) model.Wire
            {model with Wire=updatedWire; DragBox=newDragBox; DragWire = newDragWire}, Cmd.none

        // mouse move
        | Move -> 
            // reset dragBox and dragWire
            let resetDragBox = {Edge1=pos; Edge2=pos; isDragging=false}
            let resetDragWire = {SrcEdge=pos; TargetEdge=pos; isDragging=false; DraggingPort=CommonTypes.PortType.Input}
           
            // obtain new symbol
            let newSymbol, _ = 
                let hoverCheck, _ = 
                    Symbol.update (Symbol.Msg.SymbolHovering pos) model.Wire.Symbol

                // checking if symbol overlaps any other symbol in model and checking if hovering over symbol
                Symbol.update Symbol.Msg.SymbolOverlap hoverCheck 

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
            {model with Wire={model.Wire with WX=newWX; Symbol=newSymbol}; DragBox=resetDragBox; DragWire=resetDragWire}, Cmd.none
        
    | ErrorHighlight ->
        // return updated model
        let newModel, _ = 
            BusWire.update (BusWire.Msg.ErrorHighlight) model.Wire
            
        {model with Wire = newModel; UndoWireModels=(storePastWireData model.Wire model.UndoWireModels); RedoWireModels=[]}, Cmd.none


    /// **********************************************************************************************************************
    ///                                                       NOTE:
    /// 
    ///                  The messages below are NOT part of the Draw2D canvas implementation. It is used to take
    ///                  user-defined input parameters (e.g. port widths, custom component labels, etc.) for demo
    ///                  purposes only. This is used to replace the ISSIE interface for the demo (to allow user-
    ///                  defined modules).
    ///
    /// **********************************************************************************************************************
    
    // for Input and Output
    | ChangeInputWidth width ->
        {model with ComponentInfo = {model.ComponentInfo with InputWidth = width}}, Cmd.none
    | ChangeOutputWidth width ->
        {model with ComponentInfo = {model.ComponentInfo with OutputWidth = width}}, Cmd.none

    // for BusSelection
    | ChangeBusSelectionOutWidth outWidth ->
        {model with ComponentInfo = {model.ComponentInfo with BusSelectionOutWidth = outWidth}}, Cmd.none
    | ChangeBusSelectionLSB lsb ->
        {model with ComponentInfo = {model.ComponentInfo with BusSelectionLSB = lsb}}, Cmd.none

    // for constant
    | ChangeConstantValue value ->
        {model with ComponentInfo = {model.ComponentInfo with ConstantValue = value}}, Cmd.none
    | ChangeConstantWidth width ->
        {model with ComponentInfo = {model.ComponentInfo with ConstantWidth = width}}, Cmd.none

    // for AsyncROM
    | ChangeAsyncROMMemBits memAddrBits ->
        {model with ComponentInfo = {model.ComponentInfo with AsyncROMMemBits = memAddrBits}}, Cmd.none
    | ChangeAsyncROMOutWidth outWidth ->
        {model with ComponentInfo = {model.ComponentInfo with AsyncROMOutWidth = outWidth}}, Cmd.none
    
    // for ROM
    | ChangeROMMemBits memAddrBits ->
        {model with ComponentInfo = {model.ComponentInfo with ROMMemBits = memAddrBits}}, Cmd.none
    | ChangeROMOutWidth outWidth ->
        {model with ComponentInfo = {model.ComponentInfo with ROMOutWidth = outWidth}}, Cmd.none

    // for RAM
    | ChangeRAMMemBits memAddrBits ->
        {model with ComponentInfo = {model.ComponentInfo with RAMMemBits = memAddrBits}}, Cmd.none
    | ChangeRAMOutWidth outWidth ->
        {model with ComponentInfo = {model.ComponentInfo with RAMOutWidth = outWidth}}, Cmd.none

    // for N-bits Adder
    | ChangeAdderBits addBitSize ->
        {model with ComponentInfo = {model.ComponentInfo with AdderBits = addBitSize}}, Cmd.none

    // for SplitWires
    | ChangeSplitOutWidth outWidth ->
        {model with ComponentInfo = {model.ComponentInfo with SplitOutWidth = outWidth}}, Cmd.none

    // for Register
    | ChangeRegWidth width ->
        {model with ComponentInfo = {model.ComponentInfo with RegWidth = width}}, Cmd.none

    // for RegisterEnabled
    | ChangeRegEnabledWidth width ->
        {model with ComponentInfo = {model.ComponentInfo with RegEnabledWidth = width}}, Cmd.none

    // for IOLabel
    | ChangeIOLabelName labelName ->
        {model with ComponentInfo = {model.ComponentInfo with IOLabelName = labelName}}, Cmd.none

    // for CustomComponent
    | ChangeCustComponentName compName ->
        {model with ComponentInfo = {model.ComponentInfo with CustComponentName = compName}}, Cmd.none
    | ChangeCustComponentInpPortsList inpPortsStr ->
        {model with ComponentInfo = {model.ComponentInfo with CustComponentInpPortsList = inpPortsStr}}, Cmd.none
    | ChangeCustComponentOutPortsList outPortsStr ->
        {model with ComponentInfo = {model.ComponentInfo with CustComponentOutPortsList = outPortsStr}}, Cmd.none



/// Initialization
let init() = 
    let model,cmds = (BusWire.init 1)()
    let dragBoxInit = {Edge1={X=100.; Y=100.}; Edge2={X=100.; Y=100.}; isDragging=false}
    let dragWireInit = {SrcEdge={X=100.; Y=100.}; TargetEdge={X=100.; Y=100.}; isDragging=false; DraggingPort=CommonTypes.PortType.Input}

    {
        Wire = model
        UndoWireModels = []
        RedoWireModels = []
        // initialize user-defined values
        ComponentInfo = {InputWidth = 1; OutputWidth = 1; BusSelectionOutWidth = 2; BusSelectionLSB = 0; ConstantValue = 1; ConstantWidth = 1;
                        AsyncROMMemBits = 1; AsyncROMOutWidth = 1; ROMMemBits = 1; ROMOutWidth = 1; RAMMemBits = 1; RAMOutWidth = 1;
                        AdderBits = 1; SplitOutWidth = 1; RegWidth = 1; 
                        RegEnabledWidth = 1; IOLabelName = "wireLabel"; CustComponentName = "CustomName"; CustComponentInpPortsList = "[(in1, 1); (in2, 1)]";
                        CustComponentOutPortsList = "[(out1, 1); (out2, 1)]"}
        DragBox=dragBoxInit
        DragWire=dragWireInit
        CtrlPressed=false
        RightPaneTabVisible=Catalogue
        SelectedComponent=None
    }, Cmd.map Wire cmds