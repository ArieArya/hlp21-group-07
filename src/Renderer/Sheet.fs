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

type DragBoxType = {
    Edge1: XYPos
    Edge2: XYPos
    isDragging: bool
}

type DragWireType = {
    SrcEdge: XYPos
    TargetEdge: XYPos
    isDragging: bool
    DraggingPort: CommonTypes.PortType
}


type Model = {
    Wire: BusWire.Model
    ComponentInfo: CompInfo
    DragBox: DragBoxType
    DragWire: DragWireType
    }


//------------------------------------------------------------------------//
//---------------------------Message Type---------------------------------//
//------------------------------------------------------------------------//

type KeyboardMsg =
    | CtrlS | AltC | AltV | AltZ | AltShiftZ | DEL

type Msg =
    | Wire of BusWire.Msg
    | KeyPress of KeyboardMsg
    | CreateSymbol of CommonTypes.ComponentType * float * float
    | ChangeWireWidth of int
    | ChangeCompLabel of string
    | ChangeNumInputPorts of int
    | ChangeNumOutputPorts of int
    | MouseMsg of MouseT



//------------------------------------------------------------------------//
//---------------------------Helper Functions-----------------------------//
//------------------------------------------------------------------------//
let testMemory : CommonTypes.Memory = {
    AddressWidth = 0
        // How wide each memory word should be, in bits.
    WordWidth = 3
        // Data is a list of <2^AddressWidth> elements, where each element is a
        // 64 bit integer. This makes words longer than 64 bits not supported.
        // This can be changed by using strings instead of int64, but that is way
        // less memory efficient.
    Data = Map.empty
}
let testBusWidth = 3

// obtains the right-side menu to obtain user inputs (e.g. symbol type, name
// of components, number of input and output ports, port width, etc.)
let rightColumn = 
    Style [
        Position PositionOptions.Fixed
        Right "0px"
        Top "0px"
        Height  "100vh"
        Width "37.5%"
        BorderTop "0.4vh solid lightgray"
        BorderLeft "0.4vh solid lightgray"
        BorderBottom "0.4vh solid lightgray"
        Margin "0"
        Padding "0"
        UserSelect UserSelectOptions.None
        ZIndex 31
        BackgroundColor "#f2f2f2"
    ]

let rightColumnLeft = 
    Style [
        Position PositionOptions.Fixed
        Right "190px"
        Top "220px"
        Height  "100vh"
        Width "12.5%"
        BorderTop "0.4vh solid lightgray"
        BorderLeft "0.4vh solid lightgray"
        BorderBottom "0.4vh solid lightgray"
        Margin "0"
        Padding "0"
        UserSelect UserSelectOptions.None
        ZIndex 31
        BackgroundColor "#f2f2f2"
    ]

let rightColumnRight = 
    Style [
        Position PositionOptions.Fixed
        Right "0px"
        Top "220px"
        Height  "100vh"
        Width "12.5%"
        BorderTop "0.4vh solid lightgray"
        BorderLeft "0.4vh solid lightgray"
        BorderBottom "0.4vh solid lightgray"
        Margin "0"
        Padding "0"
        UserSelect UserSelectOptions.None
        ZIndex 31
        BackgroundColor "#f2f2f2"
    ]
let rightOfRightColumn = 
    Style [
        Position PositionOptions.Fixed
        Right "380px"
        Top "220px"
        Height  "100vh"
        Width "12.5%"
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
    let baseCanvas = drawGrid @ [svgReact] @ [dragBox] @ [dragLine]

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

          // draws the right-side menu column
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
                  ]
            ]
          div [ rightOfRightColumn ][
              div [ Style [Height "100%"; Width "100%"; TextAlign TextAlignOptions.Center]]
                [
                    div [ Style [PaddingTop "4vh"; Margin "0"; PaddingBottom "0"]][
                            a [
                                Style [
                                    Height "5vh"
                                    TextAnchor "middle" 
                                    DominantBaseline "middle" 
                                    FontSize "1.5vh"
                                    FontWeight "Bold"
                                    Fill "Gray" 
                                ]
                                OnClick (fun _ -> dispatch (CreateSymbol (CommonTypes.ComponentType.Input(testBusWidth), 40., 50.)))
                            ][str "Input"]
                      ]                     
                    div [ Style [PaddingTop "4vh"; Margin "0"; PaddingBottom "0"]][
                            a [
                                Style [
                                    Height "5vh"
                                    TextAnchor "middle" 
                                    DominantBaseline "middle" 
                                    FontSize "1.5vh"
                                    FontWeight "Bold"
                                    Fill "Gray" 
                                ]
                                OnClick (fun _ -> dispatch (CreateSymbol (CommonTypes.ComponentType.Output(testBusWidth), 40., 50.)))
                            ][str "Output"]
                      ]                     
                    div [ Style [PaddingTop "4vh"; Margin "0"; PaddingBottom "0"]][
                            a [
                                Style [
                                    Height "5vh"
                                    TextAnchor "middle" 
                                    DominantBaseline "middle" 
                                    FontSize "1.5vh"
                                    FontWeight "Bold"
                                    Fill "Gray" 
                                ]
                                OnClick (fun _ -> dispatch (CreateSymbol (CommonTypes.ComponentType.BusSelection(testBusWidth, 0), 40., 50.)))
                            ][str "BusSelection"]
                      ]                     
                    div [ Style [PaddingTop "4vh"; Margin "0"; PaddingBottom "0"]][
                            a [
                                Style [
                                    Height "5vh"
                                    TextAnchor "middle" 
                                    DominantBaseline "middle" 
                                    FontSize "1.5vh"
                                    FontWeight "Bold"
                                    Fill "Gray" 
                                ]
                                OnClick (fun _ -> dispatch (CreateSymbol (CommonTypes.ComponentType.Constant(testBusWidth, 5), 40., 50.)))
                            ][str "Constant: 5"]
                      ]                     
                    div [ Style [PaddingTop "4vh"; Margin "0"; PaddingBottom "0"]][
                            a [
                                Style [
                                    Height "5vh"
                                    TextAnchor "middle" 
                                    DominantBaseline "middle" 
                                    FontSize "1.5vh"
                                    FontWeight "Bold"
                                    Fill "Gray" 
                                ]
                                OnClick (fun _ -> dispatch (CreateSymbol (CommonTypes.ComponentType.Not, 40., 50.)))
                            ][str "Not"]
                      ]                     
                ]
            ]
          div [ rightColumnLeft ][
              div [ Style [Height "100%"; Width "100%"; TextAlign TextAlignOptions.Center]]
                [
                      // user select to create module 1
                      div [ Style [PaddingTop "4vh"; Margin "0"; PaddingBottom "0"]][
                            a [
                                Style [
                                    Height "5vh"
                                    TextAnchor "middle" 
                                    DominantBaseline "middle" 
                                    FontSize "1.5vh"
                                    FontWeight "Bold"
                                    Fill "Gray" 
                                ]
                                OnClick (fun _ -> dispatch (CreateSymbol (CommonTypes.ComponentType.Not, 40., 50.)))
                            ][str "Not"]
                      ] 
                      div [ Style [PaddingTop "4vh"; Margin "0"; PaddingBottom "0"]][
                            a [
                                Style [
                                    Height "5vh"
                                    TextAnchor "middle" // horizontal algnment vs (X,Y)
                                    DominantBaseline "middle" // vertical alignment vs (X,Y)
                                    FontSize "1.5vh"
                                    FontWeight "Bold"
                                    Fill "Gray" // font color
                                ]
                                OnClick (fun _ -> dispatch (CreateSymbol (CommonTypes.ComponentType.Or, 60., 50.)))
                            ][str "Or"]
                      ]
                      div [ Style [PaddingTop "4vh"; Margin "0"; PaddingBottom "0"]][
                            a [
                                Style [
                                    Height "5vh"
                                    TextAnchor "middle" // horizontal algnment vs (X,Y)
                                    DominantBaseline "middle" // vertical alignment vs (X,Y)
                                    FontSize "1.5vh"
                                    FontWeight "Bold"
                                    Fill "Gray" // font color
                                ]
                                OnClick (fun _ -> dispatch (CreateSymbol (CommonTypes.ComponentType.And, 60., 50.)))
                            ][str "And"]
                      ]
                      div [ Style [PaddingTop "4vh"; Margin "0"; PaddingBottom "0"]][
                            a [
                                Style [
                                    Height "5vh"
                                    TextAnchor "middle" // horizontal algnment vs (X,Y)
                                    DominantBaseline "middle" // vertical alignment vs (X,Y)
                                    FontSize "1.5vh"
                                    FontWeight "Bold"
                                    Fill "Gray" // font color
                                ]
                                OnClick (fun _ -> dispatch (CreateSymbol (CommonTypes.ComponentType.Nor, 60., 50.)))
                            ][str "Nor"]
                      ]
                      div [ Style [PaddingTop "4vh"; Margin "0"; PaddingBottom "0"]][
                            a [
                                Style [
                                    Height "5vh"
                                    TextAnchor "middle" // horizontal algnment vs (X,Y)
                                    DominantBaseline "middle" // vertical alignment vs (X,Y)
                                    FontSize "1.5vh"
                                    FontWeight "Bold"
                                    Fill "Gray" // font color
                                ]
                                OnClick (fun _ -> dispatch (CreateSymbol (CommonTypes.ComponentType.Xor, 60., 50.)))
                            ][str "Xor"]
                      ]
                      div [ Style [PaddingTop "4vh"; Margin "0"; PaddingBottom "0"]][
                            a [
                                Style [
                                    Height "5vh"
                                    TextAnchor "middle" // horizontal algnment vs (X,Y)
                                    DominantBaseline "middle" // vertical alignment vs (X,Y)
                                    FontSize "1.5vh"
                                    FontWeight "Bold"
                                    Fill "Gray" // font color
                                ]
                                OnClick (fun _ -> dispatch (CreateSymbol (CommonTypes.ComponentType.Nand, 60., 50.)))
                            ][str "Nand"]
                        ]
                      div [ Style [PaddingTop "4vh"; Margin "0"; PaddingBottom "0"]][
                            a [
                                Style [
                                    Height "5vh"
                                    TextAnchor "middle" // horizontal algnment vs (X,Y)
                                    DominantBaseline "middle" // vertical alignment vs (X,Y)
                                    FontSize "1.5vh"
                                    FontWeight "Bold"
                                    Fill "Gray" // font color
                                ]
                                OnClick (fun _ -> dispatch (CreateSymbol (CommonTypes.ComponentType.Xnor, 60., 50.)))
                            ][str "Xnor"]
                       ]
                      div [ Style [PaddingTop "4vh"; Margin "0"; PaddingBottom "0"]][
                            a [
                                Style [
                                    Height "5vh"
                                    TextAnchor "middle" // horizontal algnment vs (X,Y)
                                    DominantBaseline "middle" // vertical alignment vs (X,Y)
                                    FontSize "1.5vh"
                                    FontWeight "Bold"
                                    Fill "Gray" // font color
                                ]
                                OnClick (fun _ -> dispatch (CreateSymbol (CommonTypes.ComponentType.Decode4, 200., 80.)))
                            ][str "Decode4"]
                       ]
                      div [ Style [PaddingTop "4vh"; Margin "0"; PaddingBottom "0"]][
                            a [
                                Style [
                                    Height "5vh"
                                    TextAnchor "middle" // horizontal algnment vs (X,Y)
                                    DominantBaseline "middle" // vertical alignment vs (X,Y)
                                    FontSize "1.5vh"
                                    FontWeight "Bold"
                                    Fill "Gray" // font color
                                ]
                                OnClick (fun _ -> dispatch (CreateSymbol (CommonTypes.ComponentType.Mux2, 60., 50.)))
                            ][str "Mux2"]
                        ]
                      div [ Style [PaddingTop "4vh"; Margin "0"; PaddingBottom "0"]][
                            a [
                                Style [
                                    Height "5vh"
                                    TextAnchor "middle" // horizontal algnment vs (X,Y)
                                    DominantBaseline "middle" // vertical alignment vs (X,Y)
                                    FontSize "1.5vh"
                                    FontWeight "Bold"
                                    Fill "Gray" // font color
                                ]
                                OnClick (fun _ -> dispatch (CreateSymbol (CommonTypes.ComponentType.Demux2, 60., 50.)))
                            ][str "Demux2"]
                        ]
                ]
            ]
          div [ rightColumnRight ][
              div [ Style [Height "100%"; Width "100%"; TextAlign TextAlignOptions.Center]]
                [
                      div [ Style [PaddingTop "4vh"; Margin "0"; PaddingBottom "0"]][
                            a [
                                Style [
                                    Height "5vh"
                                    TextAnchor "middle" // horizontal algnment vs (X,Y)
                                    DominantBaseline "middle" // vertical alignment vs (X,Y)
                                    FontSize "1.5vh"
                                    FontWeight "Bold"
                                    Fill "Gray" // font color
                                ]
                                OnClick (fun _ -> dispatch (CreateSymbol (CommonTypes.ComponentType.NbitsAdder(testBusWidth), 150., 100.)))
                            ][str "NbitsAdder"]
                        ]
                      div [ Style [PaddingTop "4vh"; Margin "0"; PaddingBottom "0"]][
                            a [
                                Style [
                                    Height "5vh"
                                    TextAnchor "middle" // horizontal algnment vs (X,Y)
                                    DominantBaseline "middle" // vertical alignment vs (X,Y)
                                    FontSize "1.5vh"
                                    FontWeight "Bold"
                                    Fill "Gray" // font color
                                ]
                                OnClick (fun _ -> dispatch (CreateSymbol (CommonTypes.ComponentType.MergeWires, 60., 70.)))
                            ][str "MergeWires"]
                        ]                          
                      div [ Style [PaddingTop "4vh"; Margin "0"; PaddingBottom "0"]][
                            a [
                                Style [
                                    Height "5vh"
                                    TextAnchor "middle" // horizontal algnment vs (X,Y)
                                    DominantBaseline "middle" // vertical alignment vs (X,Y)
                                    FontSize "1.5vh"
                                    FontWeight "Bold"
                                    Fill "Gray" // font color
                                ]
                                OnClick (fun _ -> dispatch (CreateSymbol (CommonTypes.ComponentType.SplitWire(testBusWidth), 60., 70.)))
                            ][str "SplitWire"]
                        ]  
                      div [ Style [PaddingTop "4vh"; Margin "0"; PaddingBottom "0"]][
                            a [
                                Style [
                                    Height "5vh"
                                    TextAnchor "middle" // horizontal algnment vs (X,Y)
                                    DominantBaseline "middle" // vertical alignment vs (X,Y)
                                    FontSize "1.5vh"
                                    FontWeight "Bold"
                                    Fill "Gray" // font color
                                ]
                                OnClick (fun _ -> dispatch (CreateSymbol (CommonTypes.ComponentType.DFF, 60., 50.)))
                            ][str "DFF"]
                        ] 
                      div [ Style [PaddingTop "4vh"; Margin "0"; PaddingBottom "0"]][
                            a [
                                Style [
                                    Height "5vh"
                                    TextAnchor "middle" // horizontal algnment vs (X,Y)
                                    DominantBaseline "middle" // vertical alignment vs (X,Y)
                                    FontSize "1.5vh"
                                    FontWeight "Bold"
                                    Fill "Gray" // font color
                                ]
                                OnClick (fun _ -> dispatch (CreateSymbol (CommonTypes.ComponentType.DFFE, 100., 80.)))
                            ][str "DFFE"]
                        ] 
                      div [ Style [PaddingTop "4vh"; Margin "0"; PaddingBottom "0"]][
                            a [
                                Style [
                                    Height "5vh"
                                    TextAnchor "middle" // horizontal algnment vs (X,Y)
                                    DominantBaseline "middle" // vertical alignment vs (X,Y)
                                    FontSize "1.5vh"
                                    FontWeight "Bold"
                                    Fill "Gray" // font color
                                ]
                                OnClick (fun _ -> dispatch (CreateSymbol (CommonTypes.ComponentType.Register(testBusWidth), 100., 120.)))
                            ][str "Register"]
                        ] 
                      div [ Style [PaddingTop "4vh"; Margin "0"; PaddingBottom "0"]][
                            a [
                                Style [
                                    Height "5vh"
                                    TextAnchor "middle" // horizontal algnment vs (X,Y)
                                    DominantBaseline "middle" // vertical alignment vs (X,Y)
                                    FontSize "1.5vh"
                                    FontWeight "Bold"
                                    Fill "Gray" // font color
                                ]
                                OnClick (fun _ -> dispatch (CreateSymbol (CommonTypes.ComponentType.RegisterE(testBusWidth), 100., 120.)))
                            ][str "RegisterE"]
                        ]  
                      div [ Style [PaddingTop "4vh"; Margin "0"; PaddingBottom "0"]][
                            a [
                                Style [
                                    Height "5vh"
                                    TextAnchor "middle" // horizontal algnment vs (X,Y)
                                    DominantBaseline "middle" // vertical alignment vs (X,Y)
                                    FontSize "1.5vh"
                                    FontWeight "Bold"
                                    Fill "Gray" // font color
                                ]
                                OnClick (fun _ -> dispatch (CreateSymbol (CommonTypes.ComponentType.AsyncROM(testMemory), 150., 100.)))
                            ][str "AsyncROM"]
                        ] 
                      div [ Style [PaddingTop "4vh"; Margin "0"; PaddingBottom "0"]][
                            a [
                                Style [
                                    Height "5vh"
                                    TextAnchor "middle" // horizontal algnment vs (X,Y)
                                    DominantBaseline "middle" // vertical alignment vs (X,Y)
                                    FontSize "1.5vh"
                                    FontWeight "Bold"
                                    Fill "Gray" // font color
                                ]
                                OnClick (fun _ -> dispatch (CreateSymbol (CommonTypes.ComponentType.ROM(testMemory), 150., 100.)))
                            ][str "ROM"]
                        ] 
                      div [ Style [PaddingTop "4vh"; Margin "0"; PaddingBottom "0"]][
                            a [
                                Style [
                                    Height "5vh"
                                    TextAnchor "middle" // horizontal algnment vs (X,Y)
                                    DominantBaseline "middle" // vertical alignment vs (X,Y)
                                    FontSize "1.5vh"
                                    FontWeight "Bold"
                                    Fill "Gray" // font color
                                ]
                                OnClick (fun _ -> dispatch (CreateSymbol (CommonTypes.ComponentType.RAM(testMemory), 150., 160.)))
                            ][str "RAM"]
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
    | CreateSymbol (compType, width, height) ->

        // finds position to insert new symbol (i.e. ensuring no collision with existing symbols)
        let pos = Symbol.findNextAvailablePos (model.Wire.Symbol) (width, height)

        // define symbol vertices from: top left -> top right -> bottom right -> bottom left
        //unused
        let vertices = [{X=pos.X - width; Y=pos.Y - height}; {X=pos.X + width; Y=pos.Y - height}; {X=pos.X + width; Y=pos.Y + height}; {X=pos.X - width; Y=pos.Y + height}]

        // all other information comes from user input
        let portWidth = model.ComponentInfo.PortWidth
        //unused
        // let label = model.ComponentInfo.ComponentLabel
        // let inputPortLength = model.ComponentInfo.NumInputPorts
        // let outputPortLength = model.ComponentInfo.NumOutputPorts 

        // return updated model
        let newModel, newCmd = Symbol.update (Symbol.Msg.AddSymbol (compType, portWidth, pos)) model.Wire.Symbol
        //let newModel, newCmd = Symbol.update (Symbol.Msg.AddSymbol (pos, portWidth, label, vertices, inputPortLength, outputPortLength)) model.Wire.Symbol
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
                // send mouse message to Buswire
                let updatedWire, _ = BusWire.update (BusWire.Msg.MouseMsg mMsg) model.Wire
                {model with Wire=updatedWire}, Cmd.none

            | None, false ->
                // initialize DragBox and DragWire
                let newDragBox = {Edge1 = pos; Edge2 = pos; isDragging = true}
                let newDragWire = {SrcEdge = pos; TargetEdge = pos; isDragging = false; DraggingPort=CommonTypes.PortType.Input}
                let updatedWire, _ = BusWire.update (BusWire.Msg.MouseMsg mMsg) model.Wire
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
                        |> Symbol.update (Symbol.Msg.ExpandPort (model.DragWire.DraggingPort, wire.SrcPort.Width))
                        |> fst

                    {model with Wire={model.Wire with WX=updatedWire.WX; Symbol=newSymbol}; DragBox=newDragBox; DragWire=newDragWire}, Cmd.none
                
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
                    {model with Wire={model.Wire with Symbol=newSymbol}; DragBox=newDragBox; DragWire=newDragWire}, Cmd.none
                    
        // mouse up
        | Up ->
            // reset dragBox and dragWire (i.e. so it is not displayed when mouse up)
            let resetDragBox = {Edge1=pos; Edge2=pos; isDragging=false}
            let resetDragWire = {SrcEdge=pos; TargetEdge=pos; isDragging=false; DraggingPort=CommonTypes.PortType.Input}
           
            // make all ports not expanded / highlighted (since mouse is lifted)
            let newSymbol = 
                match Symbol.isSymbolHoveredAndSelected model.Wire.Symbol pos with
                // if mouse lifted on top of selected symbol, all symbols remain selected
                | true -> model.Wire.Symbol

                // otherwise, deselect all symbols
                | false -> fst (Symbol.update (Symbol.Msg.BoxSelected (model.DragBox.Edge1, model.DragBox.Edge2)) model.Wire.Symbol)
                |> List.map (fun sym -> {sym with ExpandedPort = None})

            // find if mouse up occurs at any port
            let selectedPort = Symbol.findPortByPosition model.Wire.Symbol pos

            match selectedPort with
            // if no port selected, return model with updated symbol
            | None ->
                {model with Wire={model.Wire with Symbol=newSymbol}; DragBox=resetDragBox; DragWire=resetDragWire}, Cmd.none
            
            // if port selected, add new wire between two ports if wire was dragging
            | Some port ->
                match model.DragWire.isDragging with
                | false -> {model with Wire={model.Wire with Symbol=newSymbol}; DragBox=resetDragBox}, Cmd.none

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
                        // return updated model
                        let newWire, _ = BusWire.update (BusWire.Msg.AddWire (originPort, port)) (model.Wire)
                        {model with Wire={model.Wire with WX=newWire.WX; Symbol=newSymbol}; DragBox=resetDragBox}, Cmd.none

                    | None ->
                        // return updated model
                        {model with Wire={model.Wire with Symbol=newSymbol}; DragBox=resetDragBox}, Cmd.none

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
            
            // send mouse message to Buswire
            let updatedWire, _ = BusWire.update (BusWire.Msg.MouseMsg mMsg) model.Wire

            // return updated model
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
                Symbol.update Symbol.Msg.SymbolOverlap hoverCheck //checking if symbol overlaps any other symbol in model after checking if hovering over symbol

            // move wire alongside its corresponding ports if symbol moved
            let newWX = 
                model.Wire.WX 
                |> List.map (fun wire -> 
                    // find new source and target symbol positions
                    let newSrcPos = Symbol.symbolPortPos model.Wire.Symbol wire.SrcPort.Id //fixed to correct function name and call parameters
                    let newTargetPos = Symbol.symbolPortPos model.Wire.Symbol wire.TargetPort.Id

                    // find new source and target ports with updated positions
                    let newSrcPort = {wire.SrcPort with Pos=newSrcPos}
                    let newTargetPort = {wire.TargetPort with Pos=newTargetPos}

                    {wire with SrcPort = newSrcPort; TargetPort = newTargetPort}
                )

            // return updated model
            {model with Wire={model.Wire with WX=newWX; Symbol=newSymbol}; DragBox=resetDragBox; DragWire=resetDragWire}, Cmd.none

/// Initialization
let init() = 
    let model,cmds = (BusWire.init 1)()
    let dragBoxInit = {Edge1={X=100.; Y=100.}; Edge2={X=100.; Y=100.}; isDragging=false}
    let dragWireInit = {SrcEdge={X=100.; Y=100.}; TargetEdge={X=100.; Y=100.}; isDragging=false; DraggingPort=CommonTypes.PortType.Input}

    {
        Wire = model
        ComponentInfo = {PortWidth = 1; ComponentLabel = "C1"; NumInputPorts = 1; NumOutputPorts = 1}
        DragBox=dragBoxInit
        DragWire=dragWireInit

    }, Cmd.map Wire cmds
