module Sheet
open Fable.React
open Fable.React.Props
open Browser
open Elmish
open Elmish.React
open Helpers
open type CommonTypes.ComponentType

//------------------------------------------------------------------------//
//------------------------------Sheets Types------------------------------//
//------------------------------------------------------------------------//

type CompInfo = {
    // --------------- This section is for the interface (replacing ISSIE) ------------
    // for input and output
    InputWidth: int
    OutputWidth: int

    // for bus selection component
    BusSelectionOutWidth: int
    BusSelectionLSB: int

    // for constant
    ConstantValue: int
    ConstantWidth: int

    // for AsyncROM
    AsyncROMMemBits: int
    AsyncROMOutWidth: int
    
    // for ROM
    ROMMemBits: int
    ROMOutWidth: int

    // for RAM
    RAMMemBits: int
    RAMOutWidth: int

    // for N-bits Adder
    AdderBits: int

    // for SplitWires
    SplitOutWidth: int

    // for Register
    RegWidth: int

    // for RegisterEnabled
    RegEnabledWidth: int

    // for IOLabel
    IOLabelName: string

    // for CustomComponent
    CustComponentName: string
    CustComponentInpPortsList: string
    CustComponentOutPortsList: string
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
    | MouseMsg of MouseT

    // This section is for handling user-defined parameters for Interface (replacing ISSIE)
    // for input and output
    | ChangeInputWidth of int
    | ChangeOutputWidth of int

    // for busSelection
    | ChangeBusSelectionOutWidth of int
    | ChangeBusSelectionLSB of int

    // for constant
    | ChangeConstantValue of int
    | ChangeConstantWidth of int

    // for AsyncROM
    | ChangeAsyncROMMemBits of int
    | ChangeAsyncROMOutWidth of int
    
    // for ROM
    | ChangeROMMemBits of int
    | ChangeROMOutWidth of int

    // for RAM
    | ChangeRAMMemBits of int
    | ChangeRAMOutWidth of int

    // for N-bits Adder
    | ChangeAdderBits of int

    // for SplitWires
    | ChangeSplitOutWidth of int

    // for Register
    | ChangeRegWidth of int

    // for RegisterEnabled
    | ChangeRegEnabledWidth of int

    // for IOLabel
    | ChangeIOLabelName of string

    // for CustomComponent
    | ChangeCustComponentName of string
    | ChangeCustComponentInpPortsList of string
    | ChangeCustComponentOutPortsList of string


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

          // ----------------------- FOR DEMO INTERFACE (NO DRAW2DCANVAS IMPLEMENTATION HERE) ------------------------------
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
                      div [ Style [Width "25%"; Float FloatOptions.Left]]  [
                          // Not           
                          div [ Style [PaddingTop "2.5vh"; Margin "0"; PaddingBottom "0"]][
                                a [
                                    Style [
                                        Height "3vh"
                                        TextAnchor "middle" 
                                        DominantBaseline "middle" 
                                        FontSize "1.6vh"
                                        FontWeight "Bold"
                                        Fill "Gray" 
                                    ]
                                    OnClick (fun _ -> dispatch (CreateSymbol (CommonTypes.ComponentType.Not, 40., 50.)))
                                ][str "Not"]
                          ]    
                          // Or
                          div [ Style [PaddingTop "2.5vh"; Margin "0"; PaddingBottom "0"]][
                                a [
                                    Style [
                                        Height "3vh"
                                        TextAnchor "middle" // horizontal algnment vs (X,Y)
                                        DominantBaseline "middle" // vertical alignment vs (X,Y)
                                        FontSize "1.6vh"
                                        FontWeight "Bold"
                                        Fill "Gray" // font color
                                    ]
                                    OnClick (fun _ -> dispatch (CreateSymbol (CommonTypes.ComponentType.Or, 60., 70.)))
                                ][str "Or"]
                          ]
                          // And
                          div [ Style [PaddingTop "2.5vh"; Margin "0"; PaddingBottom "0"]][
                                a [
                                    Style [
                                        Height "3vh"
                                        TextAnchor "middle" // horizontal algnment vs (X,Y)
                                        DominantBaseline "middle" // vertical alignment vs (X,Y)
                                        FontSize "1.6vh"
                                        FontWeight "Bold"
                                        Fill "Gray" // font color
                                    ]
                                    OnClick (fun _ -> dispatch (CreateSymbol (CommonTypes.ComponentType.And, 60., 70.)))
                                ][str "And"]
                          ] 
                      ]

                      div [ Style [Width "25%"; Float FloatOptions.Left]]  [
                          // Nor
                          div [ Style [PaddingTop "2.5vh"; Margin "0"; PaddingBottom "0"]][
                                a [
                                    Style [
                                        Height "3vh"
                                        TextAnchor "middle" // horizontal algnment vs (X,Y)
                                        DominantBaseline "middle" // vertical alignment vs (X,Y)
                                        FontSize "1.6vh"
                                        FontWeight "Bold"
                                        Fill "Gray" // font color
                                    ]
                                    OnClick (fun _ -> dispatch (CreateSymbol (CommonTypes.ComponentType.Nor, 60., 70.)))
                                ][str "Nor"]
                          ]

                          // Xor
                          div [ Style [PaddingTop "2.5vh"; Margin "0"; PaddingBottom "0"]][
                            a [
                                Style [
                                    Height "3vh"
                                    TextAnchor "middle" // horizontal algnment vs (X,Y)
                                    DominantBaseline "middle" // vertical alignment vs (X,Y)
                                    FontSize "1.6vh"
                                    FontWeight "Bold"
                                    Fill "Gray" // font color
                                ]
                                OnClick (fun _ -> dispatch (CreateSymbol (CommonTypes.ComponentType.Xor, 60., 70.)))
                            ][str "Xor"]
                          ]

                          // Nand
                          div [ Style [PaddingTop "2.5vh"; Margin "0"; PaddingBottom "0"]][
                                a [
                                    Style [
                                        Height "3vh"
                                        TextAnchor "middle" // horizontal algnment vs (X,Y)
                                        DominantBaseline "middle" // vertical alignment vs (X,Y)
                                        FontSize "1.6vh"
                                        FontWeight "Bold"
                                        Fill "Gray" // font color
                                    ]
                                    OnClick (fun _ -> dispatch (CreateSymbol (CommonTypes.ComponentType.Nand, 60., 70.)))
                                ][str "Nand"]
                            ]
                      ]

                      div [ Style [Width "25%"; Float FloatOptions.Left]]  [
                          // Xnor
                          div [ Style [PaddingTop "2.5vh"; Margin "0"; PaddingBottom "0"]][
                                a [
                                    Style [
                                        Height "3vh"
                                        TextAnchor "middle" // horizontal algnment vs (X,Y)
                                        DominantBaseline "middle" // vertical alignment vs (X,Y)
                                        FontSize "1.6vh"
                                        FontWeight "Bold"
                                        Fill "Gray" // font color
                                    ]
                                    OnClick (fun _ -> dispatch (CreateSymbol (CommonTypes.ComponentType.Xnor, 60., 70.)))
                                ][str "Xnor"]
                           ]

                           // Decode4
                          div [ Style [PaddingTop "2.5vh"; Margin "0"; PaddingBottom "0"]][
                                a [
                                    Style [
                                        Height "3vh"
                                        TextAnchor "middle" // horizontal algnment vs (X,Y)
                                        DominantBaseline "middle" // vertical alignment vs (X,Y)
                                        FontSize "1.6vh"
                                        FontWeight "Bold"
                                        Fill "Gray" // font color
                                    ]
                                    OnClick (fun _ -> dispatch (CreateSymbol (CommonTypes.ComponentType.Decode4, 170., 100.)))
                                ][str "Decode4"]
                           ]

                          // Mux2
                          div [ Style [PaddingTop "2.5vh"; Margin "0"; PaddingBottom "0"]][
                                a [
                                    Style [
                                        Height "3vh"
                                        TextAnchor "middle" // horizontal algnment vs (X,Y)
                                        DominantBaseline "middle" // vertical alignment vs (X,Y)
                                        FontSize "1.6vh"
                                        FontWeight "Bold"
                                        Fill "Gray" // font color
                                    ]
                                    OnClick (fun _ -> dispatch (CreateSymbol (CommonTypes.ComponentType.Mux2, 70., 50.)))
                                ][str "Mux2"]
                            ]
                      ]

                      div [ Style [Width "25%"; Float FloatOptions.Left]]  [
                          // Demux2
                          div [ Style [PaddingTop "2.5vh"; Margin "0"; PaddingBottom "0"]][
                                a [
                                    Style [
                                        Height "3vh"
                                        TextAnchor "middle" // horizontal algnment vs (X,Y)
                                        DominantBaseline "middle" // vertical alignment vs (X,Y)
                                        FontSize "1.6vh"
                                        FontWeight "Bold"
                                        Fill "Gray" // font color
                                    ]
                                    OnClick (fun _ -> dispatch (CreateSymbol (CommonTypes.ComponentType.Demux2, 70., 50.)))
                                ][str "Demux2"]
                          ]  
                          // DFF
                          div [ Style [PaddingTop "2.5vh"; Margin "0"; PaddingBottom "0"]][
                                a [
                                    Style [
                                        Height "3vh"
                                        TextAnchor "middle" // horizontal algnment vs (X,Y)
                                        DominantBaseline "middle" // vertical alignment vs (X,Y)
                                        FontSize "1.6vh"
                                        FontWeight "Bold"
                                        Fill "Gray" // font color
                                    ]
                                    OnClick (fun _ -> dispatch (CreateSymbol (CommonTypes.ComponentType.DFF, 60., 50.)))
                                ][str "DFF"]
                            ] 
                          
                           // DFFE
                          div [ Style [PaddingTop "2.5vh"; Margin "0"; PaddingBottom "0"]][
                                a [
                                    Style [
                                        Height "3vh"
                                        TextAnchor "middle" // horizontal algnment vs (X,Y)
                                        DominantBaseline "middle" // vertical alignment vs (X,Y)
                                        FontSize "1.6vh"
                                        FontWeight "Bold"
                                        Fill "Gray" // font color
                                    ]
                                    OnClick (fun _ -> dispatch (CreateSymbol (CommonTypes.ComponentType.DFFE, 100., 80.)))
                                ][str "DFFE"]
                            ]        
                      ]

                      div [ Style [Width "50%"; Float FloatOptions.Left]]  [
                          // Input
                          div [ Style [PaddingTop "2.5vh"; Margin "0"; PaddingBottom "0"]][
                            a [
                                Style [
                                    Height "3vh"
                                    TextAnchor "middle" 
                                    DominantBaseline "middle" 
                                    FontSize "1.6vh"
                                    FontWeight "Bold"
                                    Fill "Gray" 
                                ]
                                OnClick (fun _ -> dispatch (CreateSymbol (CommonTypes.ComponentType.Input(model.ComponentInfo.InputWidth), 30., 60.)))
                            ][str "Input"]
                          ] 
                          div [ Style [PaddingTop "0.5vh"]][
                            input [   
                                     Type "number"
                                     Placeholder "width"
                                     OnChange (fun ev -> 
                                                if (int ev.Value < 1) then dispatch (ChangeInputWidth(1))
                                                else dispatch (ChangeInputWidth(int ev.Value)))

                                     Style [
                                         Width "50%"
                                         FontSize "1.2vh"
                                         Height "2.3vh"
                                         TextAlign TextAlignOptions.Center
                                    ]
                                 ]
                            ]  


                          // Output
                          div [ Style [PaddingTop "2.5vh"; Margin "0"; PaddingBottom "0"]][
                                a [
                                    Style [
                                        Height "3vh"
                                        TextAnchor "middle" 
                                        DominantBaseline "middle" 
                                        FontSize "1.6vh"
                                        FontWeight "Bold"
                                        Fill "Gray" 
                                    ]
                                    OnClick (fun _ -> dispatch (CreateSymbol (CommonTypes.ComponentType.Output(model.ComponentInfo.OutputWidth), 30., 60.)))
                                ][str "Output"]
                          ]     
                          div [ Style [PaddingTop "0.5vh"]][
                            input [   
                                     Type "number"
                                     Placeholder "width"
                                     OnChange (fun ev -> 
                                                if (int ev.Value < 1) then dispatch (ChangeOutputWidth(1))
                                                else dispatch (ChangeOutputWidth(int ev.Value)))

                                     Style [
                                         Width "50%"
                                         FontSize "1.2vh"
                                         Height "2.3vh"
                                         TextAlign TextAlignOptions.Center
                                    ]
                                 ]
                            ]    


                          // Bus Selection
                          div [ Style [PaddingTop "2.5vh"; Margin "0"; PaddingBottom "0"]][
                                a [
                                    Style [
                                        Height "5vh"
                                        TextAnchor "middle" 
                                        DominantBaseline "middle" 
                                        FontSize "1.6vh"
                                        FontWeight "Bold"
                                        Fill "Gray" 
                                    ]
                                    OnClick (fun _ -> dispatch (CreateSymbol (CommonTypes.ComponentType.BusSelection(model.ComponentInfo.BusSelectionOutWidth, model.ComponentInfo.BusSelectionLSB), 30., 60.)))
                                ][str "BusSelection"]
                          ] 
                          div [ Style [PaddingTop "0.5vh"]][
                            input [   
                                     Type "number"
                                     Placeholder "output width"
                                     OnChange (fun ev -> 
                                                if (int ev.Value < 1) then dispatch (ChangeBusSelectionOutWidth(1))
                                                else dispatch (ChangeBusSelectionOutWidth(int ev.Value)))

                                     Style [
                                         Width "50%"
                                         FontSize "1.2vh"
                                         Height "2.3vh"
                                         TextAlign TextAlignOptions.Center
                                    ]
                                 ]
                            ]   
                          div [ Style [PaddingTop "0.5vh"]][
                            input [   
                                     Type "number"
                                     Placeholder "LS output bit"
                                     OnChange (fun ev -> 
                                                if (int ev.Value < 1) then dispatch (ChangeBusSelectionLSB(1))
                                                else dispatch (ChangeBusSelectionLSB(int ev.Value)))

                                     Style [
                                         Width "50%"
                                         FontSize "1.2vh"
                                         Height "2.3vh"
                                         TextAlign TextAlignOptions.Center
                                    ]
                                 ]
                            ]   


                          // Constant
                          div [ Style [PaddingTop "2.5vh"; Margin "0"; PaddingBottom "0"]][
                                a [
                                    Style [
                                        Height "3vh"
                                        TextAnchor "middle" 
                                        DominantBaseline "middle" 
                                        FontSize "1.6vh"
                                        FontWeight "Bold"
                                        Fill "Gray" 
                                    ]
                                    OnClick (fun _ -> dispatch (CreateSymbol (CommonTypes.ComponentType.Constant(model.ComponentInfo.ConstantWidth, model.ComponentInfo.ConstantValue), 20., 50.)))
                                ][str "Constant"]
                          ]
                          div [ Style [PaddingTop "0.5vh"]][
                            input [   
                                     Type "number"
                                     Placeholder "value"
                                     OnChange (fun ev -> dispatch (ChangeConstantValue(int ev.Value)))

                                     Style [
                                         Width "50%"
                                         FontSize "1.2vh"
                                         Height "2.3vh"
                                         TextAlign TextAlignOptions.Center
                                    ]
                                 ]
                            ]  
                          div [ Style [PaddingTop "0.5vh"]][
                            input [   
                                     Type "number"
                                     Placeholder "width"
                                     OnChange (fun ev -> 
                                                if (int ev.Value < 1) then dispatch (ChangeConstantWidth(1))
                                                else dispatch (ChangeConstantWidth(int ev.Value)))

                                     Style [
                                         Width "50%"
                                         FontSize "1.2vh"
                                         Height "2.3vh"
                                         TextAlign TextAlignOptions.Center
                                    ]
                                 ]
                            ]  


                          // AsyncROM
                          div [ Style [PaddingTop "2.5vh"; Margin "0"; PaddingBottom "0"]][
                                a [
                                    Style [
                                        Height "3vh"
                                        TextAnchor "middle" // horizontal algnment vs (X,Y)
                                        DominantBaseline "middle" // vertical alignment vs (X,Y)
                                        FontSize "1.6vh"
                                        FontWeight "Bold"
                                        Fill "Gray" // font color
                                    ]
                                    OnClick (fun _ -> 
                                            let curMemory : CommonTypes.Memory = {
                                                AddressWidth = model.ComponentInfo.AsyncROMMemBits
                                                WordWidth = model.ComponentInfo.AsyncROMOutWidth
                                                Data = Map.empty
                                            }
                                            dispatch (CreateSymbol (CommonTypes.ComponentType.AsyncROM(curMemory), 150., 100.)))
                                ][str "AsyncROM"]
                            ] 
                          div [ Style [PaddingTop "0.5vh"]][
                            input [   
                                     Type "number"
                                     Placeholder "mem addr bits"
                                     OnChange (fun ev -> 
                                                if (int ev.Value < 1) then dispatch (ChangeAsyncROMMemBits(1))
                                                else dispatch (ChangeAsyncROMMemBits(int ev.Value)))

                                     Style [
                                         Width "50%"
                                         FontSize "1.2vh"
                                         Height "2.3vh"
                                         TextAlign TextAlignOptions.Center
                                    ]
                                 ]
                            ]  
                          div [ Style [PaddingTop "0.5vh"]][
                            input [   
                                     Type "number"
                                     Placeholder "mem word size"
                                     OnChange (fun ev -> 
                                                if (int ev.Value < 1) then dispatch (ChangeAsyncROMOutWidth(1))
                                                else dispatch (ChangeAsyncROMOutWidth(int ev.Value)))

                                     Style [
                                         Width "50%"
                                         FontSize "1.2vh"
                                         Height "2.3vh"
                                         TextAlign TextAlignOptions.Center
                                    ]
                                 ]
                            ]  

                          // ROM
                          div [ Style [PaddingTop "2.5vh"; Margin "0"; PaddingBottom "0"]][
                                a [
                                    Style [
                                        Height "3vh"
                                        TextAnchor "middle" // horizontal algnment vs (X,Y)
                                        DominantBaseline "middle" // vertical alignment vs (X,Y)
                                        FontSize "1.6vh"
                                        FontWeight "Bold"
                                        Fill "Gray" // font color
                                    ]
                                    OnClick (fun _ -> 
                                            let curMemory : CommonTypes.Memory = {
                                                AddressWidth = model.ComponentInfo.ROMMemBits
                                                WordWidth = model.ComponentInfo.ROMOutWidth
                                                Data = Map.empty
                                            }
                                            dispatch (CreateSymbol (CommonTypes.ComponentType.ROM(curMemory), 150., 100.)))
                                ][str "ROM"]
                            ] 
                          div [ Style [PaddingTop "0.5vh"]][
                            input [   
                                     Type "number"
                                     Placeholder "mem addr bits"
                                     OnChange (fun ev -> 
                                                if (int ev.Value < 1) then dispatch (ChangeROMMemBits(1))
                                                else dispatch (ChangeROMMemBits(int ev.Value)))

                                     Style [
                                         Width "50%"
                                         FontSize "1.2vh"
                                         Height "2.3vh"
                                         TextAlign TextAlignOptions.Center
                                    ]
                                 ]
                            ]  
                          div [ Style [PaddingTop "0.5vh"]][
                            input [   
                                     Type "number"
                                     Placeholder "mem word size"
                                     OnChange (fun ev -> 
                                                if (int ev.Value < 1) then dispatch (ChangeROMOutWidth(1))
                                                else dispatch (ChangeROMOutWidth(int ev.Value)))

                                     Style [
                                         Width "50%"
                                         FontSize "1.2vh"
                                         Height "2.3vh"
                                         TextAlign TextAlignOptions.Center
                                    ]
                                 ]
                            ]

                          // RAM
                          div [ Style [PaddingTop "2.5vh"; Margin "0"; PaddingBottom "0"]][
                                a [
                                    Style [
                                        Height "3vh"
                                        TextAnchor "middle" // horizontal algnment vs (X,Y)
                                        DominantBaseline "middle" // vertical alignment vs (X,Y)
                                        FontSize "1.6vh"
                                        FontWeight "Bold"
                                        Fill "Gray" // font color
                                    ]
                                    OnClick (fun _ -> 
                                            let curMemory : CommonTypes.Memory = {
                                                AddressWidth = model.ComponentInfo.RAMMemBits
                                                WordWidth = model.ComponentInfo.RAMOutWidth
                                                Data = Map.empty
                                            }
                                            dispatch (CreateSymbol (CommonTypes.ComponentType.RAM(curMemory), 150., 160.)))
                                ][str "RAM"]
                            ] 
                          div [ Style [PaddingTop "0.5vh"]][
                            input [   
                                     Type "number"
                                     Placeholder "mem addr bits"
                                     OnChange (fun ev -> 
                                                if (int ev.Value < 1) then dispatch (ChangeRAMMemBits(1))
                                                else dispatch (ChangeRAMMemBits(int ev.Value)))

                                     Style [
                                         Width "50%"
                                         FontSize "1.2vh"
                                         Height "2.3vh"
                                         TextAlign TextAlignOptions.Center
                                    ]
                                 ]
                            ]  
                          div [ Style [PaddingTop "0.5vh"]][
                            input [   
                                     Type "number"
                                     Placeholder "mem word size"
                                     OnChange (fun ev -> 
                                                if (int ev.Value < 1) then dispatch (ChangeRAMOutWidth(1))
                                                else dispatch (ChangeRAMOutWidth(int ev.Value)))

                                     Style [
                                         Width "50%"
                                         FontSize "1.2vh"
                                         Height "2.3vh"
                                         TextAlign TextAlignOptions.Center
                                    ]
                                 ]
                            ]
                          
                      ]
                       
                      
                      div [ Style [Width "50%"; Float FloatOptions.Left]]  [
                          // N-Bit Adder
                          div [ Style [PaddingTop "2.5vh"; Margin "0"; PaddingBottom "0"]][
                                a [
                                    Style [
                                        Height "3vh"
                                        TextAnchor "middle" // horizontal algnment vs (X,Y)
                                        DominantBaseline "middle" // vertical alignment vs (X,Y)
                                        FontSize "1.6vh"
                                        FontWeight "Bold"
                                        Fill "Gray" // font color
                                    ]
                                    OnClick (fun _ -> dispatch (CreateSymbol (CommonTypes.ComponentType.NbitsAdder(model.ComponentInfo.AdderBits), 150., 100.)))
                                ][str "NbitsAdder"]
                            ]
                          div [ Style [PaddingTop "0.5vh"]][
                            input [   
                                     Type "number"
                                     Placeholder "operand bits"
                                     OnChange (fun ev -> 
                                                if (int ev.Value < 1) then dispatch (ChangeAdderBits(1))
                                                else dispatch (ChangeAdderBits(int ev.Value)))

                                     Style [
                                         Width "50%"
                                         FontSize "1.2vh"
                                         Height "2.3vh"
                                         TextAlign TextAlignOptions.Center
                                    ]
                                 ]
                            ]


                          // Merge Wires
                          div [ Style [PaddingTop "2.5vh"; Margin "0"; PaddingBottom "0"]][
                                a [
                                    Style [
                                        Height "3vh"
                                        TextAnchor "middle" // horizontal algnment vs (X,Y)
                                        DominantBaseline "middle" // vertical alignment vs (X,Y)
                                        FontSize "1.6vh"
                                        FontWeight "Bold"
                                        Fill "Gray" // font color
                                    ]
                                    OnClick (fun _ -> dispatch (CreateSymbol (CommonTypes.ComponentType.MergeWires, 80., 80.)))
                                ][str "MergeWires"]
                            ]           

                          // Split Wire
                          div [ Style [PaddingTop "2.5vh"; Margin "0"; PaddingBottom "0"]][
                                a [
                                    Style [
                                        Height "3vh"
                                        TextAnchor "middle" // horizontal algnment vs (X,Y)
                                        DominantBaseline "middle" // vertical alignment vs (X,Y)
                                        FontSize "1.6vh"
                                        FontWeight "Bold"
                                        Fill "Gray" // font color
                                    ]
                                    OnClick (fun _ -> dispatch (CreateSymbol (CommonTypes.ComponentType.SplitWire(model.ComponentInfo.SplitOutWidth), 80., 80.)))
                                ][str "SplitWire"]
                            ]  
                          div [ Style [PaddingTop "0.5vh"]][
                            input [   
                                     Type "number"
                                     Placeholder "top LSB bits"
                                     OnChange (fun ev -> 
                                                if (int ev.Value < 1) then dispatch (ChangeSplitOutWidth(1))
                                                else dispatch (ChangeSplitOutWidth(int ev.Value)))

                                     Style [
                                         Width "50%"
                                         FontSize "1.2vh"
                                         Height "2.3vh"
                                         TextAlign TextAlignOptions.Center
                                    ]
                                 ]
                            ]


                          // Register
                          div [ Style [PaddingTop "2.5vh"; Margin "0"; PaddingBottom "0"]][
                                a [
                                    Style [
                                        Height "3vh"
                                        TextAnchor "middle" // horizontal algnment vs (X,Y)
                                        DominantBaseline "middle" // vertical alignment vs (X,Y)
                                        FontSize "1.6vh"
                                        FontWeight "Bold"
                                        Fill "Gray" // font color
                                    ]
                                    OnClick (fun _ -> dispatch (CreateSymbol (CommonTypes.ComponentType.Register(model.ComponentInfo.RegWidth), 100., 120.)))
                                ][str "Register"]
                            ] 
                          div [ Style [PaddingTop "0.5vh"]][
                            input [   
                                     Type "number"
                                     Placeholder "width"
                                     OnChange (fun ev -> 
                                                if (int ev.Value < 1) then dispatch (ChangeRegWidth(1))
                                                else dispatch (ChangeRegWidth(int ev.Value)))

                                     Style [
                                         Width "50%"
                                         FontSize "1.2vh"
                                         Height "2.3vh"
                                         TextAlign TextAlignOptions.Center
                                    ]
                                 ]
                            ]

                          // Register with Enable
                          div [ Style [PaddingTop "2.5vh"; Margin "0"; PaddingBottom "0"]][
                                a [
                                    Style [
                                        Height "3vh"
                                        TextAnchor "middle" // horizontal algnment vs (X,Y)
                                        DominantBaseline "middle" // vertical alignment vs (X,Y)
                                        FontSize "1.6vh"
                                        FontWeight "Bold"
                                        Fill "Gray" // font color
                                    ]
                                    OnClick (fun _ -> dispatch (CreateSymbol (CommonTypes.ComponentType.RegisterE(model.ComponentInfo.RegEnabledWidth), 100., 120.)))
                                ][str "RegisterE"]
                            ] 
                          div [ Style [PaddingTop "0.5vh"]][
                            input [   
                                     Type "number"
                                     Placeholder "width"
                                     OnChange (fun ev -> 
                                                if (int ev.Value < 1) then dispatch (ChangeRegEnabledWidth(1))
                                                else dispatch (ChangeRegEnabledWidth(int ev.Value)))

                                     Style [
                                         Width "50%"
                                         FontSize "1.2vh"
                                         Height "2.3vh"
                                         TextAlign TextAlignOptions.Center
                                    ]
                                 ]
                            ]

                          // IOLabel
                          div [ Style [PaddingTop "2.5vh"; Margin "0"; PaddingBottom "0"]][
                                a [
                                    Style [
                                        Height "3vh"
                                        TextAnchor "middle" // horizontal algnment vs (X,Y)
                                        DominantBaseline "middle" // vertical alignment vs (X,Y)
                                        FontSize "1.6vh"
                                        FontWeight "Bold"
                                        Fill "Gray" // font color
                                    ]
                                    OnClick (fun _ -> dispatch (CreateSymbol (CommonTypes.ComponentType.IOLabel, 100., 80.)))
                                ][str "IOLabel"]
                            ] 
                          div [ Style [PaddingTop "0.5vh"]][
                            input [   
                                     Type "text"
                                     Placeholder "name "
                                     OnChange (fun ev -> dispatch (ChangeIOLabelName(ev.Value)))

                                     Style [
                                         Width "50%"
                                         FontSize "1.2vh"
                                         Height "2.3vh"
                                         TextAlign TextAlignOptions.Center
                                    ]
                                 ]
                            ]

                          // Custom Component
                          div [ Style [PaddingTop "2.5vh"; Margin "0"; PaddingBottom "0"]][
                                a [
                                    Style [
                                        Height "3vh"
                                        TextAnchor "middle" // horizontal algnment vs (X,Y)
                                        DominantBaseline "middle" // vertical alignment vs (X,Y)
                                        FontSize "1.6vh"
                                        FontWeight "Bold"
                                        Fill "Gray" // font color
                                    ]
                                    OnClick (fun _ -> 
                                            // reads custom component string and convert to list
                                            let parseStringToList (inpString: string) : (string * int) list = 
                                                let rec getParsedList (stringArray: char list) (readLabel: bool) (readNumber: bool) (label: string) (width: string) = 
                                                    match stringArray with 
                                                    // stop condition 
                                                    | [] -> []
                                                    | (hd::_) when hd = ']' -> []

                                                    // start reading label name
                                                    | (hd::tl) when hd = '(' -> getParsedList (tl) (true) (false) ("") (width) 

                                                    // stop reading label name & start reading number
                                                    | (hd::tl) when hd = ',' -> getParsedList (tl) (false) (true) (label) (width)

                                                    // stop reading number & finish reading tuple
                                                    | (hd::tl) when hd = ')' -> (label, int width)::(getParsedList (tl) (false) (false) ("") (""))

                                                    // read label name 
                                                    | (hd::tl) when readLabel -> getParsedList (tl) (true) (false) (label + string hd) (width)

                                                    // read number 
                                                    | (hd::tl) when readNumber-> getParsedList (tl) (false) (true) (label) (width + string hd)

                                                    // ignore other characters
                                                    | (_::tl) -> getParsedList (tl) (readLabel) (readNumber) (label) (width)

                                                let stringList = Seq.toList inpString
                                                getParsedList (stringList) (false) (false) ("") ("")


                                            let custComp : CommonTypes.CustomComponentType = {
                                                Name = model.ComponentInfo.CustComponentName
                                                // List of tuples with (label * connection width).
     
                                                InputLabels = parseStringToList model.ComponentInfo.CustComponentInpPortsList
                                                OutputLabels = parseStringToList model.ComponentInfo.CustComponentOutPortsList
                                            }

                                            let height = 
                                                match custComp.InputLabels.Length >= custComp.OutputLabels.Length with 
                                                | true -> (float custComp.InputLabels.Length) * 40.
                                                | false -> (float custComp.OutputLabels.Length) * 40.

                                            dispatch (CreateSymbol (CommonTypes.ComponentType.Custom(custComp), 140., height)))

                                ][str "Custom Component"]
                            ]   
                          div [ Style [PaddingTop "0.5vh"]][
                            input [   
                                     Type "text"
                                     Placeholder "name"
                                     OnChange (fun ev -> dispatch (ChangeCustComponentName(ev.Value)))

                                     Style [
                                         Width "50%"
                                         FontSize "1.2vh"
                                         Height "2.3vh"
                                         TextAlign TextAlignOptions.Center
                                    ]
                                 ]
                            ]
                          div [ Style [PaddingTop "0.5vh"]][
                            input [   
                                     Type "text"
                                     Placeholder "inp {(string*int) list}"
                                     OnChange (fun ev -> dispatch (ChangeCustComponentInpPortsList(ev.Value)))

                                     Style [
                                         Width "50%"
                                         FontSize "1.2vh"
                                         Height "2.3vh"
                                         TextAlign TextAlignOptions.Center
                                    ]
                                 ]
                            ]
                          div [ Style [PaddingTop "0.5vh"]][
                            input [   
                                     Type "text"
                                     Placeholder "out {(string*int) list}"
                                     OnChange (fun ev -> dispatch (ChangeCustComponentOutPortsList(ev.Value)))

                                     Style [
                                         Width "50%"
                                         FontSize "1.2vh"
                                         Height "2.3vh"
                                         TextAlign TextAlignOptions.Center
                                    ]
                                 ]
                            ]
                          div [ Style [PaddingTop "0.5vh"]][
                            text [ 
                                Style [
                                    TextAnchor "middle" 
                                    DominantBaseline "middle" 
                                    FontSize "1.2vh"
                                    FontWeight "Bold"
                                    Fill "Gray" 
                                ]
                            ] [str "Example: [(port1, 2); (port2, 3)]"]
                          ]

                      ]
  
                  ]
            ]
            // ------------------------------ END OF DEMO INTERFACE --------------------------------------
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

        let compName = 
            match compType with 
            | IOLabel -> model.ComponentInfo.IOLabelName
            | Custom _ -> model.ComponentInfo.CustComponentName
            | _ -> ""

        // return updated model
        let newModel, newCmd = Symbol.update (Symbol.Msg.AddSymbol (compType, pos, compName)) model.Wire.Symbol
        {model with Wire = {model.Wire with Symbol = newModel}}, newCmd

    // deletes symbol
    | KeyPress DEL ->
        let newModel, _ = BusWire.update (BusWire.Msg.DeleteWiresBySymbol) model.Wire
        {model with Wire = newModel}, Cmd.none

    // sets color for model
    | KeyPress s -> 
        match s with
        | AltC -> 
            let newWires, _ = BusWire.update (BusWire.Msg.CopyWires) model.Wire
            {model with Wire = newWires}, Cmd.none

        | AltV -> 
            let newWires, _ = BusWire.update (BusWire.Msg.PasteWires) model.Wire
            {model with Wire = newWires}, Cmd.none
        
        | _ ->
            model, Cmd.none

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
                // send mouse message to Buswire if no symbol is dragging
                if symbolDraggingCheck then model, Cmd.none
                else
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
                        |> Symbol.update (Symbol.Msg.ExpandPort (port.PortType, wire.SrcPort.Width))
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
                |> List.map (fun sym -> {sym with ExpandedPort = (None, None)})

            let updatedWire, _ = BusWire.update (BusWire.Msg.SelectWiresFromSymbol) {model.Wire with Symbol=newSymbol}

            // find if mouse up occurs at any port
            let selectedPort = Symbol.findPortByPosition model.Wire.Symbol pos

            match selectedPort with
            // if no port selected, return model with updated symbol
            | None ->
                {model with Wire=updatedWire; DragBox=resetDragBox; DragWire=resetDragWire}, Cmd.none
            
            // if port selected, add new wire between two ports if wire was dragging
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
                        // return updated model
                        let newWire, _ = BusWire.update (BusWire.Msg.AddWire (originPort, port)) (updatedWire)
                        {model with Wire=newWire; DragBox=resetDragBox}, Cmd.none

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
            
            // send mouse message to Buswire if no symbol dragging
            let symbolDraggingCheck = Symbol.isAnySymbolDragging (model.Wire.Symbol)

            match symbolDraggingCheck with 
            | true -> 
                {model with DragBox=newDragBox; DragWire=newDragWire}, Cmd.none
            
            | false ->
                // send message to buswire
                let updatedWire, _ = BusWire.update (BusWire.Msg.MouseMsg mMsg) model.Wire
                {model with Wire=updatedWire; DragBox=newDragBox; DragWire = newDragWire}, Cmd.none

        // mouse move
        | Move -> 
            // reset dragBox and dragWire
            let resetDragBox = {Edge1=pos; Edge2=pos; isDragging=false}
            let resetDragWire = {SrcEdge=pos; TargetEdge=pos; isDragging=false; DraggingPort=CommonTypes.PortType.Input}
           
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
            {model with Wire={model.Wire with WX=newWX; Symbol=newSymbol}; DragBox=resetDragBox; DragWire=resetDragWire}, Cmd.none

    // ------------------------- This section performs parameter update based on user inputs ----------------------
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
        // initialize user-defined values
        ComponentInfo = {InputWidth = 1; OutputWidth = 1; BusSelectionOutWidth = 2; BusSelectionLSB = 0; ConstantValue = 1; ConstantWidth = 1;
                        AsyncROMMemBits = 1; AsyncROMOutWidth = 1; ROMMemBits = 1; ROMOutWidth = 1; RAMMemBits = 1; RAMOutWidth = 1;
                        AdderBits = 1; SplitOutWidth = 1; RegWidth = 1; 
                        RegEnabledWidth = 1; IOLabelName = "wireLabel"; CustComponentName = "CustomName"; CustComponentInpPortsList = "[(in1, 1); (in2, 1)]";
                        CustComponentOutPortsList = "[(out1, 1); (out2, 1)]"}
        DragBox=dragBoxInit
        DragWire=dragWireInit

    }, Cmd.map Wire cmds