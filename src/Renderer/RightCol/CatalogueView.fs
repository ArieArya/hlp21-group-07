module CatalogueView
open Fable.React
open Fable.React.Props
open Browser
open Elmish
open Elmish.React

open Model
open Helpers
open type CommonTypes.ComponentType


let viewCatalogue (model:Model) (dispatch:Dispatch<Msg>) =
        div [ Style [Height "100%"; Width "100%"; TextAlign TextAlignOptions.Center; PaddingBottom "5vh"]]
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
                      ] [str "Catalogue"]
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
                              OnClick (fun _ -> dispatch (CreateSymbol (CommonTypes.ComponentType.Not, 60., 60.)))
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
                              OnClick (fun _ -> dispatch (CreateSymbol (CommonTypes.ComponentType.Or, 60., 60.)))
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
                              OnClick (fun _ -> dispatch (CreateSymbol (CommonTypes.ComponentType.And, 60., 60.)))
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
                              OnClick (fun _ -> dispatch (CreateSymbol (CommonTypes.ComponentType.Nor, 60., 60.)))
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
                          OnClick (fun _ -> dispatch (CreateSymbol (CommonTypes.ComponentType.Xor, 60., 60.)))
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
                              OnClick (fun _ -> dispatch (CreateSymbol (CommonTypes.ComponentType.Nand, 60., 60.)))
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
                              OnClick (fun _ -> dispatch (CreateSymbol (CommonTypes.ComponentType.Xnor, 60., 60.)))
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
                              OnClick (fun _ -> dispatch (CreateSymbol (CommonTypes.ComponentType.Decode4, 180., 100.)))
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
                              OnClick (fun _ -> dispatch (CreateSymbol (CommonTypes.ComponentType.Mux2, 60., 60.)))
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
                              OnClick (fun _ -> dispatch (CreateSymbol (CommonTypes.ComponentType.Demux2, 60., 60.)))
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
                              OnClick (fun _ -> dispatch (CreateSymbol (CommonTypes.ComponentType.DFF, 100., 60.)))
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
                              OnClick (fun _ -> dispatch (CreateSymbol (CommonTypes.ComponentType.DFFE, 100., 60.)))
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
                          OnClick (fun _ -> dispatch (CreateSymbol (CommonTypes.ComponentType.Input(model.ComponentInfo.InputWidth), 20., 60.)))
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
                              OnClick (fun _ -> dispatch (CreateSymbol (CommonTypes.ComponentType.Output(model.ComponentInfo.OutputWidth), 20., 60.)))
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
                              OnClick (fun _ -> dispatch (CreateSymbol (CommonTypes.ComponentType.BusSelection(model.ComponentInfo.BusSelectionOutWidth, model.ComponentInfo.BusSelectionLSB), 20., 60.)))
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
                              OnClick (fun _ -> dispatch (CreateSymbol (CommonTypes.ComponentType.Constant(model.ComponentInfo.ConstantWidth, model.ComponentInfo.ConstantValue), 20., 60.)))
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
                                      dispatch (CreateSymbol (CommonTypes.ComponentType.AsyncROM(curMemory), 140., 100.)))
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
                                      dispatch (CreateSymbol (CommonTypes.ComponentType.ROM(curMemory), 140., 100.)))
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
                                      dispatch (CreateSymbol (CommonTypes.ComponentType.RAM(curMemory), 140., 140.)))
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
                              OnClick (fun _ -> dispatch (CreateSymbol (CommonTypes.ComponentType.NbitsAdder(model.ComponentInfo.AdderBits), 140., 100.)))
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
                              OnClick (fun _ -> dispatch (CreateSymbol (CommonTypes.ComponentType.MergeWires, 100., 100.)))
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
                              OnClick (fun _ -> dispatch (CreateSymbol (CommonTypes.ComponentType.SplitWire(model.ComponentInfo.SplitOutWidth), 100., 100.)))
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
                              OnClick (fun _ -> dispatch (CreateSymbol (CommonTypes.ComponentType.Register(model.ComponentInfo.RegWidth), 140., 100.)))
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
                              OnClick (fun _ -> dispatch (CreateSymbol (CommonTypes.ComponentType.RegisterE(model.ComponentInfo.RegEnabledWidth), 140., 100.)))
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
                              OnClick (fun _ -> dispatch (CreateSymbol (CommonTypes.ComponentType.IOLabel, 20., 60.)))
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
                                          | true -> (float custComp.InputLabels.Length) * 40. + 20.
                                          | false -> (float custComp.OutputLabels.Length) * 40. + 20.

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
                
                //Error Highlighting
                
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
                              OnClick (fun _ -> dispatch (ErrorHighlight))
                          ][str "ErrorHighlight"]
                      ]     
                
                div [ Style [Width "100%"; Float FloatOptions.Left; PaddingTop "3vh"]]  [
                    div [ Style [PaddingTop "0.5vh"]][
                      text [ 
                          Style [
                              TextAnchor "middle" 
                              DominantBaseline "middle" 
                              FontSize "1.5vh"
                              FontWeight "Bold"
                              Fill "Gray" 
                          ]
                      ] [str "Ctrl+C -> Copy"]
                    ]
                    div [ Style [PaddingTop "0.5vh"]][
                      text [ 
                          Style [
                              TextAnchor "middle" 
                              DominantBaseline "middle" 
                              FontSize "1.5vh"
                              FontWeight "Bold"
                              Fill "Gray" 
                          ]
                      ] [str "Ctrl+V -> Paste"]
                    ]
                    div [ Style [PaddingTop "0.5vh"]][
                      text [ 
                          Style [
                              TextAnchor "middle" 
                              DominantBaseline "middle" 
                              FontSize "1.5vh"
                              FontWeight "Bold"
                              Fill "Gray" 
                          ]
                      ] [str "Ctrl+A -> Select All"]
                    ]
                    div [ Style [PaddingTop "0.5vh"]][
                      text [ 
                          Style [
                              TextAnchor "middle" 
                              DominantBaseline "middle" 
                              FontSize "1.5vh"
                              FontWeight "Bold"
                              Fill "Gray" 
                          ]
                      ] [str "Ctrl+Z -> Undo"]
                    ]
                    div [ Style [PaddingTop "0.5vh"]][
                      text [ 
                          Style [
                              TextAnchor "middle" 
                              DominantBaseline "middle" 
                              FontSize "1.5vh"
                              FontWeight "Bold"
                              Fill "Gray" 
                          ]
                      ] [str "Ctrl+Y -> Redo"]
                    ]
                ]
            ]
        //]