module PropertiesView
open Fulma
open Fable.React
open Fable.React.Props
open Browser
open Elmish
open Elmish.React

open Symbol
open Model
open Helpers
open type CommonTypes.ComponentType

//Using Some Descriptions from Issie
let details (comp:CommonTypes.Component) =
    match comp.Type with
        | Input _ -> str "Input"
        | Constant _ -> str "Constant Wire"
        | Output _ -> str "Output"
        | BusSelection _ -> div [] [
                    str "Bus Selection"
                    br []
                    str "The output is the subrange [width+lsb-1..lsb] of the input bits. If width = 1 this selects one bit. Error if the input has less than width + lsb bits"
            ]
        | IOLabel -> div [] [
            str "Label on Wire or Bus. Each label has input on left and output on right. \
                No output connection is required from a set of labels."]
        | Not | And | Or | Xor | Nand | Nor | Xnor ->
            div [] [ str <| sprintf "%A gate" comp.Type ]
        | Mux2 -> div [] [ str "Multiplexer with two inputs and one output" ]
        | Demux2 -> div [] [ str "Demultiplexer with one input and two outputs" ]
        | MergeWires -> div [] [ str "Merge two wires of width n and m into a single wire of width n+m" ]
        | SplitWire _ -> div [] [ str "Split a wire of width n+m into two wires of width n and m"]
        | NbitsAdder numberOfBits -> div [] [ str <| sprintf "%d bit(s) adder" numberOfBits ]
        | Decode4 -> div [] [ str <| "4 bit decoder: Data is output on the Sel output, all other outputs are 0"]
        | Custom custom ->
            let toHTMLList =
                List.map (fun (label, width) -> li [] [str <| sprintf "%s: %d bit(s)" label width])
            div [] [
                str <| sprintf "%s: user defined component" custom.Name
                br []
                span [Style [FontStyle "italic"]] [str <| "Inputs"]
                ul [] (toHTMLList custom.InputLabels)
                span [Style [FontStyle "italic"]] [str <| "Outputs"]
                ul [] (toHTMLList custom.OutputLabels)
            ]
        | DFF -> div [] [ str "D-flip-flop. The component is implicitly connected to the global clock" ]
        | DFFE -> div [] [
            str "D-flip-flop with enable. If the enable signal is high the state of
                 the D-flip-flop will be updated at the next clock cycle.
                 The component is implicitly connected to the global clock" ]
        | Register _  -> div [] [ str "Register. The component is implicitly connected to the global clock" ]
        | RegisterE _ ->
            div [] [ str "Register with enable. If the enable signal is high the
                          state of the Register will be updated at the next clock
                          cycle. The component is implicitly connected to the global
                          clock" ]
        | AsyncROM mem ->
            str "Asynchronous ROM: the output is updated as soon as the address changes"
            //makeMemoryInfo descr mem comp.Id model dispatch
        | ROM mem ->
            str "Synchronous ROM: the output is updated only after a clock tick. The component is implicitly connected to the global clock"
            //makeMemoryInfo descr mem comp.Id model dispatch
        | RAM mem ->
            str
                "RAM memory. At every clock tick, the RAM can either read or write
                the content of the memory location selected by the address. If the
                write signal is high, the content of the selected memory location
                is set to the value of data-in. This value will also be propagated
                to data-out immediately. The component is implicitly connected to
                the global clock"
            //makeMemoryInfo descr mem comp.Id model dispatch

let private textFormField (comp:CommonTypes.Component) (isRequired:bool) (model:Model) (dispatch:Dispatch<Msg>) =
    div [ Style [PaddingTop "0.5vh"]][
        input [
            SpellCheck false
            AutoFocus true
            DefaultValue comp.Label  
            Type "text"
            Placeholder (if isRequired then "Name (required)" else "Name (optional)")
            OnChange (fun ev -> dispatch (UpdateComponentLabel(comp, ev.Value)))

            Style [
                Width "50%"
                FontSize "1.2vh"
                Height "2.3vh"
                TextAlign TextAlignOptions.Center
            ]
        ]
    ]

let private makeNumberOfBitsField (comp:CommonTypes.Component) (model:Model) (dispatch:Dispatch<Msg>) =
    let title, width =
        match comp.Type with
        | Input w | Output w | NbitsAdder w | Register w | RegisterE w -> "Number of bits", w 
        | SplitWire w -> "Number of bits in the top (LSB) wire", w
        | BusSelection( w, _) -> "Number of bits selected: width", w
        | Constant(w, _) -> "Number of bits in the wire", w
        | c -> failwithf "makeNumberOfBitsField called with invalid component: %A" c

    div [ Style [PaddingTop "3vh"; Margin "2vh"]][
        text [ 
            Style [
                TextAnchor "middle" 
                DominantBaseline "middle" 
                FontSize "2.3vh"
                FontWeight "Bold"
                Fill "Gray" 
            ]
        ] [str $"{title}"]
        br []
        input [
            DefaultValue width  
            Type "number"
            Placeholder $"{width}"
            OnChange (fun ev -> 
                match comp.Type with
                | Input _ | Output _ | NbitsAdder _ | Register _ | RegisterE _ | SplitWire _
                | BusSelection( _, _) | Constant(_, _) -> 
                    if (int ev.Value < 1) then dispatch (UpdateComponentWidth(comp, 1)) else dispatch (UpdateComponentWidth(comp, int ev.Value))
                | c -> failwithf "makeNumberOfBitsField called with invalid component: %A" c
            )

            Style [
                Width "50%"
                FontSize "1.2vh"
                Height "2.3vh"
                TextAlign TextAlignOptions.Center
            ]
        ]
    ]

let private makeLsbBitNumberField (comp:CommonTypes.Component) (model:Model) (dispatch:Dispatch<Msg>) =
    let lsbPos, infoText =
        match comp.Type with
        | BusSelection(width,lsb) -> uint32 lsb, "Least Significant Bit number selected: lsb"
        | _ -> failwithf "makeLsbBitNumberfield called from %A" comp.Type

    div [ Style [PaddingTop "3vh"; Margin "2vh"]][
        text [ 
            Style [
                TextAnchor "middle" 
                DominantBaseline "middle" 
                FontSize "2.3vh"
                FontWeight "Bold"
                Fill "Gray" 
            ]
        ] [str $"{infoText}"]
        br []
        input [
            DefaultValue lsbPos     
            Type "number"
            Placeholder $"{lsbPos}"
            OnChange (fun ev -> 
                if (int ev.Value < 0) then dispatch (ChangeBusSelectionLSB(0))
                else dispatch (ChangeBusSelectionLSB(int ev.Value)))

            Style [
                Width "50%"
                FontSize "1.2vh"
                Height "2.3vh"
                TextAlign TextAlignOptions.Center
            ]
        ]
    ]

let private makeConstantValueField (comp:CommonTypes.Component) (model:Model) (dispatch:Dispatch<Msg>) =
    let cVal, width =
        match comp.Type with 
        | Constant(width,cVal) -> cVal, width
        | _ -> failwithf "makeConstantValuefield called from %A" comp.Type
    
    div [ Style [PaddingTop "3vh"; Margin "2vh"]][
        text [ 
            Style [
                TextAnchor "middle" 
                DominantBaseline "middle" 
                FontSize "2.3vh"
                FontWeight "Bold"
                Fill "Gray" 
            ]
        ] [str $"Constant Value"]
        br []
        input [   
            DefaultValue cVal  
            Type "number"
            Placeholder $"{cVal}"
            OnChange (fun ev -> 
                if (int ev.Value > 32) then dispatch (ChangeConstantValue(32))
                else dispatch (ChangeConstantValue(int ev.Value)))

            Style [
                Width "50%"
                FontSize "1.2vh"
                Height "2.3vh"
                TextAlign TextAlignOptions.Center
            ]
        ]
    ]

let private makeExtraInfo (comp:CommonTypes.Component) (model:Model) (dispatch:Dispatch<Msg>) =
    match comp.Type with
    | Input _ | Output _ | NbitsAdder _ | SplitWire _ | Register _ ->
        makeNumberOfBitsField comp model dispatch
    | BusSelection _ ->
        div [] [
            makeNumberOfBitsField comp model dispatch
            makeLsbBitNumberField comp model dispatch
            ]
    | Constant _ ->
        div [] [
             makeNumberOfBitsField comp model dispatch
             makeConstantValueField comp model dispatch
             ]
    | _ -> div [] []

let viewProperties (model:Model) (dispatch:Dispatch<Msg>) =
        dispatch (ChangeSelectedComponent (findSelectedSymbol model.Wire.Symbol))
        div [ Style [Height "100%"; Width "100%"; TextAlign TextAlignOptions.Center; PaddingBottom "5vh"]][
            div [ Style [PaddingTop "5vh"]][
                  text [ 
                      Style [
                          TextAnchor "middle" 
                          DominantBaseline "middle" 
                          FontSize "3vh"
                          FontWeight "Bold"
                          Fill "Gray" 
                      ]
                  ] [str "Component Properties"]
            ]
            match model.SelectedComponent with
            | None -> 
                div [ Style [PaddingTop "2vh"; TextAlign TextAlignOptions.Center; FontSize "1.8vh"]] 
                    [str "Select a Symbol to See Details and Change Values"]    
            | Some comp ->
                div [ Style [PaddingTop "3vh"; Margin "2vh"]][
                    text [ 
                        Style [
                            TextAnchor "middle" 
                            DominantBaseline "middle" 
                            FontSize "2.3vh"
                            FontWeight "Bold"
                            Fill "Gray" 
                        ]
                    ] [str "Details:"]
                    text [ 
                        Style [
                              TextAnchor "middle" 
                              DominantBaseline "middle" 
                              FontSize "1.8vh"
                              FontWeight "Normal"
                              Fill "Gray"
                              Padding "0.5vh" 
                        ]
                    ] [details comp]
                ]
                makeExtraInfo comp model dispatch
                let required = match comp.Type with | SplitWire _ | MergeWires | BusSelection _ -> false | _ -> true
                div [ Style [PaddingTop "3vh"; Margin "2vh"]][
                    text [ 
                        Style [
                            TextAnchor "middle" 
                            DominantBaseline "middle" 
                            FontSize "2.3vh"
                            FontWeight "Bold"
                            Fill "Gray" 
                        ]
                    ] [str "Component Name:"]
                    textFormField comp required model dispatch 
                ]
            ]