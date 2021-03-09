module BusWire
open Fable.React
open Fable.React.Props
open Browser
open Elmish
open Elmish.React
open Helpers

//------------------------------------------------------------------------//
//------------------------------BusWire Types-----------------------------//
//------------------------------------------------------------------------//

type Wire = {
    Id: CommonTypes.ConnectionId 
    SrcPort: CommonTypes.Port // Input Port
    TargetPort: CommonTypes.Port // Source Port
    IsDragging: bool
    DraggingPort: CommonTypes.PortType // Input for Src, Output for Target
    }

type Model = {
    Symbol: Symbol.Model
    WX: Wire list
    Color: CommonTypes.HighLightColor
    }


//------------------------------------------------------------------------//
//---------------------------Message Type---------------------------------//
//------------------------------------------------------------------------//

type Msg =
    | Symbol of Symbol.Msg
    | AddWire of (CommonTypes.Port * CommonTypes.Port)
    | SetColor of CommonTypes.HighLightColor
    | DeleteWire of CommonTypes.ConnectionId
    | DeleteWiresBySymbol 
    | MouseMsg of MouseT



//------------------------------------------------------------------------//
//---------------------------Helper Functions-----------------------------//
//------------------------------------------------------------------------//

/// Converts list of coordinates to a string for SVG Polygon or Polyline. For example,
/// [(1, 2); (3, 4); (5, 6)] becomes "1,2 3,4 5,6", which is the correct SVG format
let convertPoly inpList = 
    string(inpList)
    |> String.collect (fun c -> if c = '(' || c = ')' || c = '[' || c = ']' then ""
                                elif c = ';' then " "
                                elif c = ' ' then ""
                                else string c)



//------------------------------------------------------------------------//
//-------------------------View Function for BusWire----------------------//
//------------------------------------------------------------------------//

/// Props information fore rendering wires
type WireRenderProps = {
    key : CommonTypes.ConnectionId
    WireP: Wire
    SrcP: CommonTypes.Port
    TgtP: CommonTypes.Port
    ColorP: string
    StrokeWidthP: string 
}

/// Renders a single wire between two ports
let singleWireView = 
    FunctionComponent.Of(
        fun (props: WireRenderProps) ->
            // starting wire segment
            let startingPos = 
                (props.TgtP.Pos.X, props.TgtP.Pos.Y)
            
            // ending wire segment
            let endingPos =
                (props.SrcP.Pos.X, props.SrcP.Pos.Y)

            // intermediate wire segment
            let intermediatePos1 = 
                match props.SrcP.Pos.X - props.TgtP.Pos.X with
                | a when a >= 40. -> (props.TgtP.Pos.X + a/2., props.TgtP.Pos.Y)
                | _ -> (props.TgtP.Pos.X + 40., props.TgtP.Pos.Y)

            // intermediate wire segment
            let intermediatePos4 = 
                match props.SrcP.Pos.X - props.TgtP.Pos.X with
                | a when a >= 40. -> (props.TgtP.Pos.X + a/2., props.SrcP.Pos.Y)
                | _ -> (props.SrcP.Pos.X - 40., props.SrcP.Pos.Y)

            // intermediate wire segment
            let intermediatePos2 = 
                let curY = (snd startingPos + snd endingPos)/2.
                let curX = fst intermediatePos1
                (curX, curY)

            // intermediate wire segment
            let intermediatePos3 = 
                let curY = (snd startingPos + snd endingPos)/2.
                let curX = fst intermediatePos4
                (curX, curY)

            // find the width of the wire
            let wireWidth = 
                props.SrcP.Width

            // converts list of coordinates to correct string format for SVG
            let displayString = 
                convertPoly ([startingPos] @ [intermediatePos1] @ [intermediatePos2] @ [intermediatePos3] @ [intermediatePos4] @ [endingPos])

            [
                // display the wire using polyline
                polyline [
                    Points displayString

                    SVGAttr.Fill "none"
                    SVGAttr.Stroke props.ColorP
                    SVGAttr.StrokeWidth props.StrokeWidthP ] []

                // displays the width of the wire
                text [
                    X ((fst startingPos) + 7.)
                    Y ((snd startingPos) - 7.)
                    Style [
                        FontSize "14px"
                        FontWeight "Bold"
                        UserSelect UserSelectOptions.None
                    ]
                ][str (string wireWidth)]
                                 
            ]
            |> ofList
        )

/// View function for BusWire layer of SVG
let view (model:Model) (dispatch: Dispatch<Msg>)=
    // displays wires in the canvas
    let wires = 
        model.WX
        |> List.map (fun w ->
            // if wire is dragging then color is grey, otherwise it is black
            let color = 
                if w.IsDragging then "grey" else "black"
            
            // if wire is dragging, then its displayed width is 1px, otherwise 2px
            let strokeWidth = 
                if w.IsDragging then "1px" else "2px"

            // define props for renderring wires
            let props = {
                key = w.Id
                WireP = w
                SrcP = w.SrcPort 
                TgtP = w.TargetPort 
                ColorP = color
                StrokeWidthP = strokeWidth }

            singleWireView props)

    // obtains symbols
    let symbols = Symbol.view model.Symbol (fun sMsg -> dispatch (Symbol sMsg))
        
    g [] [(g [] wires); symbols]


/// Initialization
let init n () =
    let symbols, cmd = Symbol.init()
    
    {WX=[]; Symbol=symbols; Color=CommonTypes.Red},Cmd.none

/// Update function to update BusWire models
let update (msg : Msg) (model : Model): Model*Cmd<Msg> =
    match msg with
    // updates symbol through BusWire
    | Symbol sMsg -> 
        let sm,sCmd = Symbol.update sMsg model.Symbol
        {model with Symbol=sm}, Cmd.map Symbol sCmd

    // add new wires given two ports
    | AddWire (port1, port2) -> 
        // find the input and output ports from both ports
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

        // if no port combinations possible, return the original model, otherwise
        // add new wire between the ports to the new model
        match matchPorts with
        | None -> model, Cmd.none
        | Some (inputPort, outputPort) ->
            let wire = {
                    Id = CommonTypes.ConnectionId (uuid())
                    SrcPort = inputPort
                    TargetPort = outputPort
                    IsDragging = false
                    DraggingPort = CommonTypes.PortType.Input
                }
            let newWires = wire::model.WX
            {model with WX=newWires}, Cmd.none

    // sets color of the wires
    | SetColor c -> {model with Color = c}, Cmd.none

    // deletes wire in the model
    | DeleteWire wireId ->
        let newWX = List.filter (fun wire -> wire.Id <> wireId) model.WX
        {model with WX = newWX}, Cmd.none

    // deletes all wires connected to all selected symbols
    | DeleteWiresBySymbol ->
        let newWX = List.filter (fun wire -> not (Symbol.isSymbolSelected model.Symbol wire.SrcPort.HostId || Symbol.isSymbolSelected model.Symbol wire.TargetPort.HostId)) model.WX
        let newSymbolModel, _ = Symbol.update (Symbol.Msg.DeleteSymbol) model.Symbol
        {model with WX = newWX; Symbol=newSymbolModel}, Cmd.none

    // handles mouse messages
    | MouseMsg mMsg -> 
        model, Cmd.none



//------------------------------------------------------------------------//
//-------------------------Other interface functions----------------------//
//------------------------------------------------------------------------//

// finds any wire that is being dragged (to be deleted in Sheets)
let findSelectedWire (wModel: Model) : CommonTypes.ConnectionId option = 
    let selectedWire = List.tryFind (fun wire -> wire.IsDragging) wModel.WX
    match selectedWire with
    | None -> None
    | Some wire -> Some wire.Id



//------------------------------------------------------------------------//
//---------------------------interface to Issie---------------------------//
//------------------------------------------------------------------------//

/// interfaces for Issie currently not used
let extractWire (wModel: Model) (sId:CommonTypes.ComponentId) : CommonTypes.Component= 
    failwithf "Not implemented"
let extractWires (wModel: Model) : CommonTypes.Component list = 
    failwithf "Not implemented"
let updateSymbolModelWithComponent (symModel: Model) (comp:CommonTypes.Component) =
    failwithf "Not Implemented"



    



