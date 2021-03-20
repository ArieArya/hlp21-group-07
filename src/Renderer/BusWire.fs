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
    TargetPort: CommonTypes.Port // Output Port
    Points: XYPos list 
    DidUserModifyPoint: bool list // List corresponding to line points. A value of true suggests the user has modified that point
    IsSelected: bool
    SegmentSelected: (XYPos*XYPos) option // Stores which line segment has been selected
    Width: int 
    ShowLegend: bool
    IsCopied: bool
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
    | DeleteWire of (CommonTypes.ConnectionId)
    | ToggleLegend of (CommonTypes.ConnectionId)
    | SetColor of CommonTypes.HighLightColor
    | MouseMsg of MouseT
    | DeleteWiresBySymbol
    | SelectWiresFromSymbol
    | CopyWires
    | PasteWires



//------------------------------------------------------------------------//
//---------------------------Helper Functions-----------------------------//
//------------------------------------------------------------------------//

/// Adds two XYPos components
let posAdd a b =
    {X=a.X+b.X; Y=a.Y+b.Y}

//------------------------------------------------------------------------//
//-------------------------View Function for BusWire----------------------//
//------------------------------------------------------------------------//

/// Props information fore rendering wires
type WireRenderProps = {
    key : CommonTypes.ConnectionId
    WireP: Wire
    Width: int
    Points: XYPos list 
    ColorP: string
    ShowLegend: bool 
    }

type LineRenderProps = {
    SrcP: XYPos 
    TgtP: XYPos
    ColorP: string
    StrokeWidthP: string 
    }

let makeSVGLine color (startP, endP) = 
    line [
        X1 startP.X
        Y1 startP.Y
        X2 endP.X
        Y2 endP.Y
        SVGAttr.Stroke color
        SVGAttr.StrokeWidth (sprintf "2px")  ] []

let makeText (textIn: string) (pos: XYPos) (size: string) (col: string) = 
    text [ 
            X (pos.X); 
            Y (pos.Y); 
            Style [
                TextAnchor "middle" // horizontal algnment vs (X,Y)
                DominantBaseline "middle" // vertical alignment vs (X,Y)
                FontSize size
                FontWeight "Bold"
                Fill col // font color
                UserSelect UserSelectOptions.None
                ]
            ] [str textIn]

let makeWireAnnotation (wirePoints: XYPos list) (width: int) col = // Create width annotation
    let srcP = wirePoints.[0]
    let firstP = wirePoints.[1]
    let xPos = srcP.X + (firstP.X-srcP.X)/2.
    let widthText = makeText (sprintf "%i" width) {X=xPos; Y=srcP.Y-20.} "15px" col
    let widthLine = makeSVGLine col ({ X =xPos-1.; Y=srcP.Y-10.}, {X =xPos+1.; Y=srcP.Y+10.})
    [widthText; widthLine]

let renderWire = // Return wire svg
    FunctionComponent.Of(
        fun (props: WireRenderProps) ->
            let wireAnnotation = 
                if props.ShowLegend 
                then makeWireAnnotation (props.Points) (props.Width) (props.ColorP)
                else []
            props.Points 
            |> List.pairwise //now we have points as pairs
            |> List.map (makeSVGLine (props.ColorP))
            |> fun svgline -> g [] (List.append svgline wireAnnotation)    
            )   

let requiresThreeLines srcPortPos tgtPortPos = // helper to determine whether target port is left of source port
    srcPortPos.X <= tgtPortPos.X 

let inbetween a b c = // check if a is between b and c
    if b > c 
    then (a<b)&&(a>c)
    else (a>b)&&(a<c)

let selectSide oldVal srcVal tgtVal = // helper to ensure wire does not go further than src or tgt port position
    if oldVal >= tgtVal 
    then tgtVal 
    else srcVal 

let generateThreeLines srcPortPos tgtPortPos (oldPoints: XYPos list) (didUserModifyPoint: bool list) = //generates 4 points for wire based on new src and tgt points and old user modification information
    let horizontalDifference = tgtPortPos.X - srcPortPos.X
    let middleLineX = 
        if (didUserModifyPoint.[1] && didUserModifyPoint.[2]) //Check whether user modified the points of vertical line
        then 
            if inbetween (oldPoints.[1].X) (srcPortPos.X) (tgtPortPos.X)
            then (oldPoints.[1].X)
            else selectSide (oldPoints.[1].X) (srcPortPos.X) (tgtPortPos.X)       
        else 
            srcPortPos.X+horizontalDifference/2.
    [srcPortPos; {X=middleLineX; Y=srcPortPos.Y}; {X=middleLineX; Y=tgtPortPos.Y}; tgtPortPos]
            
let generateFiveLines srcPortPos tgtPortPos (oldPoints: XYPos list) (didUserModifyPoint: bool list) = //Same as three lines
    let firstVerticalLineX = 
        if didUserModifyPoint.[1] && didUserModifyPoint.[2]
        then 
            if oldPoints.[1].X > srcPortPos.X
            then oldPoints.[1].X 
            else srcPortPos.X
        else srcPortPos.X+100.

    let horizontalLine = 
        if didUserModifyPoint.[2] && didUserModifyPoint.[3]
        then oldPoints.[2].Y 
        else (srcPortPos.Y+tgtPortPos.Y)/2.

    let secondVerticalLineX = 
        if didUserModifyPoint.[3] && didUserModifyPoint.[4]
        then 
            if oldPoints.[4].X < tgtPortPos.X
            then oldPoints.[4].X 
            else tgtPortPos.X
        else tgtPortPos.X-100.

    [srcPortPos; {X=firstVerticalLineX; Y=srcPortPos.Y}; {X=firstVerticalLineX;Y=horizontalLine}; {X=secondVerticalLineX; Y=horizontalLine}; {X=secondVerticalLineX; Y=tgtPortPos.Y}; tgtPortPos]

let generateFalses count : bool list = //Helper to create initial DidUserModifyPoint
    List.replicate count false 

let getInitialWirePoints (srcPortPos: XYPos) (tgtPortPos: XYPos) : XYPos list= 
    let initialDidUser = generateFalses 6
    if requiresThreeLines srcPortPos tgtPortPos 
    then generateThreeLines srcPortPos tgtPortPos [] initialDidUser
    else generateFiveLines srcPortPos tgtPortPos [] initialDidUser

let createNewPoints srcPortPos tgtPortPos oldPoints didUserModifyPoint = 
    let oldSrcPortPos = List.head oldPoints
    let oldTgtPortPos = List.last oldPoints 
    match (requiresThreeLines oldSrcPortPos oldTgtPortPos, requiresThreeLines srcPortPos tgtPortPos) with 
    | (false, true) | (true, false) -> getInitialWirePoints srcPortPos tgtPortPos 
    | (true, true) -> generateThreeLines srcPortPos tgtPortPos oldPoints didUserModifyPoint//keeping 3 lines
    | (false, false) -> generateFiveLines srcPortPos tgtPortPos oldPoints didUserModifyPoint

let getDidUser newPoints oldPoints oldDidUserModify = //work out if algorithm had to change user set points
    if (List.length newPoints <> List.length oldPoints)
    then generateFalses (List.length newPoints)
    else oldDidUserModify

let updateWire (symbolModel: Symbol.Model) (wire: Wire) : Wire = 
    // get new source port pos
    let srcPortPos = Symbol.symbolPortPos (symbolModel) (wire.SrcPort.Id)
    let tgtPortPos = Symbol.symbolPortPos (symbolModel) (wire.TargetPort.Id)
    let newPoints = createNewPoints srcPortPos tgtPortPos (wire.Points) (wire.DidUserModifyPoint)// changeInandOut (wire.Points) srcPortPos tgtPortPos
    let didUser = getDidUser newPoints (wire.Points) (wire.DidUserModifyPoint)
    {wire with Points= newPoints; DidUserModifyPoint=didUser}

let view (model:Model) (dispatch: Dispatch<Msg>)=    
    let wireModel : Wire list= 
        List.map (updateWire (model.Symbol)) (model.WX) 
    let wires = 
        wireModel
        |> List.map (fun w ->
            let col = if (w.IsSelected) then CommonTypes.CustomColorLightBlue.Text() else model.Color.Text()
            let props = {
                key = w.Id
                WireP = w
                Points = w.Points
                ColorP = col
                Width= w.Width 
                ShowLegend = w.ShowLegend}
            renderWire props)
    let symbols = Symbol.view model.Symbol (fun sMsg -> dispatch (Symbol sMsg)) 
    g [] [symbols; (g [] wires); ]


let init n () =
    let symbols, cmd = Symbol.init()
    let symIds = List.map (fun (sym:Symbol.Symbol) -> sym.Id) symbols
    let rng = System.Random 0
    []
    |> (fun wires -> {WX=wires;Symbol=symbols; Color=CommonTypes.CustomColorDarkBlue},Cmd.none)

let makeWireFromPorts (srcPort) (targetPort) (points: XYPos list) (width: int) : Wire=
        {
            Id=CommonTypes.ConnectionId (uuid())
            SrcPort = srcPort
            TargetPort = targetPort
            Points = points
            DidUserModifyPoint = generateFalses (List.length points)
            SegmentSelected = None
            IsSelected = false
            Width = width
            ShowLegend = true
            IsCopied = false
        }

let wasMouseBetweenPoints mousePos points = 
    match points with 
    | (first, second) when first.X = second.X -> (inbetween (mousePos.Y) (first.Y) (second.Y)) && (abs (mousePos.X-first.X) <20.)
    | (first, second) when first.Y = second.Y -> (inbetween (mousePos.X) (first.X) (second.X)) && (abs (mousePos.Y-first.Y) <20.)
    | _ -> false

let wasWireClicked (mousePos: XYPos) (wire: Wire) : bool= 
    wire.Points 
    |> List.pairwise 
    |> List.exists (wasMouseBetweenPoints mousePos)

let whichWireClicked wires (mousePos: XYPos) : Wire option = 
    List.tryFind (wasWireClicked mousePos) wires

let unselectAllWires wireModel = 
    wireModel 
    |> List.map (fun w -> {w with IsSelected=false; SegmentSelected=None})

let selectWire wire wireModel = 
    wireModel 
    |> unselectAllWires 
    |> List.map (fun w -> if w.Id=wire.Id then {w with IsSelected=true} else w)

let getSegmentClicked (wire: Wire) (mousePos: XYPos) : (XYPos *XYPos) option = 
    wire.Points 
    |> List.pairwise 
    |> List.tryFind (wasMouseBetweenPoints mousePos)

let addSegment (wire: Wire) (segment : (XYPos *XYPos) option) wireModel = 
    wireModel 
    |> List.map (fun w -> if w.Id=wire.Id then {w with SegmentSelected=segment} else w)

let wireSelector (fullModel: Model) (mousePos: XYPos) = 
    let wireClicked = whichWireClicked (fullModel.WX) mousePos   
    let wireModel = 
        match wireClicked with 
        | Some w -> 
            let segmentClicked = getSegmentClicked w mousePos
            fullModel.WX 
            |> selectWire w 
            |> addSegment w segmentClicked
        | None -> unselectAllWires (fullModel.WX)
    {fullModel with WX=wireModel}

let generateDidUserModify newPoints oldPoints oldTruths =   
    List.map3 
        (fun newP oldP truth -> 
            if truth then truth else 
                (if newP=oldP then false else true)) newPoints oldPoints oldTruths

let moveSegmentToMousePos p1 p2 mousePos = 
    match p1,p2 with 
    | _ when p1.X=p2.X -> {p1 with X=mousePos.X}, {p2 with X=mousePos.X}
    | _ when p1.Y=p2.Y -> {p1 with Y=mousePos.Y}, {p2 with Y=mousePos.Y}
    | _ -> failwithf "Not a horizontal nor vertical line"

let moveWirePoints (wire: Wire) (mousePos: XYPos) : Wire= 
    match (wire.SegmentSelected) with 
    | Some (p1, p2) -> 
        let newP1, newP2 = moveSegmentToMousePos p1 p2 mousePos
        let newPoints = 
            wire.Points 
            |> List.map 
                (function
                    | p when p=p1 -> newP1
                    | p when p=p2 -> newP2
                    | p -> p )
        {wire with Points=newPoints; SegmentSelected=Some (newP1, newP2)}
    | None -> failwithf "Trying to move a wire without a segment selected"
   
let moveWire (mousePos: XYPos) (wire: Wire) : Wire = 
    if wire.IsSelected 
    then        
        let newWire = moveWirePoints wire mousePos
        let didUserModify = generateDidUserModify (newWire.Points) (wire.Points) (wire.DidUserModifyPoint)
        {newWire with DidUserModifyPoint=didUserModify}
    else wire 

let moveWires (wireModel: Wire list) (mousePos: XYPos) = 
    wireModel
    |> List.map (moveWire mousePos)

let handleMouseForWires (model: Model) mMsg : Model = 
    let wireModel : Wire list= 
        List.map (updateWire (model.Symbol)) (model.WX)
 
    let finalModel = {model with WX=wireModel}

    match mMsg.Op with 
    | Down -> wireSelector finalModel (mMsg.Pos)
    | Drag -> {model with WX=moveWires (finalModel.WX) (mMsg.Pos)}
    | _ -> model

let update (msg : Msg) (model : Model): Model*Cmd<Msg> =
    match msg with
    | Symbol sMsg -> 
        let sm,sCmd = Symbol.update sMsg model.Symbol
        {model with Symbol=sm}, Cmd.map Symbol sCmd

    | AddWire (port1, port2) -> 
        //find the input and output ports from both ports
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


        // perform width inference
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
            | Some a, Some b ->
                let srcPortPos = srcPort.Pos
                let targetPortPos = targetPort.Pos
                let wirePoints = getInitialWirePoints srcPortPos targetPortPos
                let newWire = makeWireFromPorts (srcPort) (targetPort) wirePoints (a)
                {model with WX=List.append model.WX [newWire]}, Cmd.none

            | Some a, None ->
                let newSymModel, isValid = Symbol.portInference model.Symbol srcPort a
                if isValid then
                    let newSrcPort = {srcPort with Width = Some a}
                    let srcPortPos = newSrcPort.Pos
                    let targetPortPos = targetPort.Pos
                    let wirePoints = getInitialWirePoints srcPortPos targetPortPos
                    let newWire = makeWireFromPorts (newSrcPort) (targetPort) wirePoints (a)
                    {model with WX=List.append model.WX [newWire]; Symbol = newSymModel}, Cmd.none
                else 
                    model, Cmd.none

            | None, Some a ->
                let newSymModel, isValid = Symbol.portInference model.Symbol targetPort a
                if isValid then
                    let newTargetPort = {targetPort with Width = Some a}
                    let srcPortPos = srcPort.Pos
                    let targetPortPos = newTargetPort.Pos
                    let wirePoints = getInitialWirePoints srcPortPos targetPortPos
                    let newWire = makeWireFromPorts (srcPort) (newTargetPort) wirePoints (a)
                    {model with WX=List.append model.WX [newWire]; Symbol = newSymModel}, Cmd.none
                else 
                    model, Cmd.none

            | None, None ->
                model, Cmd.none
                

    | DeleteWire (conId) -> 
        // filter out wire
        let wireRemoved = List.filter (fun w -> w.Id <> conId) (model.WX)
        {model with WX=wireRemoved}, Cmd.none

    | SetColor c -> {model with Color = c}, Cmd.none

    | ToggleLegend (conId) -> 
        let newWireModel = 
            (model.WX) 
            |> List.map (fun wire -> 
                if (wire.Id=conId) 
                then {wire with ShowLegend = not (wire.ShowLegend)}
                else wire)
        {model with WX=newWireModel}, Cmd.none

    | DeleteWiresBySymbol ->
        let remSelectedWires = List.filter (fun wire -> not wire.IsSelected) model.WX
        let newWX = List.filter (fun wire -> not (Symbol.isSymbolSelected model.Symbol wire.SrcPort.HostId || Symbol.isSymbolSelected model.Symbol wire.TargetPort.HostId)) remSelectedWires
        
        let rec getSymbolModel (wireList: Wire list) (symModel) = 
            match wireList with 
            | [] -> symModel
            | (hdWire::tl) ->
                let srcPort = hdWire.SrcPort
                let targetPort = hdWire.TargetPort
                let updatedSrcSymbol = Symbol.variablePortReset (symModel) srcPort
                let updatedTargetSymbol = Symbol.variablePortReset (updatedSrcSymbol) targetPort

                getSymbolModel tl updatedTargetSymbol
        
        // list of all deleted wires
        let removedWX = List.filter (fun wire -> Symbol.isSymbolSelected model.Symbol wire.SrcPort.HostId || Symbol.isSymbolSelected model.Symbol wire.TargetPort.HostId || wire.IsSelected) model.WX
        
        // reset symbols with variable port widths
        let modifiedSymbol = getSymbolModel removedWX model.Symbol

        let newSymbolModel, _ = Symbol.update (Symbol.Msg.DeleteSymbol) modifiedSymbol
        {model with WX = newWX; Symbol=newSymbolModel}, Cmd.none

    | MouseMsg mMsg -> 
        handleMouseForWires model mMsg, Cmd.ofMsg (Symbol (Symbol.MouseMsg mMsg))

    | SelectWiresFromSymbol ->
        let newWX = 
            model.WX
            |> List.map (fun wire -> 
                            let isSrcPortHostSelected = Symbol.isSymbolSelected (model.Symbol) (wire.SrcPort.HostId)
                            let isTargetPortHostSelected = Symbol.isSymbolSelected (model.Symbol) (wire.TargetPort.HostId)

                            if isSrcPortHostSelected && isTargetPortHostSelected then {wire with IsSelected = true}
                            else wire)
        
        {model with WX=newWX}, Cmd.none


    | CopyWires -> 
        let newSymbols, _ = Symbol.update (Symbol.Msg.CopySymbols) model.Symbol

        let newWX = 
            model.WX 
            |> List.map (fun wire ->
                            if wire.IsSelected then {wire with IsCopied = true}
                            else {wire with IsCopied = false}
                            )

        {model with WX=newWX; Symbol=newSymbols}, Cmd.none


    | PasteWires ->
        let pasteMargin = {X=80.; Y=80.}

        let newSymbols, _ = Symbol.update (Symbol.Msg.PasteSymbols pasteMargin) model.Symbol 

        let rec getWireSymbolModel (wireList: Wire list) (finalSymModel) (finalWireModel: Wire list) = 
            match wireList with 
            | [] -> finalSymModel, finalWireModel
            | (hdWire::tl) ->
                    match hdWire.IsCopied with 
                    | false -> getWireSymbolModel tl finalSymModel (hdWire::finalWireModel)
                    | true ->
                        let newSrcPos = posAdd hdWire.SrcPort.Pos pasteMargin
                        let newTargetPos = posAdd hdWire.TargetPort.Pos pasteMargin

                        let newSrcPort = Symbol.findPortByPosition newSymbols newSrcPos
                        let newTargetPort = Symbol.findPortByPosition newSymbols newTargetPos

                        match newSrcPort, newTargetPort with 
                        | Some srcPort, Some targetPort -> 
                            match targetPort.Width, srcPort.Width with
                            | Some a, Some b when a = b ->
                                let srcPortPos = srcPort.Pos
                                let targetPortPos = targetPort.Pos
                                let wirePoints = getInitialWirePoints srcPortPos targetPortPos
                                let newWire = makeWireFromPorts (srcPort) (targetPort) wirePoints (a)
                                getWireSymbolModel tl finalSymModel (hdWire::(newWire::finalWireModel))

                            | Some a, None ->
                                let newSymModel, isValid = Symbol.portInference finalSymModel srcPort a
                                if isValid then
                                    let newSrcPort = {srcPort with Width = Some a}
                                    let srcPortPos = newSrcPort.Pos
                                    let targetPortPos = targetPort.Pos
                                    let wirePoints = getInitialWirePoints srcPortPos targetPortPos
                                    let newWire = makeWireFromPorts (newSrcPort) (targetPort) wirePoints (a)
                                    getWireSymbolModel tl newSymModel (hdWire::(newWire::finalWireModel))
                                    
                                else 
                                    getWireSymbolModel tl finalSymModel (hdWire::finalWireModel)

                            | None, Some a ->
                                let newSymModel, isValid = Symbol.portInference finalSymModel targetPort a
                                if isValid then
                                    let newTargetPort = {targetPort with Width = Some a}
                                    let srcPortPos = srcPort.Pos
                                    let targetPortPos = newTargetPort.Pos
                                    let wirePoints = getInitialWirePoints srcPortPos targetPortPos
                                    let newWire = makeWireFromPorts (srcPort) (newTargetPort) wirePoints (a)
                                    getWireSymbolModel tl newSymModel (hdWire::(newWire::finalWireModel))
                                else 
                                    getWireSymbolModel tl finalSymModel (hdWire::finalWireModel)

                            | _ ->
                                getWireSymbolModel tl finalSymModel (hdWire::finalWireModel)
                                

                        | _ -> getWireSymbolModel tl finalSymModel (hdWire::finalWireModel)

        
        let newSymModel, newWX = getWireSymbolModel (model.WX) newSymbols []

        {model with WX=newWX; Symbol=newSymModel}, Cmd.none




//------------------------------------------------------------------------//
//-------------------------Other interface functions----------------------//
//------------------------------------------------------------------------//

let isAnyWireHovered (wModel: Model) (pos: XYPos) : bool = 
    let findWire = 
        wModel.WX
        |> List.tryFind (fun wire -> wasWireClicked pos wire)
    
    match findWire with 
    | None -> false
    | Some _ -> true

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