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
    | MouseMsg of MouseT * bool
    | DeleteWiresBySymbol
    | SelectWiresFromSymbol
    | CopyWires
    | PasteWires
    | SelectAll
    | BoxSelected of XYPos * XYPos * bool
    | SaveModel



//------------------------------------------------------------------------//
//---------------------------Helper Functions-----------------------------//
//------------------------------------------------------------------------//

/// Adds two XYPos components
let posAdd a b =
    {X=a.X+b.X; Y=a.Y+b.Y}

// obtains the string from the component id
let unwrapConnectionId (CommonTypes.ConnectionId x) = x

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

let makeSVGLine color width (startP, endP) = 
    line [
                X1 startP.X
                Y1 startP.Y
                X2 endP.X
                Y2 endP.Y
                SVGAttr.Stroke color
                SVGAttr.StrokeWidth (sprintf "%ipx" width)  ] []

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
    let widthLine = makeSVGLine col 2 ({ X =xPos-1.; Y=srcP.Y-10.}, {X =xPos+1.; Y=srcP.Y+10.})
    [widthText; widthLine]

let getNonLinearWidth width = 
    5-(25/(width+7))

let renderWire = // Return wire svg
    FunctionComponent.Of(
        fun (props: WireRenderProps) ->
            let wireAnnotation = 
                if props.ShowLegend 
                then makeWireAnnotation (props.Points) (props.Width) (props.ColorP)
                else []
            let nonLinearWidth = getNonLinearWidth (props.Width)
            props.Points 
            |> List.pairwise //now we have points as pairs
            |> List.map (makeSVGLine (props.ColorP) nonLinearWidth)
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


let isSymbolOnLineVertical (symbolModel: Symbol.Model) xPos firstY secondY (symbol: Symbol.Symbol) = 
    let boundingBox = Symbol.symbolBoundingBox symbolModel (symbol.Id)
    //let's just write it for vertical lines initially
    xPos> boundingBox.[0].X && xPos < boundingBox.[1].X && 
        ((boundingBox.[0].Y > firstY && boundingBox.[0].Y < secondY ) || (boundingBox.[3].Y > firstY && boundingBox.[3].Y < secondY ))


let boundingBoxNearbyVertical xPos firstY secondY (symbolModel: Symbol.Model) = 
    symbolModel 
    |> List.tryFind (isSymbolOnLineVertical symbolModel xPos firstY secondY)
    |> Option.map (fun sym -> Symbol.symbolBoundingBox symbolModel (sym.Id))


let setupLinePos minimum maximum lowerMax upperMin = 
    printfn "MINS ARE %A %A %A %A" minimum maximum lowerMax upperMin 
    match 1 with 
    | _ when lowerMax < minimum -> lowerMax - 30.
    | _ when minimum < lowerMax -> (minimum+lowerMax)/2.
    | _ when maximum > upperMin -> (maximum+upperMin)/2.
    | _ -> (minimum + maximum)/2.

let generateThreeLines (symbolModel: Symbol.Model) srcPortPos tgtPortPos (oldPoints: XYPos list) (didUserModifyPoint: bool list) = //generates 4 points for wire based on new src and tgt points and old user modification information
    let horizontalDifference = tgtPortPos.X - srcPortPos.X
    let middleLineX = 
        if (didUserModifyPoint.[1] && didUserModifyPoint.[2]) //Check whether user modified the points of vertical line
        then 
            if inbetween (oldPoints.[1].X) (srcPortPos.X) (tgtPortPos.X)
            then (oldPoints.[1].X)
            else selectSide (oldPoints.[1].X) (srcPortPos.X) (tgtPortPos.X)       
        else 
            //find whether there's a bounding box 
            //try to change middleLineX to avoid it,
            let initialXPos = srcPortPos.X+horizontalDifference/2.
            let boundingBoxNearby = boundingBoxNearbyVertical initialXPos (srcPortPos.Y) (tgtPortPos.Y) symbolModel
            match boundingBoxNearby with 
            | None -> initialXPos 
            | Some b -> setupLinePos (srcPortPos.X) (tgtPortPos.X) (b.[0].X) (b.[1].X) 
    [srcPortPos; {X=middleLineX; Y=srcPortPos.Y}; {X=middleLineX; Y=tgtPortPos.Y}; tgtPortPos]


let isSymbolOnLineHorizontal (symbolModel: Symbol.Model) yPos firstX secondX (symbol: Symbol.Symbol) = 
    let boundingBox = Symbol.symbolBoundingBox symbolModel (symbol.Id)
    //let's just write it for vertical lines initially
    yPos > boundingBox.[0].Y && yPos < boundingBox.[2].Y && 
        ((boundingBox.[1].X < firstX && boundingBox.[0].X > secondX ))// || (boundingBox.[0].X > firstX && boundingBox.[1].X < secondX ))


let boundingBoxNearbyHorizontal yPos firstX secondX (symbolModel: Symbol.Model) = 
    symbolModel 
    |> List.tryFind (isSymbolOnLineHorizontal symbolModel yPos firstX secondX)
    |> Option.map (fun sym -> Symbol.symbolBoundingBox symbolModel (sym.Id))

let generateFiveLines (symbolModel: Symbol.Model) srcPortPos tgtPortPos (oldPoints: XYPos list) (didUserModifyPoint: bool list) = //Same as three lines
    let firstVerticalLineX = 
        if didUserModifyPoint.[1] && didUserModifyPoint.[2]
        then 
            if oldPoints.[1].X > srcPortPos.X
            then oldPoints.[1].X 
            else srcPortPos.X
        else srcPortPos.X+100.

    let secondVerticalLineX = 
        if didUserModifyPoint.[3] && didUserModifyPoint.[4]
        then 
            if oldPoints.[4].X < tgtPortPos.X
            then oldPoints.[4].X 
            else tgtPortPos.X
        else tgtPortPos.X-100.

    let horizontalLine = 
        if didUserModifyPoint.[2] && didUserModifyPoint.[3]
        then oldPoints.[2].Y 
        else 
            let initialY = (srcPortPos.Y+tgtPortPos.Y)/2.
            let boundingBoxNearby = boundingBoxNearbyHorizontal initialY firstVerticalLineX secondVerticalLineX symbolModel
            printfn "THE BB NEARBY IS %A" boundingBoxNearby
            match boundingBoxNearby with 
            | None -> initialY 
            | Some b -> setupLinePos (srcPortPos.Y) (tgtPortPos.Y) (b.[0].Y) (b.[2].Y) 
    

    [srcPortPos; {X=firstVerticalLineX; Y=srcPortPos.Y}; {X=firstVerticalLineX;Y=horizontalLine}; {X=secondVerticalLineX; Y=horizontalLine}; {X=secondVerticalLineX; Y=tgtPortPos.Y}; tgtPortPos]

let generateFalses count : bool list = //Helper to create initial DidUserModifyPoint
    List.replicate count false 

let getInitialWirePoints (symbolModel: Symbol.Model) (srcPortPos: XYPos) (tgtPortPos: XYPos) : XYPos list= 
    let initialDidUser = generateFalses 6
    if requiresThreeLines srcPortPos tgtPortPos 
    then generateThreeLines symbolModel srcPortPos tgtPortPos [] initialDidUser
    else generateFiveLines symbolModel srcPortPos tgtPortPos [] initialDidUser

let createNewPoints (symbolModel: Symbol.Model) srcPortPos tgtPortPos oldPoints didUserModifyPoint = 
    let oldSrcPortPos = List.head oldPoints
    let oldTgtPortPos = List.last oldPoints 
    match (requiresThreeLines oldSrcPortPos oldTgtPortPos, requiresThreeLines srcPortPos tgtPortPos) with 
    | (false, true) | (true, false) -> getInitialWirePoints symbolModel srcPortPos tgtPortPos 
    | (true, true) -> generateThreeLines symbolModel srcPortPos tgtPortPos oldPoints didUserModifyPoint//keeping 3 lines
    | (false, false) -> generateFiveLines symbolModel srcPortPos tgtPortPos oldPoints didUserModifyPoint

let getDidUser newPoints oldPoints oldDidUserModify = //work out if algorithm had to change user set points
    if (List.length newPoints <> List.length oldPoints)
    then generateFalses (List.length newPoints)
    else oldDidUserModify

let areBothSymbolsBeingDragged (symbolModel: Symbol.Model) (wire: Wire) = 
    let srcPortId = wire.SrcPort.Id 
    let tgtPortId = wire.TargetPort.Id 
    let isSrcBeingDragged = Symbol.isSymbolBeingDragged symbolModel srcPortId 
    let isTgtBeingDragged = Symbol.isSymbolBeingDragged symbolModel tgtPortId
    isSrcBeingDragged && isTgtBeingDragged 

let addChangesToPoints xChange yChange points = 
    points 
    |> List.map (fun p -> {p with X = p.X+xChange; Y= p.Y+yChange})

let translatePoints srcPortPos (points: XYPos list) = 
    let xChange = srcPortPos.X - points.[0].X 
    let yChange = srcPortPos.Y - points.[0].Y 

    addChangesToPoints xChange yChange points 
let updateWire (symbolModel: Symbol.Model) (wire: Wire) : Wire = 
    // get new source port pos
    let srcPortPos = Symbol.symbolPortPos (symbolModel) (wire.SrcPort.Id)
    let tgtPortPos = Symbol.symbolPortPos (symbolModel) (wire.TargetPort.Id)
    let areBothPortsBeingDragged = areBothSymbolsBeingDragged symbolModel wire

    let newPoints = 
        if areBothPortsBeingDragged 
        then translatePoints srcPortPos (wire.Points)
        else createNewPoints symbolModel srcPortPos tgtPortPos (wire.Points) (wire.DidUserModifyPoint)    
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
    g [] [(g [] wires); symbols; ]


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
    | (first, second) when first.X = second.X -> (inbetween (mousePos.Y) (first.Y) (second.Y)) && (abs (mousePos.X-first.X) < 5.)
    | (first, second) when first.Y = second.Y -> (inbetween (mousePos.X) (first.X) (second.X)) && (abs (mousePos.Y-first.Y) < 5.)
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

let selectWire wire isCtrlPressed wireModel = 
    if isCtrlPressed then
        wireModel 
        |> List.map (fun w -> if w.Id=wire.Id then 
                                                if w.IsSelected then {w with IsSelected=false}
                                                else {w with IsSelected=true}
                              else w)
    else 
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

let wireSelector (fullModel: Model) (mousePos: XYPos) (isCtrlPressed) = 
    let wireClicked = whichWireClicked (fullModel.WX) mousePos   
    let wireModel = 
        match wireClicked, isCtrlPressed with 
        | Some w, _ -> 
            let segmentClicked = getSegmentClicked w mousePos
            fullModel.WX 
            |> selectWire w isCtrlPressed
            |> addSegment w segmentClicked
        | None, false -> unselectAllWires (fullModel.WX)
        | None, true -> fullModel.WX
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
    | None -> wire

let isOneOfTheSymbolsBeingDragged (symbolModel: Symbol.Model) firstPortId secondPortId : bool = 
    (Symbol.isSymbolBeingDragged symbolModel firstPortId) || (Symbol.isSymbolBeingDragged symbolModel secondPortId)

let moveWire (symbolModel: Symbol.Model) (mousePos: XYPos) (wire: Wire) : Wire = 
    if wire.IsSelected && (not (isOneOfTheSymbolsBeingDragged symbolModel (wire.SrcPort.Id) (wire.TargetPort.Id)))//and neither symbol is being dragged 
    then        
        let newWire = moveWirePoints wire mousePos
        let didUserModify = generateDidUserModify (newWire.Points) (wire.Points) (wire.DidUserModifyPoint)
        {newWire with DidUserModifyPoint=didUserModify}
    else wire 

let moveWires (model: Model) (mousePos: XYPos) = 
    model.WX
    |> List.map (moveWire (model.Symbol) mousePos)

let handleMouseForWires (model: Model) mMsg isCtrlPressed : Model = 
    let wireModel : Wire list= 
        List.map (updateWire (model.Symbol)) (model.WX)
 
    let finalModel = {model with WX=wireModel}
    match mMsg.Op with 
    | Down -> 
        // select symbols 
        let newSymbol, _ = Symbol.update (Symbol.Msg.ClickSymbol (mMsg.Pos, isCtrlPressed)) model.Symbol
        let newWire = wireSelector finalModel (mMsg.Pos) (isCtrlPressed)
        {newWire with Symbol=newSymbol}
        
    | Drag -> {model with WX=moveWires finalModel (mMsg.Pos)}
    | _ -> model

let update (msg : Msg) (model : Model): Model*Cmd<Msg> =
    match msg with
    | Symbol sMsg -> 
        let sm,sCmd = Symbol.update sMsg model.Symbol
        {model with Symbol=sm}, Cmd.map Symbol sCmd

    | AddWire (port1, port2) -> 
        match port1.Width with
        | Some portWidth ->
            let srcPortPos = port1.Pos
            let targetPortPos = port2.Pos
            let wirePoints = getInitialWirePoints (model.Symbol) srcPortPos targetPortPos
            let newWire = makeWireFromPorts (port1) (port2) wirePoints (portWidth)
            {model with WX=List.append model.WX [newWire]}, Cmd.none
        | None ->
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

    | MouseMsg (mMsg, isCtrlPressed) -> 
        handleMouseForWires model mMsg isCtrlPressed, Cmd.ofMsg (Symbol (Symbol.MouseMsg mMsg))

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
                            // make sure both symbols connected to wire is also selected 
                            let srcSymbol = Symbol.findSymbolById model.Symbol wire.SrcPort.HostId
                            let targetSymbol = Symbol.findSymbolById model.Symbol wire.TargetPort.HostId

                            if (wire.IsSelected && srcSymbol.IsSelected && targetSymbol.IsSelected) then {wire with IsCopied = true}
                            else {wire with IsCopied = false}
                            )

        {model with WX=newWX; Symbol=newSymbols}, Cmd.none


    | PasteWires ->
        let pasteMargin = {X=120.; Y=120.}

        let newSymbols, _ = Symbol.update (Symbol.Msg.PasteSymbols pasteMargin) model.Symbol 

        let newWX = 
            model.WX
            |> List.collect (fun wire ->
                                if wire.IsCopied then
                                    let srcPort = wire.SrcPort
                                    let targetPort = wire.TargetPort

                                    let srcSymbol = Symbol.findSymbolById newSymbols srcPort.HostId
                                    let targetSymbol = Symbol.findSymbolById newSymbols targetPort.HostId

                                    let newSrcSymbol = Symbol.findSymbolByOriginCopiedId newSymbols srcSymbol.Id
                                    let newTargetSymbol = Symbol.findSymbolByOriginCopiedId newSymbols targetSymbol.Id

                                    match srcPort.PortType, targetPort.PortType, srcPort.Width with 
                                    | CommonTypes.PortType.Input, CommonTypes.PortType.Output, Some wireWidth ->
                                        let srcPortIndex = 
                                            [0..(srcSymbol.InputPorts.Length-1)]
                                            |> List.find (fun i -> srcSymbol.InputPorts.[i].Id = srcPort.Id)

                                        let targetPortIndex = 
                                            [0..(targetSymbol.OutputPorts.Length-1)]
                                            |> List.find (fun i -> targetSymbol.OutputPorts.[i].Id = targetPort.Id)

                                        let newSrcPort = newSrcSymbol.InputPorts.[srcPortIndex]
                                        let newTargetPort = newTargetSymbol.OutputPorts.[targetPortIndex]
                                        let newSrcPortPos = newSrcPort.Pos
                                        let newTargetPortPos = newTargetPort.Pos
                                        let wirePoints = getInitialWirePoints (model.Symbol) newSrcPortPos newTargetPortPos
                                        let newWire = makeWireFromPorts (newSrcPort) (newTargetPort) wirePoints (wireWidth)

                                        [wire; newWire]
                                        

                                    | CommonTypes.PortType.Output, CommonTypes.PortType.Input, Some wireWidth ->
                                        let srcPortIndex = 
                                            [0..(srcSymbol.OutputPorts.Length-1)]
                                            |> List.find (fun i -> srcSymbol.OutputPorts.[i].Id = srcPort.Id)

                                        let targetPortIndex = 
                                            [0..(targetSymbol.InputPorts.Length-1)]
                                            |> List.find (fun i -> targetSymbol.InputPorts.[i].Id = targetPort.Id)

                                        let newSrcPort = newSrcSymbol.OutputPorts.[srcPortIndex]
                                        let newTargetPort = newTargetSymbol.InputPorts.[targetPortIndex]
                                        let newSrcPortPos = newSrcPort.Pos
                                        let newTargetPortPos = newTargetPort.Pos
                                        let wirePoints = getInitialWirePoints (model.Symbol) newSrcPortPos newTargetPortPos
                                        let newWire = makeWireFromPorts (newSrcPort) (newTargetPort) wirePoints (wireWidth)

                                        [{wire with IsSelected=false}; {newWire with IsSelected=true}]

                                    | _ -> [wire]

                                else [wire]
                                )

        // remove all origincopied from current symbols
        let newSymModel, _ = Symbol.update (Symbol.Msg.ClearOriginCopiedId) newSymbols

        {model with WX=newWX; Symbol=newSymModel}, Cmd.none

    | SelectAll ->
        let newSymbol, _ = Symbol.update (Symbol.Msg.SelectAllSymbols) model.Symbol
        
        let newWX =
            model.WX
            |> List.map (fun wire -> {wire with IsSelected=true})
        
        {model with WX=newWX; Symbol=newSymbol}, Cmd.none

    | BoxSelected (pos1, pos2, isCtrlPressed) ->
        let newSymbol = fst (Symbol.update (Symbol.Msg.BoxSelected (pos1, pos2, isCtrlPressed)) model.Symbol)

        let newWX = 
            let startX, startY, endX, endY = 
                let x1 = pos1.X
                let x2 = pos2.X
                let y1 = pos1.Y
                let y2 = pos2.Y

                if x1 <= x2 && y1 <= y2 then x1, y1, x2, y2
                elif x1 <= x2 && y1 > y2 then x1, y2, x2, y1
                elif x1 > x2 && y1 <= y2 then x2, y1, x1, y2
                else x2, y2, x1, y1

            let portSelected (portPos: XYPos) = 
                startX <= portPos.X && endX >= portPos.X && startY <= portPos.Y && endY >= portPos.Y 

            model.WX
            |> List.map (fun wire -> 
                            let port1 = wire.SrcPort
                            let port2 = wire.TargetPort
           
                            if (portSelected port1.Pos && portSelected port2.Pos) then {wire with IsSelected=true} 
                            else 
                                if isCtrlPressed then wire
                                else {wire with IsSelected=false})

        {model with WX=newWX; Symbol=newSymbol}, Cmd.none


    | SaveModel ->
        let newSymbol, _ = Symbol.update (Symbol.Msg.SaveModel) model.Symbol
        {model with Symbol=newSymbol}, Cmd.none 

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

let wireToConnection (wire: Wire) : CommonTypes.Connection = 
    let vertices = 
        wire.Points
        |> List.map (fun pos -> (pos.X, pos.Y))
    {
        Id = unwrapConnectionId wire.Id
        Source = wire.SrcPort
        Target = wire.TargetPort
        Vertices = vertices
    }

/// Update the symbol with matching componentId to comp, or add a new symbol based on comp.
let updateWireModelWithConnection (wModel: Model) (conn:CommonTypes.Connection): Model =
    let checkIfWireInModel : Wire option = 
        wModel.WX
        |> List.tryFind (fun wire -> wire.Id = CommonTypes.ConnectionId(conn.Id))
    
    let points = 
        conn.Vertices
        |> List.map (fun (x, y) -> 
                        { X = x; Y = y}
                    )
    let sourceWidth = 
        match conn.Source.Width with 
        | Some width -> width
        | None -> failwithf "What? Wire Width not found"

    let updatedWireList : Wire List = 
        match checkIfWireInModel with
        | Some wire ->
            
            // update the models wire list in the model by removing the existing connection with that connection Id
            let updatedWireModel = 
                wModel.WX
                |> List.filter (fun wire -> wire.Id <> CommonTypes.ConnectionId(conn.Id))


            // create updated wire based on the connection given
            let updatedWire =
                {
                    Id = CommonTypes.ConnectionId(conn.Id)
                    SrcPort = conn.Source
                    TargetPort = conn.Target
                    Points = points
                    DidUserModifyPoint = generateFalses (List.length points)
                    SegmentSelected = None
                    IsSelected = false
                    Width = sourceWidth
                    ShowLegend = true
                    IsCopied = false
                }

            // add the updated wire to the head of the wire list in the model
            updatedWire :: updatedWireModel
        
        | None ->

            // create a new wire based on the connection given
            let newWire : Wire = 
                makeWireFromPorts conn.Source conn.Target points sourceWidth
            
            // add the new wire to the head of the wire list in the model
            newWire :: wModel.WX
    
    {wModel with WX = updatedWireList}

let extractWire 
    (wModel: Model) 
    (wId:CommonTypes.ConnectionId) : CommonTypes.Connection= 
    
    List.tryFind (fun wire -> wire.Id = wId) wModel.WX
    |> function
        | Some w -> wireToConnection w
        | None -> failwithf "What? Symbol not found in model"

//extracts the list of wires from the model and converts them to the connection type
let extractWires (wModel: Model) : CommonTypes.Connection list = 
    wModel.WX
    |> List.map wireToConnection
