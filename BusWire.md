# BusWire Interface
BusWire has a basic requirement to implement the View function of fixed autorouted wires. However, for this implementation, BusWire will also be handling all mouse messages, including wire dragging, dragBox calculations for multiple symbol selection, etc. The main types and interfaces used in BusWire.fs is described in the sections below:

## Model State
The model state for BusWire consists of a list of symbols defined by the Symbol type, a list of wires defined by the Wire type, a color of type CommonTypes.HighLightColor (to set different wire colors), and a dragging box of type 2-tuple (XYPos * XYPos) which defines the coordinates of the box which the user drags to select multiple components.
```F#
type Model = 
  {
    Symbol: Symbol.Model
    WX: Wire list
    Color: CommonTypes.HighLightColor
    DragBox: (XYPos * XYPos)
  }
```

## Wire Type
The Wire type contains information that specifies the wire connections.
```F#
type Wire = 
  {
    Id: CommonTypes.ConnectionId 
    SrcPort: CommonTypes.Port 
    TargetPort: CommonTypes.Port 
    IsDragging: bool
    DraggingPort: CommonTypes.PortType
  }
```
The description of each field is shown below:
<ul>
  <li><b>Id: </b>defines the Id for each wire connection.</li>
  <li><b>SrcPort: </b>defines the source port for the wire connection.</li>
  <li><b>TargetPort: </b>defines the target port for the wire connection.</li>
  <li><b>IsDragging: </b>defines whether the wire is currently being dragged by the user.</li>
  <li><b>DraggingPort: </b>defines the port type with which the wire is being dragged. i.e. if the output port is being dragged, then DraggingPort = CommonTypes.PortType.Output. By default, this value is CommonTypes.PortType.Input.</li>
</ul>

## Message Functions
Message functions in BusWire are used to handle incoming messages from the upper layer module Sheets and to correspondingly update its model. BusWire provides both message functions for the upper level Sheets, but also utilizes message functions from the lower level module Symbols. This is especially important as the BusWire module handles the majority of mouse events, thus updating symbols (through messages) based on user mouse events must be handled in this module. This is described below:

### Message functions Provided By BusWire
The list of messages handled by BusWire is shown below:
```F#
type Msg =
    | Symbol of Symbol.Msg
    | AddWire of (CommonTypes.Port * CommonTypes.Port)
    | SetColor of CommonTypes.HighLightColor
    | DeleteWire of CommonTypes.ConnectionId
    | DeleteWiresBySymbol 
    | MouseMsg of MouseT
```
The description of each message type is shown below:
<ul> 
  <li><b>Symbol: </b>sends the Symbol layer a message via BusWire.</li>
  <li><b>AddWire: </b>adds new wire connection to the Wire model (i.e. appended to model.WX) given two ports.</li>
  <li><b>SetColor: </b>changes the Color field in the model to the corresponding color.</li>
  <li><b>DeleteWire: </b>removes a wire from the Wire model (i.e. removed from model.WX) given the connection Id.</li>
  <li><b>DeleteWiresBySymbol: </b>removes all wires connected to all selected symbols (i.e. symbols marked IsSelected=true).</li>
  <li><b>MouseMsg: </b>handles mouse events (i.e. Up, Down, Drag, Move) to perform automatic wire connection and animation, wire dragging, and animating a drag box for multiple symbol selection.</li>
</ul>

### Message functions used by BusWire provided by Symbols
The list of messages sent by BusWire to the lower-level module Symbols is shown below:
```F#
DeleteSymbol 
ExpandPort of CommonTypes.PortType * int
BoxSelected of XYPos * XYPos
SymbolHovering of XYPos
```
The description of each message used by BusWire to update its Symbols is described below:
<ul>
  <li><b>DeleteSymbol: </b>deletes all selected symbols (i.e. with isSelected = true) from list of Symbols. This allows multiple symbol / component deletion from the canvas instead of deleting symbols individually.</li>
  <li><b>ExpandPort: </b>sets ExpandedPort field of symbol to the correct expanded port (or none if no ports should be expanded). This is used to display larger ports when dragging wires to indicate possible connections. e.g. if an output port is being dragged, all input ports with the same bus-width should be displayed with an expanded port size to indicate possible connection between them, and vice versa.</li>
  <li><b>BoxSelected: </b>marks all symbols that lies within the rectangular dragging box to isSelected = true. This allows multiple symbol dragging and deletion.</li>
  <li><b>SymbolHovering: </b>marks symbols that are hovered at XYPos to isHovered = true. This is used to display all available ports in the symbol when hovering over it (similar to the feature provided in ISSIE).</li>
</ul>

## Interface Functions
### Provided interfaces from BusWire to Sheets
The only interface provided by BusWire to Sheets is shown below:
```F#
findSelectedWire (wModel: Model) : CommonTypes.ConnectionId option
```
This function provides the sheet with any wires that are currently being dragged so that users can conveniently delete wires during the dragging process.

### Used interfaces from Symbol
Various interfaces are used by BusWire from the Symbols module to perform automatic wire routing, box dragging for multiple symbol selection, etc. These used interfaces are shown below:
```F#
findSrcPortPos (symModel: Model) (sId: CommonTypes.ComponentId) (portId: string) : XYPos 
findTargetPortPos (symModel: Model) (sId: CommonTypes.ComponentId) (portId: string) : XYPos 
findPortByPosition (symModel: Model) (pos: XYPos) : CommonTypes.Port option 
isSymbolHoveredAndSelected (symModel: Model) (pos: XYPos) : bool
isSymbolSelected (symModel: Model) (sId: CommonTypes.ComponentId) : bool
isAnySymbolDragging (symModel: Model) : bool
```
The description of each interface function can be found under the <a href="/Symbols.md"> Symbol markdown file. </a>
