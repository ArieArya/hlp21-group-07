# BusWire Interface
BusWire has a basic requirement to implement the View function of fixed autorouted wires. The main types and interfaces used in BusWire.fs is described in the sections below:

## Model State
The model state for BusWire consists of a list of symbols defined by the Symbol type, a list of wires defined by the Wire type, a color of type CommonTypes.HighLightColor (to set different wire colors), 
```F#
type Model = 
  {
    Symbol: Symbol.Model
    WX: Wire list
    Color: CommonTypes.HighLightColor
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
Message functions in BusWire are used to handle incoming messages from the upper layer module Sheets and to correspondingly update its model. BusWire provides both message functions for the upper level Sheets, but also utilizes message functions from the lower level module Symbols. These message functions are described below:

### Message functions Provided By BusWire
The list of messages handled by BusWire is shown below:
```F#
type Msg =
    | Symbol of Symbol.Msg
    | AddWire of (CommonTypes.Port * CommonTypes.Port)
    | SetColor of CommonTypes.HighLightColor
    | DeleteWire of CommonTypes.ConnectionId
    | ToggleLegend of CommonTypes.ConnectionId
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
  <li><b>MouseMsg: </b>handles mouse events (i.e. Up, Down, Drag, Move).</li>
</ul>

### Message functions used by BusWire provided by Symbols
The only message sent by BusWire to the lower-level module Symbols is shown below:
```F#
Symbol.Msg.DeleteSymbol 
```
DeleteSymbol simply deletes all selected symbols (i.e. with isSelected = true) from list of Symbols. This allows more convenient multiple symbol / component deletion from the canvas instead of individual deletion of symbols.

## Interface Functions
BusWire provides interfaces for the upper-level module Sheets whilst also utilizing interface functions from the lower-level module Symbol. 

### Provided interfaces from BusWire to Sheets
The only interface provided by BusWire to Sheets is shown below:
```F#
findSelectedWire (wModel: Model) : CommonTypes.ConnectionId option
```
This function provides the sheet with any wires that are currently being dragged so that users can conveniently delete wires during the dragging process.

### Used interfaces from Symbol
The interface used by BusWire from the Symbols module is shown below:
```F#
isSymbolSelected (symModel: Model) (sId: CommonTypes.ComponentId) : bool
```
This interface is used during the deletion of symbols so it can delete all stray wires from the deleted symbols. By identifying symbols that are currently selected (i.e. isSelected = true), all wires connected to this symbol can be subsequently deleted.
