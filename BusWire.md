

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
symbolPortPos: Model -> string(Port.Id) : XYPos
```
This interface is used during the deletion of symbols so it can delete all stray wires from the deleted symbols. By identifying symbols that are currently selected (i.e. isSelected = true), all wires connected to this symbol can be subsequently deleted.

## Issie Interface Functions
Interface functions are used to interface with Issie and convert between Wire and Connection types and vice versa. The main Issie interface functions used are shown below:
```F#
wireToConnection (wire: Wire) : CommonTypes.Connection
updateWireModelWithConnection (wModel: Model) (conn:CommonTypes.Connection): Model
extractWire (wModel: Model) (wId:CommonTypes.ConnectionId) : CommonTypes.Connection
extractWires (wModel: Model) : CommonTypes.Connection list
```
<ul>
  <li><b>symToComp: </b>converts the given Wire into a connection type, which is used in Issie</li>
  <li><b>updateWireModelWithConnection: </b>Update the wire with matching connectionId to new connection, or add the new connection</li>
  <li><b>extractWire: </b>extracts the wire of the given connection id from the model and converts it to a connection type before returning</li>
  <li><b>extractWires: </b>extracts the list of wires from the model and converts them to the connection type, returning a list of connections</li>
</ul>
