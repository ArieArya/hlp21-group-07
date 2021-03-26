# BusWire Interface Documentation

## Message Functions
Message functions in BusWire are used to handle incoming messages from the upper layer module Sheets and to correspondingly update its model. BusWire provides both message functions for the upper level Sheets, but also utilizes message & interface functions from the lower level module Symbols. These message functions are described below:

### Message functions Provided By BusWire
The list of messages handled by BusWire is shown below:
```F#
type Msg =
    | Symbol of Symbol.Msg
    | AddWire of (CommonTypes.Port * CommonTypes.Port)
    | SetColor of CommonTypes.HighLightColor
    | DeleteWire of (CommonTypes.ConnectionId)
    | DeleteWiresBySymbol
    | MouseMsg of MouseT * bool
    | BoxSelected of XYPos * XYPos * bool
    | ToggleLegend
    | SelectWiresFromSymbol
    | CopyWires
    | PasteWires
    | SelectAll
    | SaveModel
    | ErrorHighlight
```
The description of each message type is shown below:
<ul> 
  <li><b>Symbol: </b>sends the Symbol layer a message via BusWire.</li>
  <li><b>AddWire: </b>adds new wire connection to the Wire model (i.e. appended to model.WX) given two ports.</li>
  <li><b>SetColor: </b>changes the Color field in the model to the corresponding color.</li>
  <li><b>DeleteWire: </b>removes a wire from the Wire model (i.e. removed from model.WX) given the connection Id.</li>
  <li><b>DeleteWiresBySymbol: </b>removes all wires connected to all selected symbols (i.e. symbols marked IsSelected=true).</li>
  <li><b>MouseMsg: </b>handles mouse events (i.e. Up, Down, Drag, Move).</li>
  <li><b>BoxSelected: </b>selects all wires whose two ports fall within the dragging box.</li>
  <li><b>ToggleLegend: </b>toggles whether or not to display the buswidth legend of wires.</li>
  <li><b>CopyWires: </b>sets all wires that are selected (and whose two end symbols are selected) to IsCopied=true.</li>
  <li><b>PasteWires: </b>pastes all wires that are marked as IsCopied=true.</li>
  <li><b>SelectAll: </b>sets all wires to IsSelected=true.</li>
  <li><b>SaveModel: </b>resets all wires and symbols to IsSelected=false and IsCopied=false to save the model (for undo and redo).</li>
  <li><b>ErrorHighlight: </b>highlights all ports which are floating (i.e. not connected by a wire) as red (ErrorHighlight=true).</li>
</ul>


## Interface Functions
BusWire provides interfaces for the upper-level module Sheets whilst also utilizing interface functions from the lower-level module Symbol. 

### Provided interfaces from BusWire to Sheets
The only interface provided by BusWire to Sheets is shown below:
```F#
BusWire.isAnyWireHovered (wire: Wire.Model) (pos: XYPos) : bool
```
This interface provides Sheet with a bool on whether or not any wire is currently being hovered, and is used by MouseMsg to initiate wire dragging if the mouse Down operation has a position on top of any wire, otherwise to draw a drag box for multiple component selection.

### Used interfaces from Symbol
The interface used by BusWire from the Symbols module is shown below:
```F#
Symbol.symbolBoundingBox (symModel: Model) (sId: CommonTypes.ComponentId) : XYPos List
Symbol.isSymbolBeingDragged (symbolModel: Model) (portId: string) : bool
Symbol.symbolPortPos (symModel: Model) (portId: string) : XYPos
Symbol.removeErrorHighlight (symModel: Model) : model.Symbol
Symbol.isSymbolHoveredAndSelected (symModel: Model) (pos: XYPos) : bool
Symbol.isSymbolSelected (symModel: Model) (sId: CommonTypes.ComponentId) : bool
Symbol.variablePortReset (symModel: Model) (port: CommonTypes.Port) : Model
Symbol.findSymbolById (symModel: Model) (sId: CommonTypes.ComponentId) : Symbol
Symbol.findSymbolByOriginCopiedId (symModel: Model) (sId: CommonTypes.ComponentId) : Symbol 
```
The description of each message interface function is shown below:
<ul> 
  <li><b>symbolBoundingBox: </b>returns a list of vertices from top left, clockwise to bottom left.</li>
  <li><b>isSymbolBeingDragged: </b>determines if a port's host symbol is being dragged</li>
  <li><b>symbolPortPos: </b>finds port position from its Id.</li>
  <li><b>removeErrorHighlight: </b>remove error highlighting from all ports</li>
  <li><b>isSymbolHoveredAndSelected: </b>finds if there is a symbol at a certain hovered position and whether it is selected.</li>
  <li><b>isSymbolSelected: </b>checks if symbol is selected.</li>
  <li><b>variablePortReset: </b>when deleting a symbol, all its variable port widths must be reset for width inference.</li>
  <li><b>findSymbolById: </b>extracts symbol from its Id.</li>
  <li><b>findSymbolByOriginCopiedId: </b>extracts symbol by its OriginCopiedId. This is used for copy and pasting (where the origin symbol must be identified to connect new wires).</li>
</ul>

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
