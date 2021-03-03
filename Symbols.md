# Symbols Interface
The main purpose of Symbols is to create a view function for all the symbols listed in the ISSIE components. The main types and interfaces used in Symbol.fs is described in the corresponding sections below.

## Model State
The model state is defined as a list of symbols defined by the Symbol type. This describes all Issie components currently defined in the canvas.
```F#
type Model = Symbol list
```

## Message Functions
Message functions are used to handle messages from other module layers (i.e. BusWire and Sheets) and correspondingly update the symbol model. The list of messages handled by the Symbol module are shown below:
```F#
type Msg =
    | MouseMsg of MouseT
    | StartDragging of sId : CommonTypes.ComponentId * pagePos: XYPos
    | Dragging of sId : CommonTypes.ComponentId * pagePos: XYPos
    | EndDragging of sId : CommonTypes.ComponentId
    | AddSymbol of XYPos * int * string * XYPos list * int * int 
    | DeleteSymbol 
    | BoxSelected of XYPos * XYPos
    | SymbolHovering of XYPos
    | ExpandPort of CommonTypes.PortType * int
```
The description of each message type is described below:
<ul>
  <li><b>MouseMsg: </b>handles mouse events (e.g. symbol dragging). This feature is unused in the current implementation of Symbol.fs.</li>
  <li><b>StartDragging: </b>updates dragged symbols to isDragging = true to indicate the symbol is beng dragged.</li>
  <li><b>Dragging: </b>updates position of dragged symbols to the correct page position.</li>
  <li><b>EndDragging: </b>stops dragging symbols by setting isDragging = false.</li>
  <li><b>AddSymbol: </b>adds new symbol with user-specified parameters (e.g. component name, number of input ports, etc.) to the Symbol model.</li>
  <li><b>DeleteSymbol: </b>deletes all selected symbols (i.e. with isSelected = true) from list of Symbols.</li>
  <li><b>BoxSelected: </b>marks all symbols that lies within the rectangular dragging box to isSelected = true. This allows multiple symbol dragging and deletion.</li>
  <li><b>SymbolHovering: </b>marks symbols that are hovered at XYPos to isHovered = true. This is used to display all available ports in the symbol when hovering over it (similar to the feature provided in ISSIE).</li>
  <li><b>ExpandPort: </b>sets ExpandedPort field of symbol to the correct expanded port (or none if no ports should be expanded). This is used to display larger ports when dragging wires to indicate possible connections. e.g. if an output port is being dragged, all input ports with the same bus-width should be displayed with an expanded port size to indicate possible connection between them, and vice versa.</li>
</ul>

## Interface Functions
Interface functions are used to send information to other module layers (i.e. BusWire and Sheets). The main interface functions used are shown below:
```F#
findPortPos (symModel: Model) (sId: CommonTypes.ComponentId) (portId: string) : XYPos 
findPortByPosition (symModel: Model) (pos: XYPos) : CommonTypes.Port option 
isSymbolHoveredAndSelected (symModel: Model) (pos: XYPos) : bool
isSymbolSelected (symModel: Model) (sId: CommonTypes.ComponentId) : bool
isAnySymbolDragging (symModel: Model) : bool
findNextAvailablePos (symModel: Model) (dimensions: float * float) : XYPos
symbolPortPos: Model -> string(Port.Id) : XYPos
symbolPortType: Model -> string(Port.Id): CommonTypes.PortType 
symbolPortWidth: Model  -> string(Port.Id):  int
```
<ul>
  <li><b>findPortPos: </b>finds the new position of the port when a symbol is moved. This is important to ensure the ports of a symbol follow the symbol (i.e. appears attached to the symbol) when it is being dragged.</li>
  <li><b>findPortByPosition: </b>returns the port in the mouse position pos. If no port is found, return None. This is to determine whether a user clicks on a port and to extend a wire to animate wiring coming from that component port.</li>
  <li><b>isSymbolHoveredAndSelected: </b>finds if there is a symbol at a certain hovered position and whether it is selected (i.e. isSelected=true). This is an interface used when dragging multiple symbols to prevent all symbols to be unselected upon release.</li>
  <li><b>isSymbolSelected: </b>checks if a symbol specified by its id is selected (i.e. isSelected = true).</li>
  <li><b>isAnySymbolDragging: </b>checks if any symbol in the model is dragging (i.e. isDragging = true).</li>
  <li><b>findNextAvailablePos: </b>finds next available position to insert new symbol. This performs bounding box calculations to ensure no collision of newly inserted symbol. This enables a systematic and organized insertion of new symbols without overlapping components.</li>
  <li><b>symbolPortPos: </b>finds the new position of the source port when a symbol is moved. This is important to ensure the ports of a symbol follow the symbol when it is being dragged.</li>
  <li><b>symbolPortType: </b>returns the port type from the port id</li>
  <li><b>symbolPortWidth: </b>returns the port width from the port id</li>
</ul>
