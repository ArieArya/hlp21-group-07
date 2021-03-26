# Symbols Interface Documentation
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
    | AddSymbol of CommonTypes.ComponentType * XYPos * string
    | DeleteSymbol 
    | BoxSelected of XYPos * XYPos * bool
    | SymbolHovering of XYPos
    | SymbolOverlap
    | ExpandPort of CommonTypes.PortType * int option
    | CopySymbols
    | PasteSymbols of XYPos
    | ClickSymbol of (XYPos * bool)
    | ClearOriginCopiedId
    | SelectAllSymbols
    | SaveModel
    | ErrorHighlightPorts of CommonTypes.Port List
    | UpdateComponentLabel of CommonTypes.Component * string
    | UpdateComponentWidth of CommonTypes.Component * int
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
  <li><b>SymbolOverlap: </b>marks symbols that are overlapped to isOverlapped = true. This is used to show the bounding boxes of different symbols when they overlap, so you can see where the symbols bounding boxes end and which symbol is ontop.</li>
  <li><b>ExpandPort: </b>sets ExpandedPort field of symbol to the correct expanded port (or none if no ports should be expanded). This is used to display larger ports when dragging wires to indicate possible connections. e.g. if an output port is being dragged, all input ports with the same bus-width should be displayed with an expanded port size to indicate possible connection between them, and vice versa.</li>
  <li><b>CopySymbols: </b>marks all selected symbols (i.e. IsSelected=true) to isCopied=true.</li>
  <li><b>PasteSymbols: </b>duplicates symbols with isCopied=true (with its own unique ID).</li>
  <li><b>ClickSymbol: </b>marks all symbols clicked (while ctrl is pressed) to IsSelected=true.</li>
  <li><b>ClearOriginCopiedId: </b>Resets OriginCopiedId to 0 - used when copy-pasting symbols and wires.</li>
  <li><b>SelectAllSymbols: </b>marks all symbols as IsSelected - used when user presses Ctrl+A</li>
    <li><b>SaveModel: </b>when the user performs an action, the previous model is saved (for undo purposes) and all symbol settings (IsDragging, IsHovered, IsSelected) are reset</li>
  <li><b>ErrorHighlightPorts: </b>highlights all the ports on symbols which are not connected to anything, in red. And these highlights can be toggled on and off on successive calls of the message.</li>
  <li><b>UpdateComponentLabel: </b>updates the label of a given symbol</li>
  <li><b>UpdateComponentWidth: </b>updates the width of a given symbol</li>
</ul>

## Interface Functions
Interface functions are used to send information to other module layers (i.e. BusWire and Sheets). The main interface functions used are shown below:
```F#
symbolBoundingBox (symModel: Model) (sId: CommonTypes.ComponentId) : XYPos List
findSymbolById (symModel: Model) (sId: CommonTypes.ComponentId) : Symbol
findSymbolByOriginCopiedId (symModel: Model) (sId: CommonTypes.ComponentId) : Symbol
findPortPos (symModel: Model) (sId: CommonTypes.ComponentId) (portId: string) : XYPos 
symbolPortPos: Model -> string(Port.Id) : XYPos
findPortByPosition (symModel: Model) (pos: XYPos) : CommonTypes.Port option 
isSymbolHoveredAndSelected (symModel: Model) (pos: XYPos) : bool
isSymbolSelected (symModel: Model) (sId: CommonTypes.ComponentId) : bool
isAnySymbolDragging (symModel: Model) : bool
findNextAvailablePos (symModel: Model) (dimensions: float * float) : XYPos
portInference (symModel: Model) (port: CommonTypes.Port) (portWidth: int) : (Model * bool)
variablePortReset (symModel: Model) (port: CommonTypes.Port) : Model
doesPortBelongToSymbol (portId: string) (symbol: Symbol) : bool
findSymbolFromPort (symbolModel: Model) (portId: string) : Symbol Option
isSymbolBeingDragged (symbolModel: Model) (portId: string) : bool
```
<ul>
  <li><b>symbolBoundingBox: </b>returns a list of vertices for the given symbols bounding box from top left, clockwise to bottom left.</li> 
  <li><b>findSymbolById: </b>returns the symbol with the id given.</li>
  <li><b>findSymbolByOriginCopiedId: </b>returns the symbol with the corresponding OriginCopiedId that was given.</li>
  <li><b>findPortPos: </b>returns the position of a port with a given port id, within a symbol of  a given component id. This can be used to find the new position of the port when a symbol is moved. This is important to ensure the ports of a symbol follow the symbol (i.e. appears attached to the symbol) when it is being dragged.</li>
  <li><b>symbolPortPos: </b>finds port position without symbol id. This can be used to find the port position without symbol idfinds the new position of the source port when a symbol is moved. This is important to ensure the ports of a symbol follow the symbol when it is being dragged.</li>
  <li><b>findPortByPosition: </b>returns the port in the mouse position pos. If no port is found, return None. This is to determine whether a user clicks on a port and to extend a wire to animate wiring coming from that component port.</li>
  <li><b>isSymbolHoveredAndSelected: </b>finds if there is a symbol at a certain hovered position and whether it is selected (i.e. isSelected=true). This is an interface used when dragging multiple symbols to prevent all symbols to be unselected upon release.</li>
  <li><b>isSymbolSelected: </b>checks if a symbol specified by its id is selected (i.e. isSelected = true).</li>
  <li><b>isAnySymbolDragging: </b>checks if any symbol in the model is dragging (i.e. isDragging = true).</li>
  <li><b>findNextAvailablePos: </b>finds next available position to insert new symbol. This performs bounding box calculations to ensure no collision of newly inserted symbol. This enables a systematic and organized insertion of new symbols without overlapping components.</li>
  <li><b>portInference: </b>check if two ports can be connected by width inference. If possible, return new symbol model with modified ports and return true.</li>
  <li><b>variablePortReset: </b>when deleting a symbol, variable port widths must be reset for width inference.</li>
  <li><b>doesPortBelongToSymbol: </b>determines if a port belongs to a symbol.</li>
  <li><b>findSymbolFromPort: </b>finds the symbol which the port belongs to.</li>
  <li><b>isSymbolBeingDragged: </b>determines if a port's host symbol is being dragged.</li>
</ul>

## Issie Interface Functions
Interface functions are used to interface with Issie and convert between Symbol and Component types and vice versa. The main Issie interface functions used are shown below:
```F#
updateSymbolModelWithComponent (symModel: Model) (comp:CommonTypes.Component) : Model
symToComp (sym: Symbol) : CommonTypes.Component
extractComponent (symModel: Model) (sId:CommonTypes.ComponentId) : CommonTypes.Component
extractComponents (symModel: Model) : CommonTypes.Component list 
findSelectedSymbol (symModel: Model) : CommonTypes.Component option
```
<ul>
  <li><b>updateSymbolModelWithComponent: </b>updates the symbol model by updating any symbol of matching componentId to the component given using the component given, or adds a new symbol based on component given</li>
  <li><b>symToComp: </b>converts the given symbol into a component type, which is used in Issie</li>
  <li><b>extractComponent: </b>extracts the symbol of the given component id from the model and converts it to a component type before returning</li>
  <li><b>extractComponents: </b>extracts the list of symbols from the model and converts each symbol to the component type, returning an equivalent list of components</li>
  <li><b>findSelectedSymbol: </b>trys to find the selected symbol in the model and converts it to a component type, returning a component option. </li>
</ul>
