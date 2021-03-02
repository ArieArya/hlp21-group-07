# Symbols Interface
The main purpose of Symbols is to create a view function for all the symbols listed in the Issie components. The main types and interfaces used in Symbol.fs is described in the sections below.

## Model State
The model state is defined by default as a list of symbols defined by the Symbol type.
```F#
type Model = Symbol list
```

## Symbol Type
The Symbol type contains additional features from those specified in the Skeleton code. 
```F#
type Symbol =
    {
        Id : CommonTypes.ComponentId
        Pos: XYPos
        LastDragPos : XYPos
        IsDragging : bool
        isSelected: bool
        isHovered: bool
        Label: string
        InputPorts: CommonTypes.Port list
        OutputPorts: CommonTypes.Port list
        ExpandedPort: CommonTypes.PortType option
        Vertices: XYPos list
    }
```
The description of each field is described below:
<ul>
  <li><b>Id:</b> defines the Id of the component.</li>
  <li><b>Pos:</b> defines the current position of the center of the component.</li>
  <li><b>LastDragPos:</b> defines the last dragging position of the component.</li>
  <li><b>isDragging:</b> determines whether the component is currently being dragged.</li>
  <li><b>isSelected:</b> determines whether the component is selected by a user. This allows multiple components to be selected simultaneously to allow multiple component dragging and deletion.</li>
  <li><b>Label:</b> specifies the user-defined label for the added component.</li>
  <li><b>InputPorts:</b> defines a list of input ports for the component. Each input port has a type CommonTypes.Port.</li>
  <li><b>OutputPorts:</b> defines a list of output ports for the component. Each output port has a type CommonTypes.Port.</li>
  <li><b>ExpandedPort:</b> defines the type of port that should be expanded in the component. This is used to indicate possible port connections when dragging wires. If ExpandedPort = None, no ports in the component should be expanded.</li>
  <li><b>Vertices:</b> defines the vertices of the shape of the component.</li>
</ul>

## Port Type
Additional fields are also added to the CommonTypes.Port type.
```F#
type Port = 
      {
            Id : string
            PortNumber : int option
            PortType : PortType
            HostId : ComponentId
            Pos: XYPos
            Width: int
       } 
```
The description of each field is described below:
<ul>
  <li><b>Id:</b> defines the Id of the port.</li>
  <li><b>PortNumber:</b> defines the port number within the component.</li>
  <li><b>PortType:</b> defines the type of the port (i.e. CommonTypes.PortType.Input or CommonTypes.PortType.Output).</li>
  <li><b>HostId:</b> defines the Id of the host component.</li>
  <li><b>Pos:</b> defines the current position of the port.</li>
  <li><b>Width:</b> defines the bus width of the port.</li>
</ul>

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
findSrcPortPos (symModel: Model) (sId: CommonTypes.ComponentId) (portId: string) : XYPos 
findTargetPortPos (symModel: Model) (sId: CommonTypes.ComponentId) (portId: string) : XYPos 
findPortByPosition (symModel: Model) (pos: XYPos) : CommonTypes.Port option 
isSymbolHoveredAndSelected (symModel: Model) (pos: XYPos) : bool
isSymbolSelected (symModel: Model) (sId: CommonTypes.ComponentId) : bool
isAnySymbolDragging (symModel: Model) : bool
findNextAvailablePos (symModel: Model) (dimensions: float * float) : XYPos
```
<ul>
  <li><b>findSrcPortPos: </b>finds the new position of the source port when a symbol is moved. This is important to ensure the ports of a symbol follow the symbol when it is being dragged.</li>
  <li><b>findTargetPortPos: </b>finds the new position of the target port when a symbol is moved. This has the same functionality as above, and is important to ensure the ports follow the symbol which is being dragged.</li>
  <li><b>findPortByPosition: </b>returns the port in the mouse position pos. If no port is found, return None. This is to determine whether a user clicks on a port and to extend a wire to animate wiring coming from that component port.</li>
  <li><b>isSymbolHoveredAndSelected: </b>finds if there is a symbol at a certain hovered position and whether it is selected (i.e. isSelected=true). This is an interface used when dragging multiple symbols to prevent all symbols to be unselected upon release.</li>
  <li><b>isSymbolSelected: </b>checks if a symbol specified by its id is selected (i.e. isSelected = true).</li>
  <li><b>isAnySymbolDragging: </b>checks if any symbol in the model is dragging (i.e. isDragging = true).</li>
  <li><b>findNextAvailablePos: </b>finds next available position to insert new symbol. This performs bounding box calculations to ensure no collision of newly inserted symbol. This enables a systematic and organized insertion of new symbols without overlapping components.</li>
</ul>
