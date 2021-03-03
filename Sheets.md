# Sheets Interface
Sheet has the purpose of integrating the other modules together. Its main requirement is to ensure that all interfaces in all modules work correctly, and to handle user inputs ranging from mouse events to component parameter selection (e.g. component names, number of input or output ports, port width, etc.). The main functionality of Sheets is to implement the following features:
<ul>
  <li>Animating wire drag and drop animation</li>
  <li>Allows moving of wires and symbols</li>
  <li>Allows adding and removing of symbols and wires</li>
  <li>Automatic positioning of new symbols without collision</li>
  <li>Highlighting available ports (using port inference) during wire drag and drop</li>
  <li>Animating a rectangular dragging box to select multiple symbols and wires to perform multiple dragging / deletion</li>
</ul>

## Model State
The model state for Sheets include Wire, which is of type BusWire.Model (this can be observed in the <a href="/BusWire.md">BusWire markdown file</a>), a dragging box of type 2-tuple (XYPos * XYPos) which defines the coordinates of the box which the user drags to select multiple components, and CompInfo, whose type is described below and contains information including the added component label, its port width, the number of input ports, and the number of output ports.
```F#
type Model = 
  {
    Wire: BusWire.Model
    ComponentInfo: CompInfo
    DragBox: (XYPos * XYPos)
  }
```

## CompInfo Type
CompInfo is an additional type for Sheets to handle user inputs of the component parameters they would like to select. This is shown below:
```F#
type CompInfo = 
  {
    PortWidth: int
    ComponentLabel: string
    NumInputPorts: int
    NumOutputPorts: int
  }
```
The description for each field is shown below:
<ul>
  <li><b>PortWidth: </b>defines the input and output port widths of the newly created component.</li>
  <li><b>ComponentLabel: </b>defines the label (or name) for newly created component.</li>
  <li><b>NumInputPorts: </b>defines the number of input ports in the newly created component.</li>
  <li><b>NumOutputPorts: </b>defines the number of output ports in the newly created component.</li>
</ul>

## Message Functions
Message functions are used in Sheets to update its model given certain events from the user (i.e. dragging mouse, clicking a HTML <a> tag to create a new symbol, changing symbol parameters through the HTML input tag, etc.). In addition, as the purpose of Sheet is to integrate the functionalities of the Symbol and BusWire modules, messages from both lower-level modules are used to update the model within Sheets (i.e. its symbols and wires).
  
### Message Functions Provided by Sheets
```F#
type KeyboardMsg =
    | CtrlS | AltC | AltV | AltZ | AltShiftZ | DEL

type Msg =
    | Wire of BusWire.Msg
    | KeyPress of KeyboardMsg
    | CreateSymbol of float * float
    | ChangeWireWidth of int
    | ChangeCompLabel of string
    | ChangeNumInputPorts of int
    | ChangeNumOutputPorts of int
    | MouseMsg of MouseT
```
Each message function is described below:
<ul> 
  <li><b>Wire: </b>passes any BusWire messages to the BusWire module.</li>
  <li><b>KeyPress: </b>performs specific operations depending on the key that the user presses. In the current implementation, KeyPress handles deletion and changing wire colors as provided in the skeleton code.</li>
  <li><b>CreateSymbol: </b>creates a new symbol (with parameters defined by the user inputs under CompInfo) in an open position in the canvas. This calls the interface function <i>Symbol.findNextAvailablePos</i> to find the free position to ensure no collision of the new symbol with any existing symbols.</li>
  <li><b>ChangeWireWidth: </b>changes the CompInfo.PortWidth to the corresponding port width set by the user. This uses a HTML input tag with an <i>OnChange</i> listener to automatically update the port width.</li>
  <li><b>ChangeCompLabel: </b>changes the CompInfo.ComponentLabel to the corresponding label set by the user. This uses a HTML input tag with an <i>OnChange</i> listener to automatically update the label.</li>
  <li><b>ChangeNumInputPorts: </b>changes the CompInfo.NumInputPorts to the corresponding number of input ports set by the user. This uses a HTML input tag with an <i>OnChange</i> listener to automatically update the number of input ports.</li>
  <li><b>ChangeNumOutputPorts: </b>changes the CompInfo.NumOutputPorts to the corresponding number of output ports set by the user. This uses a HTML input tag with an <i>OnChange</i> listener to automatically update the number of output ports.</li>
  <li><b>MouseMsg: </b>handles mouse events (i.e. Up, Down, Drag, Move) to perform automatic wire connection and animation, wire dragging, and animating a drag box for multiple symbol selection.</li>
</ul>

### Message Functions from BusWire utilized by Sheets
As Sheet obtains all inputs from users, messages must be sent to the BusWire module to correspondingly update its model (e.g. wiring between modules, adding new wires, deleting wires, etc.). The BusWire messages utilized by Sheets are shown below:
```F#
BusWire.Msg.DeleteWire of CommonTypes.ConnectionId
BusWire.Msg.DeleteWiresBySymbol 
BusWire.MouseMsg of MouseT
```
The description of each message is shown below:
<ul>
  <li><b>DeleteWire: </b>deletes a single wire from the BusWire model given its CommonTypes.ConnectionId.</li>
  <li><b>DeleteWiresBySymbol: </b>deletes all wires from symbols with isSelected = True. This feature allows multiple symbols and their connected wires to be deleted simultaneously.</li>
  <li><b>MouseMsg: </b>this passes on mouse messages from the users (Up, Down, Drag, Move) to the BusWire module.</li>
</ul>

### Message Functions from Symbol utilized by Sheets
Sheets will also have to call messages to the Symbol module directly. For example, Sheets can send messages to Symbols to: create new symbols from user input, deleting symbols, moving symbols, showing ports of symbols when hovering over the symbols, etc. Each message function used by Sheets from Symbols is shown below:
```F#
Symbol.Msg.AddSymbol
Symbol.Msg.ExpandPort of CommonTypes.PortType * int
Symbol.Msg.BoxSelected of XYPos * XYPos
Symbol.Msg.SymbolHovering of XYPos
```
Each message and its functionality is described below:
<ul>
  <li><b>AddSymbol: </b>The Sheets model contains the type <i>CompInfo</i>, which describes the parameters of the new component being constructed (i.e. component label, port width, number of input ports, number of output ports). Once the user clicks on a HTML hyperlink (a) tag of the module of choice, a Symbol.Msg.AddSymbol message will be sent to create the new symbol (with the specified parameters) and append it to the symbol list.</li>
  <li><b>ExpandPort: sets ExpandedPort field of symbol to the correct expanded port (or none if no ports should be expanded). This is used to display larger ports when dragging wires to indicate possible connections. e.g. if an output port is being dragged, all input ports with the same bus-width should be displayed with an expanded port size to indicate possible connection between them, and vice versa.</b></li>
  <li><b>BoxSelected: </b>marks all symbols that lies within the rectangular dragging box to isSelected = true. This allows multiple symbol dragging and deletion.</li>
  <li><b>SymbolHovering: </b>marks symbols that are hovered at XYPos to isHovered = true. This is used to display all available ports in the symbol when hovering over it (similar to the feature provided in ISSIE).</li>  
</ul>

## Interface Functions
As Sheets is the highest level module of the three, no interface functions are provided by Sheets to the other modules. However, Sheets utilizes interface functions provided by the other modules to implement its functionality.

### Interfaces from BusWire
The interface function utilized by Sheets from the BusWire interfaces are shown below:
```F#
findSelectedWire (wModel: Model) : CommonTypes.ConnectionId option
```
This interface provides Sheet with the wire that is currently being dragged. If a <i>KeyPress DEL</i> is sent from Sheets, this wire can immmediately be removed from the model.

### Interfaces from Symbols
The interface function utilized by Sheets from the Symbol interfaces are shown below:
```F#
findNextAvailablePos (symModel: Model) (dimensions: float * float) : XYPos
findPortPos (symModel: Model) (sId: CommonTypes.ComponentId) (portId: string) : XYPos
findPortByPosition (symModel: Model) (pos: XYPos) : CommonTypes.Port option 
isSymbolHoveredAndSelected (symModel: Model) (pos: XYPos) : bool
isAnySymbolDragging (symModel: Model) : bool
```
The description of each of the used interfaces from Symbols are shown below:
<ul>
  <li><b>findNextAvailablePos: </b>uses exhaustive search to find the next empty position in the canvas to place new symbols without collision with existing components.</li>
  <li><b>findPortPos: </b>finds the new position of the port when a symbol is moved. This is important to ensure the ports of a symbol follow (i.e. is attached to) the symbol when it is being dragged.</li>
  <li><b>findPortByPosition: </b>returns the port in the mouse position pos. If no port is found, return None. This is to determine whether a user clicks on a port and to extend a wire to animate wiring coming from that component port.</li>
  <li><b>isSymbolHoveredAndSelected: </b>finds if there is a symbol at a certain hovered position and whether it is selected (i.e. isSelected=true). This is an interface used when dragging multiple symbols to prevent all symbols from being unselected upon release of mouse.</li>
  <li><b>isAnySymbolDragging: </b>checks if any symbol in the model is dragging (i.e. isDragging = true). If any symbol is dragging, rectangular dragging box will not be drawn.</li>
</ul>
