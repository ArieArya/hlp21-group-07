# Sheets Interface
Sheet has the purpose of integrating the other modules together. Its main requirement is to ensure that all interfaces in all modules work correctly, and to handle user inputs ranging from mouse events and keyboard events (e.g. copy, paste, undo, redo). The main functionality of Sheets is to implement the following features:
<ul>
  <li>Animating wire drag and drop animation</li>
  <li>Allows moving of wires and symbols</li>
  <li>Allows adding and removing of symbols and wires</li>
  <li>Automatic positioning of new symbols without collision</li>
  <li>Highlighting available ports (using port inference) during wire drag and drop</li>
  <li>Animating a rectangular dragging box to select multiple symbols and wires to perform multiple dragging / deletion</li>
  <li>Copy & Paste</li>
  <li>Undo & Redo</li>
  <li>Snap-to-grid</li>
</ul>

## Model State
The model state for Sheets is shown below. This includes:
<ul>
  <li><p><b>Wire: </b>this holds the Wire model state and is of type BusWire.Model (this can be observed in the <a href="/BusWire.md">BusWire markdown file.</a>)</p></li>
  <li><p><b>UndoWireModels and RedoWire Models: </b> this is of type Wire list, and keeps track of previous models for redo and undo operations.</p></li>
  <li><p><b>DragBox and DragWire: </b>this is of type DragBoxType and DragWireType respectively (shown below), and is required to animate the dragbox for multiple component selection, as well as the wire drag and drop animation.</p></li>
  <li><p><b>CtrlPressed: </b>a state indicating whether the key "Ctrl" is pressed, used to trigger the KeyboardMsg CtrlA (select all), CtrlC (copy), CtrlV (paste), CtrlZ (undo), CtrlY (redo)</p></li>
</ul>

```F#
type Model = 
{
    Wire: BusWire.Model
    UndoWireModels: BusWire.Model list
    RedoWireModels: BusWire.Model list
    ComponentInfo: CompInfo
    DragBox: DragBoxType
    DragWire: DragWireType
    CtrlPressed: bool
}

type DragBoxType = 
{
    Edge1: XYPos
    Edge2: XYPos
    isDragging: bool
}

type DragWireType = 
{
    SrcEdge: XYPos
    TargetEdge: XYPos
    isDragging: bool
    DraggingPort: CommonTypes.PortType
}
```

## Message Functions
Message functions are used in Sheets to update its model given certain events from the user (i.e. dragging mouse, pressing a key, changing component information, etc.). In addition, as the purpose of Sheet is to integrate the functionalities of the Symbol and BusWire modules, messages from both lower-level modules are used to update the model within Sheets (i.e. its symbols and wires).
  
### Message Functions Provided by Sheets
The primary messages provided by Sheets for the Draw2D Canvas implementation is shown below. Note that in the code for Sheet.fs, a number of other message functions are defined. However, these are only to take user-defined inputs and parameters for the component users' would like to add (e.g. name of custom component, LSB of SplitWire, etc.). Thus, this is used only for demo purposes (for ease of adding different symbols) and is not part of the Draw2D implementation.

```F#
type KeyboardMsg =
    | CtrlS | AltC | AltV | AltZ | AltShiftZ | DEL | CtrlA | CtrlC | CtrlV | CtrlZ | CtrlY

type Msg =
    | Wire of BusWire.Msg
    | KeyPress of KeyboardMsg
    | CreateSymbol of CommonTypes.ComponentType * float * float
    | MouseMsg of MouseT
    | CtrlKeyPress of KeyOp
```

Each message function is described below:
<ul> 
  <li><p><b>Wire: </b>passes any BusWire messages to the BusWire module.</p></li>
  <li>
    <p>
    <b>KeyPress: </b>performs specific operations depending on the key that the user presses. The full list of key press operations are shown below:
    <ul>
      <li>Ctrl+A -> Select All</li>
      <li>Ctrl+C -> Copy</li>
      <li>Ctrl+V -> Paste</li>
      <li>Ctrl+Z -> Undo</li>
      <li>Ctrl+Y -> Redo</li>
      <li>DEL -> Delete All Selected</li>
      <li>AltV -> Toggle Buswidth Legend</li>
    </ul>
    </p>
  </li>
  <li><p><b>CreateSymbol: </b>creates a new symbol based on its component type and provided component information. All symbols in the ISSIE CommonTypes are available.</p></li>
  <li><p><b>MouseMsg: </b>handles mouse events (i.e. Up, Down, Drag, Move) to perform wire drag and drop, animating a drag box for multiple symbol selection, manual wire routing, multiple symbol selection by Ctrl selecting, etc.</p></li>
  <li><p><b>CtrlKeyPress: </b> if a user presses the Ctrl key, this message will set the CtrlPressed state of the model to true. This is used for the different KeyboardMsg that requires the use of the Ctrl key.</p></li>
</ul>

### Message Functions from BusWire utilized by Sheets
As Sheet obtains all inputs from users, messages must be sent to the BusWire module to correspondingly update its model (e.g. wiring between modules, adding new wires, deleting wires, etc.). The BusWire messages utilized by Sheets are shown below:
```F#
BusWire.Msg.SelectAll
BusWire.Msg.CopyWires
BusWire.Msg.PasteWires
BusWire.Msg.DeleteWire of (CommonTypes.ConnectionId)
BusWire.Msg.DeleteWiresBySymbol 
BusWire.Msg.ToggleLegend
BusWire.MouseMsg of MouseT * bool
BusWire.Msg.BoxSelected of XYPos * XYPos * bool
BusWire.Msg.SaveModel
```
The description of each message is shown below:
<ul>
  <li><p><b>SelectAll: </b>sets all wires to IsSelected=true. This is used when the user presses Ctrl+A.</p></li>
  <li><p><b>CopyWires: </b>sets all wires that are selected and whose two symbols that make up the connection to IsCopied=true. This marks all wires that are copied in the model for it to be pasted.</p></li>
  <li><p><b>PasteWires: </b>pastes all wires with IsCopied=true to the model.</p></li>
  <li><p><b>DeleteWire: </b>deletes a single wire from the BusWire model given its CommonTypes.ConnectionId.</p></li>
  <li><p><b>DeleteWiresBySymbol: </b>deletes all wires from symbols with isSelected = True. This feature allows multiple symbols and their connected wires to be deleted simultaneously.</p></li>
  <li><p><b>ToggleLegend: </b>toggles between displaying the buswidth legend besides each wire or hiding the legends.</p></li>
  <li><p><b>MouseMsg: </b>this passes on mouse messages from the users (Up, Down, Drag, Move) to the BusWire module, used primarily for manual re-routing.</p></li>
  <li><p><b>BoxSelected: </b>selects all wires whose two ports lie inside a dragbox (defined by two corner points). The bool represents "IsCtrlPressed". If Ctrl is pressed, all other wires that are selected remain selected, otherwise all other wires will be unselected.</p></li>
  <li><p><b>SaveModel: </b>in order to save the previous model to UndoWireModels, all wires and symbols must be set to IsSelected=false, IsCopied=false, IsDragging=false. This message performs this action to avoid bugs and allow for a neater undo / redo implementation.</p></li>
</ul>

### Message Functions from Symbol utilized by Sheets
Sheets will also have to call messages to the Symbol module directly. For example, Sheets can send messages to Symbols to: create new symbols from user input, deleting symbols, moving symbols, showing ports of symbols when hovering over the symbols, showing available ports for connection during wire drag and drop, etc. Each message function used by Sheets from Symbols is shown below:
```F#
Symbol.Msg.AddSymbol
Symbol.Msg.ExpandPort of CommonTypes.PortType * int
Symbol.Msg.SymbolHovering of XYPos
Symbol.Msg.SymbolOverlap
```
Each message and its functionality is described below:
<ul>
  <li><p><b>AddSymbol: </b>The Sheets model contains the type <i>CompInfo</i>, which describes the parameters of the new component being added (i.e. its component type, width, custom name, etc.). Once the user clicks on a HTML hyperlink (a) tag of the module of choice, a Symbol.Msg.AddSymbol message will be sent to create the new symbol (with the specified parameters) and append it to the symbol list. In addition, all new symobls will be placed on an empty space to avoid collisions with any existing symbols.</p></li>
  <li><p><b>ExpandPort: </b>sets ExpandedPort field of symbol to the correct expanded port (or none if no ports should be expanded). This is used to display larger ports when dragging wires to indicate possible connections. e.g. if an output port is being dragged, all input ports with the same bus-width should be displayed with an expanded port size to indicate possible connection between them, and vice versa.</p></li>
  <li><p><b>SymbolHovering: </b>this displays ports on symbols only when hovering the mouse over the symbol, otherwise all ports will remain hidden.</p></li>
  <li><p><b>SymbolOverlap: </b>this updates the symbol model and creates an outline on symbols which are overlapping over one another.</p></li>
</ul>

## Interface Functions
As Sheets is the highest level module of the three, no interface functions are provided by Sheets to the other modules. However, Sheets utilizes interface functions provided by the other modules to implement its functionality.

### Interfaces from BusWire
The interface function utilized by Sheets from the BusWire interfaces are shown below:
```F#
BusWire.isAnyWireHovered (wire: Wire.Model) (pos: XYPos) : bool
```
This interface provides Sheet with a bool on whether or not any wire is currently being hovered, and is used by MouseMsg to initiate wire dragging if the mouse Down operation has a position on top of any wire, otherwise to draw a drag box for multiple component selection.

### Interfaces from Symbols
The interface function utilized by Sheets from the Symbol interfaces are shown below:
```F#
Symbol.findNextAvailablePos (symModel: Symbol.Model) (dimensions: float * float) : XYPos
Symbol.findPortPos (symModel: Symbol.Model) (sId: CommonTypes.ComponentId) (portId: string) : XYPos
Symbol.findPortByPosition (symModel: Symbol.Model) (pos: XYPos) : CommonTypes.Port option 
Symbol.isSymbolHoveredAndSelected (symModel: Symbol.Model) (pos: XYPos) : bool
Symbol.isAnySymbolDragging (symModel: Symbol.Model) : bool
Symbol.portInference (symModel: Symbol.Model) (port: CommonTypes.Port) (portWidth: int) : (Symbol.Model * bool)
```
The description of each of the used interfaces from Symbols are shown below:
<ul>
  <li><p><b>findNextAvailablePos: </b>uses exhaustive search to find the next empty position in the canvas to place new symbols without collision with existing components.</p></li>
  <li><p><b>findPortPos: </b>finds the new position of the port when a symbol is moved. This is important to ensure the ports of a symbol follow (i.e. is attached to) the symbol when it is being dragged.</p></li>
  <li><p><b>findPortByPosition: </b>returns the port in the mouse position pos. If no port is found, return None. This is to determine whether a user clicks on a port and to extend a wire to animate wiring coming from that component port.</p></li>
  <li><p><b>isSymbolHoveredAndSelected: </b>finds if there is a symbol at a certain hovered position and whether it is selected (i.e. isSelected=true). This is an interface used when dragging multiple symbols to prevent all symbols from being unselected upon release of mouse.</p></li>
  <li><p><b>isAnySymbolDragging: </b>checks if any symbol in the model is dragging (i.e. isDragging = true). If any symbol is dragging, rectangular dragging box will not be drawn.
  </p></li>
  <li>
    <p>
    <b>portInference: </b> this interface is crucial for width-inference. When a wire drag-and-drop is performed between two ports, this message will check whether or not such a connection is valid (using width inference). The output of the interface is of type (Model * bool), where bool indicates whether such a connection is possible, and Model is the new symbol model with updated symbols based on width inference. This allows the component SplitWire, MergeWires, BusConnection, and IOLabel to have their variable port widths inferred and updated.
  </p></li>
  
</ul>
