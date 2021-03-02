# Sheets Interface
Sheet has the purpose of integrating the other modules together. Its main requirement is to ensure that all interfaces in all modules work correctly, and to handle user inputs ranging from mouse events to component parameter selection (e.g. component names, number of input or output ports, port width, etc.).

## Model State
The model state for Sheets include Wire, which is of type BusWire.Model (this can be observed in the <a href="/BusWire.md"> BusWire markdown file</a>), and CompInfo, whose type is described below and contains information including the added component label, its port width, the number of input ports, and the number of output ports.
```F#
type Model = 
  {
    Wire: BusWire.Model
    ComponentInfo: CompInfo
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
Message functions are used in Sheets to update its model given certain events from the user (i.e. dragging mouse, clicking a HTML <a> tag to create a new symbol, changing symbol parameters through the HTML input tag, etc.). The list of message functions handled by sheets are shown below:
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
</ul>

## Interface Functions
As Sheets is the highest level module of the three, no interface functions are provided by Sheets to the other modules. However, Sheets utilizes interface functions provided by the other modules.

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
```
This provides Sheet with the position with which a new symbol can be inserted without collision with any other existing symbol in the canvas. This allows new symbols to be inserted in a systematic and organized manner.
