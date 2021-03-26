# Documentation

## Introduction
This project aims to create an interactive schematic drawing canvas to replace the canvas used in Issie, an open-source digital circuit design and simulation developed by Tom Clarke. The project is split into three key modules: Symbol, BusWire, and Sheets - each implementing a distinct functionality. Each module is briefly described in the sections below, with their interfaces detailed in their corresponding markdown files.

## Interfaces
The interface functions and detailed description of functionalities in each module can be found in their corresponding markdown files:
<ul>
  <li><a href="/Symbols.md">Symbols</a></li>
  <li><a href="/BusWire.md">BusWire</a></li>
  <li><a href="/Sheets.md">Sheet</a></li>
</ul>

## External Interfaces
The following external interfaces are provided for ISSIE:

### Interface from Symbols
These interfaces include:
```F#
updateSymbolModelWithComponent (symModel: Model) (comp:CommonTypes.Component) : Model
symToComp (sym: Symbol) : CommonTypes.Component
extractComponent (symModel: Model) (sId:CommonTypes.ComponentId) : CommonTypes.Component
extractComponents (symModel: Model) : CommonTypes.Component list
extractComponentType (comp: CommonTypes.Component) : CommonTypes.ComponentType
```
Each external function is described below:
<ul>
  <li><p><b>updateSymbolModelWithComponent: </b>update the symbol with matching componentId to a new component, or add a new symbol based on the new component.</p></li>
  <li><p><b>symToComp: </b>converts the symbol to a component type, which may be used in Issie.</p></li>
  <li><p><b>extractComponent: </b>extracts the symbol from the model and converts it to a component type.</p></li>
  <li><p><b>extractComponents: </b>extracts the list of symbols from the model and converts them to a list of their component types.</p></li>
  <li><p><b>extractComponentType: </b>extracts component type from a component.</p></li>
</ul>

### Interface from BusWire
These interfaces include:
```F#
wireToConnection (wire: Wire) : CommonTypes.Connection
updateWireModelWithConnection (wModel: Model) (conn:CommonTypes.Connection): Model
extractWire (wModel: Model) (wId:CommonTypes.ConnectionId) : CommonTypes.Connection
extractWires (wModel: Model) : CommonTypes.Connection list
```
Each external function is described below:
<ul>
  <li><p><b>wireToConnection: </b>converts a wire to a CommonTypes connection.</p></li>
  <li><p><b>updateWireModelWithConnection: </b>Update the wire with matching connectionId to new connection, or add the new connection.</p></li>
  <li><p><b>extractWire: </b>extracts connection based on the connection id.</p></li>
  <li><p><b>extractWires: </b>extracts the list of wires from the model and converts them to a list of their connection types.</p></li>
</ul>

## Summary of Demo Features and Functionality
This section describes and demonstrates the primary features implemented in the Draw2D canvas. The RHS column provided by the canvas, though not part of the actual Draw2D canvas implementation, provides an easy interface for user-defined inputs to test the different functionalities defined in the project.

### 1. Canvas
The canvas uses a grid system, in which each symbol within the canvas will be snapped to the grid (i.e. snap-to-grid). Additionally, a RHS column is used for demo purposes (similar to that in ISSIE) to allow user-defined inputs when selecting the symbol of their choice. This RHS column is only used for demo purposes, and is not a core functionality of the Draw2D canvas. The layout of the canvas can be seen in the figure below:
<p align="center">
  <img src="/img/canvas.PNG" width="100%">
</p>

### 2. Symbols
All symbols defined from the ISSIE CommonTypes are shown in the figure below. Note that the custom component is fully customizable from the canvas interface - including customizable name, number of ports, and width of each ports. It follows the same type defined by the Issie CommonTypes.
<p align="center">
  <img src="/img/symbols.PNG" width="80%">
</p>

### 3. Snap-to-Grid
Each symbol in the canvas will be snapped to the closest grid snapping point. This functionality works for both single symbol dragging and multiple symbol dragging. This feature is demonstrated with two symbols shown below:
<p align="center">
  <img src="/img/snap-grid.gif" width="60%">
</p>

### 4. Width Inference
Automatic width-inference of wires are directly implemented. This means symbols with variable port widths will have their port widths inferred automatically as wires are connected to its ports. This applies to the four ISSIE symbols with variable port widths that require width inference:
<ul>
  <li>SplitWire</li>
  <li>MergeWires</li>
  <li>BusSelection</li>
  <li>IOLabel</li>
</ul>
A demonstration of width-inference is shown below with the module SplitWire with an upper LSB set to 2, a MergeWire, and a BusConnection of width 3 and starting bit 3. As can be seen, the widths obtained from each of the three symbols are inferred from its other variable port widths that have been defined.
<p></p>
<p align="center">
  <img src="/img/width-inference.gif" width="60%">
</p>

### 5. Automatically Adding New Symbols in Empty Position
When new symbols are added, they are automatically placed in an empty position in the canvas to prevent collision (unless canvas is completely full). This is feature is demonstrated below:
<p align="center">
  <img src="/img/auto-positioning.gif" width="80%">
</p>

### 6. Automatic Unique Symbol Numbering & Labelling
Each symbol that is generated contains a unique number tag that is automatically generated. This is shown in the figure below:
<p align="center">
  <img src="/img/numbering.PNG" width="70%">
</p>

### 7. Multiple Symbol Selection Dragging & Deletion
Multiple symbols can be selected, dragged, and deleted, simultaneously. This feature is shown below:
<p align="center">
  <img src="/img/multi-symbol.gif" width="60%">
</p>



### 8. Select All (Ctrl+A) and Multiple select by Ctrl+Clicking
By clicking Ctrl+A, users are able to select all symbols and wires in the canvas. Additionally, by holding Ctrl and clicking symbols or wires, users are able to select individual symbols or wires. This is demonstrated below:
<p align="center">
  <img src="/img/ctrl-selecting.gif" width="60%">
</p>

### 9. Copy & Paste
Symbols and wires can be copy and pasted by clicking Ctrl+C and Ctrl+V respectively. This is shown below:
<p align="center">
  <img src="/img/copy-paste.gif" width="60%">
</p>

### 10. Undo & Redo 
The canvas will remember the past 20 actions you make (including adding symbols, moving symbols / wires, adding wires, etc.). Any actions you make in the canvas can be undone by pressing Ctrl+Z. Additionally, any undo's can itself be undone (i.e. redo) by clicking Ctrl+Y. This is demonstrated below:
<p align="center">
  <img src="/img/undo-redo.gif" width="60%">
</p>

### 11. Highlighting Available Ports
When dragging wires from symbols, all target ports in which connections are possible (i.e. same width) are highlighted in the canvas. Ports in which a connection is not possible will remain hidden. This gives an indicator to the user on which connections are possible and which are not. This feature is demonstrated below, where two input and output components can only connect to its counterpart with the same defined port-width:
<p align="center">
  <img src="/img/highlight-ports.gif" width="60%">
</p>

### 12. Showing Ports when Hovering over Symbols
Ports in symbols are only shown when the symbol is hovered, otherwise it remains hidden. This is identical to the interface in ISSIE, and is demonstrated below:
<p align="center">
  <img src="/img/hovering.gif" width="60%">
</p>

### 13. Automatic Wire Routing
Wire routes into either three or five segment shape depending on where input and output ports are - automatically re-routing when ports change positions.

<p align="center">
  <img src="/img/wire-auto-routing.gif" width="60%">
</p>

### 14. Manual Wire Routing
Wires can also be manually adjusted by the user. If a user modifies a particular segment, the program remembers which segment was modified and attempts to keep that segment in place when symbols change position:

<p align="center">
  <img src="/img/wire-manual-routing.gif" width="60%">
</p>

### 15. Symbol avoidance for buswire
When automatic routing, wires attempt to avoid symbols. This is especially important for feedback loops where we need to avoid the current symbol.
<p align="center">
  <img src="/img/symb-avoidance.gif" width="60%">
</p>

### 16. Curved Wires
Wires have curved corners, as shown below.
<p align="center">
  <img src="/img/curved-wires.PNG" width="35%">
</p>

### 17. Buswire dragging two symbols wire retains shape
When dragging both symbols of a wire, the wire retains its shape completly
<p align="center">
  <img src="/img/retain-shape.gif" width="60%">
</p>

### 18. Toggle legend
Wire annotations can be toggled on or off by the user
<p align="center">
  <img src="/img/toggle-legend.gif" width="60%">
</p>

### 19. Symbol Overlap
When symbols overlap, their bounding boxes are shown to indicate their borders and which symbol is on top.
<p align="center">
  <img src="/img/symb-overlap.gif" width="60%">
</p>


### 20. Error Highlighting
Users can toggle to perform error highlighting of ports that have not been connected. This is required before using the Issie 'waveform' feature to ensure the ports of all symbols are connected before the simulation.
<p align="center">
  <img src="/img/toggle-error.gif" width="60%">
</p>
