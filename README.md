# Documentation

## Introduction
This project aims to create an interactive schematic drawing canvas to replace the canvas used in Issie, an open-source digital circuit design and simulation developed by Tom Clarke. The project is split into three key modules: Symbol, BusWire, and Sheets - each implementing a distinct functionality. Each module is briefly described in the sections below, with their interfaces detailed in their corresponding markdown files.

## Interfaces
The interface functions of each module can be found in their corresponding markdown files:
<ul>
  <li><a href="/Symbols.md">Symbols</a></li>
  <li><a href="/BusWire.md">BusWire</a></li>
  <li><a href="/Sheets.md">Sheet</a></li>
</ul>

## Summary of Demo Features and Functionality
### 1. Canvas
The canvas uses a grid system, in which each symbol within the canvas will be snapped to the grid (i.e. snap-to-grid). Additionally, a RHS column is used for demo purposes (similar to that in ISSIE) to allow user-defined inputs when selecting the symbol of their choice. This RHS column is only used for demo purposes, and is not a core functionality of the Draw2D canvas. The layout of the canvas can be seen in the figure below:
<p align="center">
  <img src="/img/canvas.PNG" width="100%">
</p>

### 2. Symbols
All symbols defined from the ISSIE CommonTypes are shown in the figure below:
<p align="center">
  <img src="/img/symbols.PNG" width="80%">
</p>

### 3. Snap-to-Grid
Each symbol in the canvas will be snapped to the closest grid snapping point. This functionality works for both single symbol dragging and multiple symbol dragging.

### 4. Width Inference
Automatic width-inference of wires are directly implemented. This means symbols with variable port widths will have their port widths inferred automatically as wires are connected to its ports. This applies to the four ISSIE symbols with variable port widths that require width inference:
<ul>
  <li>SplitWire</li>
  <li>MergeWires</li>
  <li>BusSelection</li>
  <li>IOLabel</li>
</ul>






