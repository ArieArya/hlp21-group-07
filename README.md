# Interface Documentation

## Introduction
This project aims to create an interactive schematic drawing canvas to replace the canvas used in Issie, an open-source digital circuit design and simulation developed by Tom Clarke. The project is split into three key modules: Symbol, BusWire, and Sheets - each implementing a distinct functionality for the operation of the entire platform. Each module is briefly described in the sections below, with their interfaces detailed in their corresponding markdown files.

## Symbols
The main purpose of Symbols is to create a view function for all the symbols listed in the Issie components. The primary goals, as listed in the specifications, include:
<ul>
  <li>
    Issie (nearly always) requires outputs to be on right, inputs on left. That is simple and enforces left to right signal flow (desirable) but it does limit layouts which can become unpleasant. One option would be to allow interactive rotation of symbols - but then without text adjusting they become unreadable. Another option would be to allow interactive movement of individual inputs or outputs to a different (top or bottom) edge. That would allow signal flow up and down while enforcing good practice rightward flow normally.
  </li>
  <li>
   Do as much sizing as possible automatically, so that nice looking symbols just happen without need for manual adjustment
  </li>
  <li>
    Make symbol definition flexible so that a wider variety of symbols can easily be defined for new components.
  </li>
</ul>
The interfaces used in Symbol.fs can be seen in its corresponding <a href="/Symbols.md"> markdown file. </a>


## BusWire
BusWire has a basic requirement to implement the View function of fixed autorouted wires where the shape of the wire which can have up to 5 segments and wire geometry depends only on the directions and relative position of the two ends. In addition, wires can be implemented such that each wire has its corresponding bus width to infer whether connections can be made. The primary goals listed in the specification includes:
<ul>
  <li>
    You try to reimplement the interactive router from Issie (which in draw2d has a bug). This starts off like the fixed router but allows manual adjustment of each segment by dragging. It addition it can generate extra segments if components are moved after manual adjustment. I'm not quite sure exactly how this works in Issie, but it is quite usable and you can no doubt work it out. Simple manual adjustment would be good for basic functionality.
  </li>
  
  <li>
    You try to make a better fully automatic router. If you try out the fixed wire router in Issie you will see that the inability to adjust wire positions leads to lots of bad diagrams. Some of the "badness" conditions (e.g. connected wires not being on top of each other when they can be so) can be automatically dealt with. Note that bounding box calculations will be helpful and a suitable library should be provided by Sheet.
  </li>
  
  <li>
    The best solution overall would be a better automatic router followed by the possibility of manual correction.
  </li>
</ul>
The interfaces used in BusWire.fs can be seen in its corresponding <a href="/BusWire.md"> markdown file. </a>


# Sheets
Sheet has the purpose of integrating the other modules together. Its main requirement is to ensure that all interfaces in all modules work correctly. The primary goals of Sheets include:
<ul>
  <li>
    implement snap-to-grid or snap-to-alignment or both.
  </li>
  
  <li>
    Work out what prevents fluidity (instant animated response to mouse dragging or movement) and alter update rate or improve algorithms as necessary.
  </li>
  
  <li>
    When a sheet with 100s of components is loaded it can take time because of the necessary operations for each component and wire. Instrument this and see what are the bottlenecks. This is good for future development.
  </li>
  
  <li>
    For a sheet with lots of objects the bounding box calculations, needed by BusWire and Sheet, must be efficient. Can you implement better bounding box library functions with other than an exhaustive search?
  </li>
</ul>
The interfaces used in Sheets.fs can be seen in its corresponding <a href="/Sheets.md"> markdown file. </a>
