﻿module Model

open Fable.React
open Fable.React.Props
open Browser
open Elmish
open Elmish.React

open Helpers
open type CommonTypes.ComponentType
//------------------------------------------------------------------------//
//------------------------------Sheet Types-------------------------------//
//------------------------------------------------------------------------//



/// **********************************************************************************************************************
///                                                       NOTE:
/// 
///                  CompInfo is used to take and store user-defined input parameters (e.g. port widths, custom 
///                  component labels, etc.) for demo purposes only. This is used to replace the ISSIE interface 
///                  for the demo (to allow user-defined modules).
///
/// **********************************************************************************************************************
type CompInfo = {
    
    // for input and output
    InputWidth: int
    OutputWidth: int

    // for bus selection component
    BusSelectionOutWidth: int
    BusSelectionLSB: int

    // for constant
    ConstantValue: int
    ConstantWidth: int

    // for AsyncROM
    AsyncROMMemBits: int
    AsyncROMOutWidth: int

    // for ROM
    ROMMemBits: int
    ROMOutWidth: int

    // for RAM
    RAMMemBits: int
    RAMOutWidth: int

    // for N-bits Adder
    AdderBits: int

    // for SplitWires
    SplitOutWidth: int

    // for Register
    RegWidth: int

    // for RegisterEnabled
    RegEnabledWidth: int

    // for IOLabel
    IOLabelName: string

    // for CustomComponent
    CustComponentName: string
    CustComponentInpPortsList: string
    CustComponentOutPortsList: string
    }

type DragBoxType = {
    Edge1: XYPos
    Edge2: XYPos
    isDragging: bool
}

type DragWireType = {
    SrcEdge: XYPos
    TargetEdge: XYPos
    isDragging: bool
    DraggingPort: CommonTypes.PortType
}

type RightTab =
    | Catalogue
    | Properties


type Model = {
    Wire: BusWire.Model
    UndoWireModels: BusWire.Model list
    RedoWireModels: BusWire.Model list
    ComponentInfo: CompInfo
    DragBox: DragBoxType
    DragWire: DragWireType
    CtrlPressed: bool
    RightPaneTabVisible: RightTab
    SelectedComponent: CommonTypes.Component option
    }

//------------------------------------------------------------------------//
//---------------------------Message Type---------------------------------//
//------------------------------------------------------------------------//

type KeyboardMsg =
    | CtrlS | AltC | AltV | AltZ | AltShiftZ | DEL | CtrlA | CtrlC | CtrlV | CtrlZ | CtrlY

type KeyOp =
    | KeyDown | KeyUp

type Msg =
    | Wire of BusWire.Msg
    | KeyPress of KeyboardMsg
    | CreateSymbol of CommonTypes.ComponentType * float * float
    | MouseMsg of MouseT
    | CtrlKeyPress of KeyOp
    | ChangeRightTab of RightTab
    | ChangeSelectedComponent of CommonTypes.Component option
    | UpdateComponentLabel of CommonTypes.Component * string
    | UpdateComponentWidth of CommonTypes.Component * int
    | ErrorHighlight
    
    /// **********************************************************************************************************************
    ///                                                       NOTE:
    /// 
    ///                  The messages below are NOT part of the Draw2D canvas implementation. It is used to take
    ///                  user-defined input parameters (e.g. port widths, custom component labels, etc.) for demo
    ///                  purposes only. This is used to replace the ISSIE interface for the demo (to allow user-
    ///                  defined modules).
    ///
    /// **********************************************************************************************************************
    
    // for input and output
    | ChangeInputWidth of int
    | ChangeOutputWidth of int

    // for busSelection
    | ChangeBusSelectionOutWidth of int
    | ChangeBusSelectionLSB of int

    // for constant
    | ChangeConstantValue of int
    | ChangeConstantWidth of int

    // for AsyncROM
    | ChangeAsyncROMMemBits of int
    | ChangeAsyncROMOutWidth of int

    // for ROM
    | ChangeROMMemBits of int
    | ChangeROMOutWidth of int

    // for RAM
    | ChangeRAMMemBits of int
    | ChangeRAMOutWidth of int

    // for N-bits Adder
    | ChangeAdderBits of int

    // for SplitWires
    | ChangeSplitOutWidth of int

    // for Register
    | ChangeRegWidth of int

    // for RegisterEnabled
    | ChangeRegEnabledWidth of int

    // for IOLabel
    | ChangeIOLabelName of string

    // for CustomComponent
    | ChangeCustComponentName of string
    | ChangeCustComponentInpPortsList of string
    | ChangeCustComponentOutPortsList of string
