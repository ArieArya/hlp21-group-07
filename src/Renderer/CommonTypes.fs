module CommonTypes
    open Fable.Core
    open Helpers

    let draw2dCanvasWidth = 3000
    let draw2dCanvasHeight = 10000

    //==========================================//
    // Canvas state mapped to f# data structure //
    //==========================================//

    // The next types are not strictly necessary, but help in understanding what is what.
    // Used consistently they provide type protection that greatly reduces coding errors

    /// SHA hash unique to a component - common between JS and F#

    [<Erase>]
    type ComponentId      = | ComponentId of string
    /// SHA hash unique to a connection - common between JS and F#

    [<Erase>]
    type ConnectionId     = | ConnectionId of string
    /// Human-readable name of component as displayed on sheet.
    /// For I/O/labelIO components a width indication eg (7:0) is also displayed, but NOT included here

    [<Erase>]
    type ComponentLabel   = | ComponentLabel of string
    /// SHA hash unique to a component port - common between JS and F#.
    /// Connection ports and connected component ports have the same port Id
    /// InputPortId and OutputPortID wrap the hash to distinguish component
    /// inputs and outputs some times (e.g. in simulation)

    [<Erase>]
    type InputPortId      = | InputPortId of string
    /// SHA hash unique to a component port - common between JS and F#.
    /// Connection ports and connected component ports have the same port Id
    /// InputPortId and OutputPortID wrap the hash to distinguish component
    /// inputs and outputs some times (e.g. in simulation)

    [<Erase>]
    type OutputPortId     = | OutputPortId of string

    /// Port numbers are sequential unique with port lists.
    /// Inputs and Outputs are both numberd from 0 up.

    [<Erase>]
    type InputPortNumber  = | InputPortNumber of int
    /// Port numbers are sequential unique with port lists.
    /// Inputs and Outputs are both numberd from 0 up.

    [<Erase>]
    type OutputPortNumber = | OutputPortNumber of int



    // Specify the position and type of a port in a JSComponent.
    type PortType = Input | Output

    /// A component I/O.
    /// Id (like any other Id) is a string generated with 32 random hex charactes,
    /// so it is (practically) globally unique. These Ids are used by the draw2d
    /// library to uniquely refer to ports and components. They are generated via:
    /// http://www.draw2d.org/draw2d_touch/jsdoc_6/#!/api/draw2d.util.UUID.
    /// PortNumber is used to identify which port on a component, contiguous from 0
    /// separately for inputs and outputs.
    /// HostId is the unique Id of the component where the port is. For example,
    /// all three ports on the same And component will have the same HostId.
    type Port = {
            Id : string
            // For example, an And would have input ports 0 and 1, and output port 0.
            // If the port is used in a Connection record as Source or Target, the Number is None. 
            PortNumber : int option
            PortType : PortType
            HostId : ComponentId
            Pos: XYPos
            Width: int option // must be option to deal with width inference
        } 


    /// Name identified the LoadedComponent used.
    /// The labels define legends on symbol.
    /// Label strings are unique per CustomComponent.
    /// Multiple CustomComponent instances are differentiated by Component data.
    type CustomComponentType = {
        Name: string
        // Tuples with (label * connection width).
        InputLabels: (string * int) list
        OutputLabels: (string * int) list 
    }

    type Memory = {
        // How many bits the address should have.
        // The memory will have 2^AddressWidth memory locations.
        AddressWidth : int 
        // How wide each memory word should be, in bits.
        WordWidth : int
        // Data is a list of <2^AddressWidth> elements, where each element is a
        // 64 bit integer. This makes words longer than 64 bits not supported.
        // This can be changed by using strings instead of int64, but that is way
        // less memory efficient.
        Data : Map<int64,int64>
    }

    // Types instantiating objects in the Digital extension.
    type ComponentType =
        | Input of BusWidth: int | Output of BusWidth: int | IOLabel 
        | BusSelection of OutputWidth: int * OutputLSBit: int
        | Constant of Width: int * ConstValue: int
        | Not | And | Or | Xor | Nand | Nor | Xnor |Decode4
        | Mux2 | Demux2
        | NbitsAdder of BusWidth: int
        | Custom of CustomComponentType // schematic sheet used as component
        | MergeWires | SplitWire of BusWidth: int // int is bus width
        // DFFE is a DFF with an enable signal.
        // No initial state for DFF or Register? Default 0.
        | DFF | DFFE | Register of BusWidth: int | RegisterE of BusWidth: int 
        | AsyncROM of Memory | ROM of Memory | RAM of Memory // memory is contents

    /// JSComponent mapped to F# record.
    /// Id uniquely identifies the component within a sheet and is used by draw2d library.
    /// Label is optional descriptor displayed on schematic.
    type Component = {
        Id : string
        Type : ComponentType
        Label : string // All components have a label that may be empty.
        InputPorts : Port list
        OutputPorts : Port list
        X : int
        Y : int
        H : int
        W : int
    }

    /// JSConnection mapped to F# record.
    /// Id uniquely identifies connection globally and is used by library.
    type Connection = {
        Id : string
        Source : Port
        Target : Port
        Vertices : (float * float) list
    }

    /// F# data describing the contents of a single schematic sheet.
    type CanvasState = Component list * Connection list

    //=======//
    // Other //
    //=======//

    type NumberBase = | Hex | Dec | Bin | SDec

    /// Colors to highlight components
    /// Case name is used as HTML color name.
    /// See JSHelpers.getColorString
    /// lots of colors can be added, see https://www.w3schools.com/colors/colors_names.asp
    /// The Text() method converts it to the correct HTML string
    /// Where speed matters the color must be added as a case in the match statement
    type HighLightColor = Red | Blue | Yellow | Green | Orange | Grey | CustomColorLightBlue | CustomColorDarkBlue
    with 
        member this.Text() = // the match statement is used for performance
            match this with
            | Red -> "Red"
            | Blue -> "Blue"
            | Yellow -> "Yellow"
            | Green -> "Green"
            | Grey -> "Grey"
            | CustomColorLightBlue -> "#04a5c2"
            | CustomColorDarkBlue -> "#025766"
            | c -> sprintf "%A" c
            
            

    (*---------------------------Types for wave Simulation----------------------------------------*)
    type MoreWaveData =
        | RamWaveData of addr: uint32 * ramPath: ComponentId list * label:string
        | ExtraData of ramPath: ComponentId list * label:string

    // The "NetList" types contain all the circuit from Diagram in an abstracted form that
    // removed layout info and connections as separate entities. However, connection Ids are
    // available as fileds in components for interface to the Diagram conmponents

    /// The driven (output) side of a connection.
    /// This is stored with a NLComponent output port number.
    /// Note that one output port can drive multiple NLTargets.
    type NLTarget = {
        TargetCompId: ComponentId
        InputPort: InputPortNumber
        TargetConnId: ConnectionId
        }

    /// The driving (input) side of a connection.
    /// This is stored with a NLComponent input port number
    type NLSource = {
        SourceCompId: ComponentId
        OutputPort: OutputPortNumber
        SourceConnId: ConnectionId
        }

    /// Components with inputs and outputs directly referencing other components.
    /// Output ports can connect to multiple components, or none.
    /// Input ports connect to a single driver, or nothing.
    type NetListComponent = {
        Id : ComponentId
        Type : ComponentType
        Label : string
        // List of input port numbers, and single mapped driving output port
        // and component.
        Inputs : Map<InputPortNumber, NLSource option>
        // Mapping from each output port number to all of the input ports and
        // Components connected to that port.
        Outputs : Map<OutputPortNumber, NLTarget list>
     }

    /// Circuit topology with connections abstracted away.
    /// Good for Wavesim calculations.
    type NetList = Map<ComponentId,NetListComponent>


    (*-----------------------------------------------------------------------------*)
    // Types used within waveform Simulation code, and for saved wavesim configuartion

    /// Identifies a connected net
    /// Does this tie together labelled nets? If so it should have a ComponentLabel option.
    /// should it include the display name(s)? this can be calculated
    type NetGroup = { driverNet: NLTarget list; connectedNets: NLTarget list array }

    /// Info saved by Wave Sim.
    /// This info is not necessarilu uptodate with deletions or additions in the Diagram.
    /// The wavesim code processing this will not fail if non-existent nets are referenced.
    type SavedWaveInfo = {
        ClkWidth: float
        Cursor: uint32 
        Radix: NumberBase
        LastClk: uint32
        DisplayedPortIds: string array
    }

    (*--------------------------------------------------------------------------------------------------*)

    /// Static data describing a schematic sheet loaded as a custom component.
    /// Every sheet is always identified with a file from which it is loaded/saved. 
    /// Name is human readable (and is the filename - without extension) and identifies sheet.
    /// File path is the sheet directory and name (with extension).
    /// InputLabels, OutputLabels are the I/O connections.
    /// The I/O connection integers are bus widths.
    /// The I/O connection strings are human readable. The strings are guaranteed
    /// to be unique in the I/O connection list. I.e. An input label may be the same
    /// as an output label, but two input (or output) labels cannot be the same.
    /// The position in the I/O connections list is important as it implicitly
    /// indicates the port number. For example, the first element in the InputLabels
    /// list is related to the Component's Port with PortNumber 0.
    /// Two instances of a loaded component have the same LoadedComponent data.
    type LoadedComponent = {
        /// File name without extension = sheet name
        Name: string
        /// When the component was last saved
        TimeStamp: System.DateTime 
        /// Complete file path, including name and dgm extension
        FilePath : string
        /// Info on WaveSim settings
        WaveInfo: SavedWaveInfo option
        /// F# equivalent of Diagram components and connections including layout
        CanvasState : CanvasState
        /// Input port names, and port numbers in any created custom component
        InputLabels : (string * int) list
        /// Output port names, and port numbers in any created custom component
        OutputLabels : (string * int) list
    }

    (*-----------------------------------------------------------------------------*)
    // Types used for naming of waveforms in the Waveform Simulator

    /// Identifies the name of a single driving component of a waveform
    type LabelSegment = { 
        LabName : string
        BitLimits : int*int 
    }

    /// Identifies the names of the driving components and the named labels of a waveform
    type WaveLabel = {
        /// Identifies the names of Output and IOLabel components connected to the waveform's net
        OutputsAndIOLabels : string list
        /// Identifies the driving components' names
        ComposingLabels : LabelSegment list 
    }


    /// Value set to None if the connection width could not be inferred.
    type ConnectionsWidth = Map<ConnectionId, int option>

    /// Documents user circuit error found during connection width inference
    type WidthInferError = {
        Msg : string
        ConnectionsAffected : ConnectionId list // A list of connection Ids.
    }


    /// Messages that will be sent from JS code.
    /// This is a define here as a hack to deal with the F# requirement of no forward references.
    /// This type must be defined before Draw2dwrapper, however Draw2dWrapper must be defined before ModelType
    /// Therefore we must define this here rather than where it should be, which is in ModelType
    /// The type parameters allow us to keep JSCanvas and JSComponent in JSTypes where they should be.
    /// whenever JSDiagramMsg is actually used: TCanvas = JSCanvas, TComponent = JSComponent
    type JSDiagramMsg<'TCanvas,'TComponent> =
        | InitCanvas of 'TCanvas // Has to be dispatched only once.
        | SelectComponent of 'TComponent
        | UnselectComponent of unit
        | InferWidths of unit
        | SetHasUnsavedChanges of bool