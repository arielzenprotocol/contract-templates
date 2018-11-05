module Program

open Argu

open ModifyAST
open ResultUtil
open ViewFile

let ERR_MSG_UNSPECIFIED_CONTRACT =
    "Error - Contract file wasn't specified"
let ERR_MSG_FILE_NOT_FOUND : Printf.StringFormat<string -> string,string> =
    "Error - File not found:\n%s"
let ERR_MSG_PARSING_FAILED : Printf.StringFormat<string -> string,string> =
    "Error - Parsing failed for file:\n%s"
let ERR_MSG_INVALID_VIEW_FILE =
    "Error - Invalid view file"
let ERR_MSG_MODIFICATION_TYPE : Printf.StringFormat<string -> string,string> =
    "Error - Incompatibility between the type of a parameter in the contract and in the view file.\nParameter: %s"

let MSG_GENERATED_CONTRACT_TO_FILE : Printf.TextWriterFormat<string -> unit,unit> =
    "Generated contract to file:\n%s" 
let MSG_EXTRACTED_VIEW_TO_FILE : Printf.TextWriterFormat<string -> unit,unit> =
    "Extracted view to file:\n%s"

type Error =
    | Unspecified_Contract 
    | File_Not_Found       of filename:string
    | Parsing_Failed       of filename:string
    | Invalid_View_File
    | Usage                of message:string
    | Modification_Error   of string * Modification_Error

type ProgramResult<'a> = Result<'a, Error>

type CLIArg =
    | [<CliPrefix(CliPrefix.None)>]
        Extract of ParseResults<ExtractArgs>
//    | [<CliPrefix(CliPrefix.None)>]
//        Modify of ParseResults<ModifyArgs>
    | [<CliPrefix(CliPrefix.None)>]
        Generate of ParseResults<GenerateArgs>
with interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Extract(_)      -> "Extract parameters from a file to a .json view file"
//            | Modify(_)       -> "Modify specific parameters in a file"
            | Generate(_)     -> "Generate a new contract from a contract template (contract + view files)"

and ExtractArgs =
    | [<MainCommand; ExactlyOnce>]
        Source_file of src_filename:string
    | [<CliPrefix(CliPrefix.Dash); AltCommandLine("--view")>]
        V of view_filename:string 
with interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Source_file(_) -> "Name of the contract (.fst) file to extract the parameters from"
            | V(_)           -> "Name of the view (.json) file to extract the parameters to.\n"
                              + "If this argument isn't specified the parameters will be printed to stdout"

//and ModifyArgs =
//    | [<Mandatory; CliPrefix(CliPrefix.Dash); AltCommandLine("--name")>]
//        N of name:string
//    | [<Mandatory; CliPrefix(CliPrefix.Dash); AltCommandLine("--value")>]
//        V of value:string
//    | [<MainCommand; ExactlyOnce>]
//        Filenames of source_filename:string * destination_filename:string 
//with interface IArgParserTemplate with
//        member this.Usage =
//            match this with
//            | Filenames(_,_) -> "Names of the source file and the generated file"
//            | N(_)           -> "Parameter name"
//            | V(_)           -> "Parameters value"

and GenerateArgs =
    | [<MainCommand; ExactlyOnce>]
        Source_file of src_filename:string
    | [<CliPrefix(CliPrefix.Dash); AltCommandLine("--view")>]
        V of view_filename:string 
with interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Source_file(_) -> "Name of the contract (.fst) file to use as template"
            | V(_)           -> "Name of the view (.json) file to take the parameters from.\n"
                              + "If this argument isn't specified the parameters will be taken from stdin"




let cli_parser      = ArgumentParser.Create<CLIArg>()
let extract_parser  = ArgumentParser.Create<ExtractArgs>()
//let modify_parser   = ArgumentParser.Create<ModifyArgs>()
let generate_parser = ArgumentParser.Create<GenerateArgs>()

let cli_usage()      = Usage <| cli_parser.PrintUsage()
let extract_usage()  = Usage <| extract_parser.PrintUsage()
//let modify_usage()   = Usage <| modify_parser.PrintUsage()
let generate_usage() = Usage <| generate_parser.PrintUsage()

let with_file (filename : string) : ProgramResult<unit> =
     if System.IO.File.Exists filename
        then Result.Ok ()
        else Result.Error <| File_Not_Found(filename)

let try_parse (filename : string) : ProgramResult<ASTUtils.AST> =
    try 
        Result.Ok <| ASTUtils.parse_file filename
    with _ ->
        Result.Error <| Parsing_Failed(filename)

let handle_extract_args (args : ParseResults<ExtractArgs>) : ProgramResult<unit> =
    if args.GetAllResults() |> List.isEmpty
        then
            Result.Error <| extract_usage()
        else
            match args.TryGetResult ExtractArgs.Source_file with
            | None ->
                throw Unspecified_Contract
            | Some src_filename ->
                with_file src_filename >>=fun()-> 
                    result {
                        let  filename      = System.IO.Path.GetFileName src_filename
                        let  contract_name = System.IO.Path.GetFileNameWithoutExtension src_filename
                        let! ast           = try_parse src_filename
                        let  viewfile      = ViewFile.extractViewFile filename contract_name ast
                        let  json          = ViewFile.renderViewFile viewfile
                        return
                            match args.TryGetResult ExtractArgs.V with
                            | None ->
                                printfn "%s" json
                            | Some view_filename ->
                                System.IO.File.WriteAllText(view_filename, json)
                                printfn MSG_EXTRACTED_VIEW_TO_FILE view_filename
                    }

//let handle_modify_args (args : ParseResults<ModifyArgs>) : ProgramResult<unit> =
//    failwithf "Not implemented yet."

let handle_generate_args (args : ParseResults<GenerateArgs>) : ProgramResult<unit> =
    if args.GetAllResults() |> List.isEmpty
        then
            Result.Error <| generate_usage()
        else
            result {
                let! content =
                    match args.TryGetResult GenerateArgs.V with
                    | None ->
                        Result.Ok <| System.Console.ReadLine()
                    | Some view_filename ->
                        with_file view_filename >>=fun()->
                            Result.Ok <| System.IO.File.ReadAllText view_filename
                
                let! viewfile =
                    parseViewFile content
                    |> withError Invalid_View_File
                    
                let! src_filename =
                    args.TryGetResult GenerateArgs.Source_file
                    |> withError Unspecified_Contract
                
                let! src_ast =
                    with_file src_filename >>=fun()-> try_parse src_filename
                
                let parameters = viewfile._parameters
                
                let! dst_ast =
                    match foldM modify_AST src_ast parameters with
                    | Result.Ok ast ->
                        Result.Ok ast
                    | Result.Error(name, err) ->
                        Result.Error <| Modification_Error(name, err) 
                    
                let dst_filename = viewfile._filename
                
                return
                    System.IO.File.WriteAllText(dst_filename, ASTUtils.ast_to_string dst_ast);
                    printfn MSG_GENERATED_CONTRACT_TO_FILE dst_filename
            }

let handle_cli_arg (arg : CLIArg) : ProgramResult<unit> =
    match arg with
    | Extract(args)  -> handle_extract_args  args
//    | Modify(args)   -> handle_modify_args   args
    | Generate(args) -> handle_generate_args args

let handle_cli_args = List.fold (fun r hd -> r >>=fun()-> handle_cli_arg hd) (Ok ())

[<EntryPoint>]
let main argv =
    if Array.isEmpty argv then printfn "%s" (cli_parser.PrintUsage())
    try
        let results = cli_parser.ParseCommandLine(inputs = argv, raiseOnUsage = true)
        let args    = results.GetAllResults()
        match handle_cli_args args with 
        | Result.Ok () ->
            0
        | Result.Error err ->
            let errorMsg = 
                match err with
                | Unspecified_Contract ->
                    ERR_MSG_UNSPECIFIED_CONTRACT
                | File_Not_Found(filename) ->
                    sprintf ERR_MSG_FILE_NOT_FOUND filename
                | Parsing_Failed(filename) ->
                    sprintf ERR_MSG_PARSING_FAILED filename
                | Invalid_View_File ->
                    ERR_MSG_INVALID_VIEW_FILE
                | Modification_Error(name, Modification_Type(_, _)) ->
                    sprintf ERR_MSG_MODIFICATION_TYPE name
                | Usage msg ->
                    msg
            eprintfn "%s" errorMsg
            1
    with e ->
        printfn "%s" e.Message
        1
