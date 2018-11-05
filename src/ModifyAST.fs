module ModifyAST

module AST   = FStar.Parser.AST
module Id    = FStar.Ident
module Const = FStar.Const
module Range = FStar.Range

type Comment    = string * FStar.Range.range
type AST        = AST.modul * list<Comment>
type paramsdata = ParametersData.paramsdata

open ResultUtil

type Modification_Error =
    | Modification_Type of is:paramsdata * required:AST.term'

type Modification_Result<'a> = Result<'a, Modification_Error>

type Named_Modification_Result<'a> = Result<'a, string * Modification_Error>

let private update_range (s : string) (r : Range.range) : Range.range =
    let n = String.length s + 2
    let e = Range.end_of_range r
    let l = Range.line_of_pos e
    let c = Range.col_of_pos e + n
    Range.mk_range (Range.file_of_range r) (Range.start_of_range r) (Range.mk_pos l c) 

let private modify_term' (newdata : paramsdata) (t : AST.term') : Modification_Result<AST.term'> =
    match t, newdata with
    | AST.Const(Const.Const_bool(_)), ParametersData.CD_bool(b) ->
        Result.Ok <| AST.Const(Const.Const_bool(b))
    | AST.Const(Const.Const_int(_,t)), ParametersData.CD_int(s,t') when t = t' ->
        Result.Ok <| AST.Const(Const.Const_int(s,t)) 
    | AST.Const(Const.Const_char(_)), ParametersData.CD_char(c) ->
        Result.Ok <| AST.Const(Const.Const_char(c))
    | AST.Const(Const.Const_string(_,r)), ParametersData.CD_string(s) ->
        Result.Ok <| AST.Const(Const.Const_string(s, update_range s r))
    | _ -> Result.Error <| Modification_Type (newdata, t)

let private modify_term (newdata : paramsdata) (t : AST.term) : Modification_Result<AST.term> =
    result {
        let! tm' = modify_term' newdata (t.tm)
        return { t with tm = tm'  }
    }

let private check_name (name : string) (p : AST.pattern) : bool =
    match p.pat with
    | AST.PatVar(v,_) -> v.idText = name
    | _               -> false

let private modify_pattern_term (name : string) (newdata : paramsdata) ((p,t) : AST.pattern * AST.term)
    : Named_Modification_Result<AST.pattern * AST.term> =
    if check_name name p
        then
            match modify_term newdata t with
            | Result.Ok t' -> Result.Ok (p, t')
            | Result.Error(err) -> Result.Error(name, err)
        else
            Result.Ok (p, t)

let private modify_decl' (name : string) (newdata : paramsdata) (dec' : AST.decl')
    : Named_Modification_Result<AST.decl'> =
    match dec' with
    | AST.TopLevelLet(q, [pt]) ->
        result {
            let! pt' = modify_pattern_term name newdata pt
            return AST.TopLevelLet(q, [pt'])
        }
    | _ -> Result.Ok dec'

let private modify_decl (name : string) (newdata : paramsdata) (dec : AST.decl) : Named_Modification_Result<AST.decl> =
    result {
        let! d' = modify_decl' name newdata dec.d
        return { dec with d = d' }
    }

let modify_modul (name : string) (newdata : paramsdata) (m : AST.modul) : Named_Modification_Result<AST.modul> =
    result {
        match m with
        | AST.Module(lid, decs) ->
            let! decs' = List.map (modify_decl name newdata) decs |> sequenceResultM
            return AST.Module(lid, decs') 
        | AST.Interface(lid, decs, b) ->
            let! decs' = List.map (modify_decl name newdata) decs |> sequenceResultM
            return AST.Interface(lid, decs', b)
    }

let modify_AST ((m, cmts) : AST) ((name,newdata) : string * paramsdata) : Named_Modification_Result<AST> =
    result {
        let! modul' = modify_modul name newdata m 
        return (modul', cmts)
    }
