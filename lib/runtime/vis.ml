open Domain

(* Visibility of type variables, variables, and function names *)

module TDVis = MakeVis (Id)
module TVis = MakeVis (Id)
module VVis = MakeVis (Id)
module FVis = MakeVis (FId)

type tvis = TDVis.t * FVis.t * VVis.t * TVis.t

let tvis_empty = (TDVis.empty, FVis.empty, VVis.empty, TVis.empty)

type vis = TDVis.t * FVis.t * VVis.t

let vis_empty = (TDVis.empty, FVis.empty, VVis.empty)
