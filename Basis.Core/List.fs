namespace Basis.Core

module Seq =
  let internal takeWhileButFirst pred xs = seq {
    let cont = ref true
    use itor = (xs :> _ seq).GetEnumerator()
    while itor.MoveNext() && !cont do
      let x = itor.Current
      if not (pred x) then
        cont := false
      yield x
  }

module List =
  [<CompiledName "Cons">]
  let cons x xs = x::xs

  open System
  open ComputationExpr

  type ListBuilder internal () =
    member this.Zero() = [], Continue
    member this.Return(x) = [x], Break
    member this.ReturnFrom(xs: _ list) = xs, Break
    member this.Yield(x) = [x], Continue
    member this.YieldFrom(xs: _ list) = xs, Continue
    member this.Bind(xs, f: _ -> _ list * FlowControl) =
      let isBreak = ref false
      let res =
        xs
        |> Seq.map f
        |> Seq.takeWhileButFirst (function _, Continue -> true | _ -> isBreak := true; false)
        |> Seq.collect fst
        |> Seq.toList
      (res, if !isBreak then Break else Continue)
    member this.Using(x: #IDisposable, f: #IDisposable -> _ list * FlowControl) =
      try f x
      finally match box x with null -> () | notNull -> x.Dispose()
    member this.Combine((x: _ list, cont), rest: unit -> _ list * FlowControl) =
      match cont with
      | Break -> x, Break
      | Continue -> let rest, cont = rest () in List.append x rest, cont
    member this.TryWith(f, h) = try f () with e -> h e
    member this.TryFinally(f, g) = try f () finally g ()
    member this.While(guard, f) =
      if not (guard ()) then this.Zero()
      else let x = f () in this.Combine(x, fun () -> this.While(guard, f))
    member this.For(xs: #seq<_>, f) =
      this.Using(
        xs.GetEnumerator(),
        fun itor -> this.While(itor.MoveNext, fun () -> f itor.Current))
    member this.Delay(f: unit -> _ list * FlowControl) = f
    member this.Run(f) = f () |> fst

[<AutoOpen>]
module ListDefaultOps =
  let list = List.ListBuilder()