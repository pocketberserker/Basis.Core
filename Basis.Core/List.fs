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

  type ListBuilder internal () =
    member this.Zero() = fun k -> k Seq.empty
    member this.Return(x) = fun _ -> Seq.singleton x
    member this.ReturnFrom(xs: _ list) = fun _ -> Seq.ofList xs
    member this.Yield(x) = fun k -> Seq.singleton x |> k
    member this.YieldFrom(xs: _ list) = fun (k: _ seq -> _) -> k xs
    member this.Bind(xs, f) =
      // Seq.fold using Continuation Passing Style
      let rec fold f acc xs k =
        if Seq.isEmpty xs then k acc
        else f (fun v -> fold f v (Seq.skip 1 xs) k) acc (Seq.head xs)
      fold (fun k acc x -> seq { yield! acc; yield! f x k; }) Seq.empty xs
    member this.Using(x: #IDisposable, f) =
      try f x
      finally match box x with null -> () | notNull -> x.Dispose()
    member this.Combine(f, rest) =
      fun k -> f (fun xs -> rest () k |> Seq.append xs)
    member this.TryWith(f, h) = try f () with e -> h e
    member this.TryFinally(f, g) = try f () finally g ()
    member this.While(guard, f) =
      if not (guard ()) then this.Zero()
      else let x = f () in this.Combine(x, fun () -> this.While(guard, f))
    member this.For(xs: #seq<_>, f) =
      this.Using(
        xs.GetEnumerator(),
        fun itor -> this.While(itor.MoveNext, fun () -> f itor.Current))
    member this.Delay(f) = f
    member this.Run(f) = f () id |> Seq.toList

[<AutoOpen>]
module ListDefaultOps =
  let list = List.ListBuilder()