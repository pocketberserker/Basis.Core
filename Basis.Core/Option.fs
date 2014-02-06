namespace Basis.Core

open System

module Option =
  [<CompiledName "GetOr">]
  let getOr defaultValue = function
  | Some v -> v
  | None -> defaultValue

  [<CompiledName "GetOrElse">]
  let getOrElse defaultValueSource = function
  | Some v -> v
  | None -> defaultValueSource ()

  [<CompiledName "GetOrElse">]
  let getOr' defaultLazyValue = function
  | Some v -> v
  | None -> defaultLazyValue |> Lazy.value

  type OptionBuilder internal () =
    member this.Zero() = fun k -> k None
    member this.Return(x) = fun _ -> Some x
    member this.ReturnFrom(x: _ option) = fun _ -> x
    member this.Bind(x, f) = x |> Option.map f |> getOrElse this.Zero
    member this.Using(x: #IDisposable, f) =
      try f x
      finally match box x with null -> () | notNull -> x.Dispose()
    member this.Combine(f, rest) = fun k -> f (fun _ -> rest () k)
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
    member this.Run(f) = f () id

[<AutoOpen>]
module OptionDefaultOps =
  let option = Option.OptionBuilder()