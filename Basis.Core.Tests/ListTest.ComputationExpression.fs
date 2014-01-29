namespace Basis.Core.Tests

open NUnit.Framework
open FsUnit

open Basis.Core

[<TestFixture>]
module ListComputationExprTest =
  [<Test>]
  let zero() =
    let res = list { () }
    res |> should equal []

  [<Test>]
  let ret() =
    let res = list { return 0 }
    res |> should equal [0]

  [<Test>]
  let yld() =
    let res = list { yield 0 }
    res |> should equal [0]

  [<Test>]
  let retret() =
    let res = list { return 10; return 20; }
    res |> should equal [10]

  [<Test>]
  let yldyld() =
    let res = list { yield 10; yield 20 }
    res |> should equal [10; 20]

  let src_retFrom = [
    TestCaseData([]: int list)
    TestCaseData([10])
  ]

  [<TestCaseSource "src_retFrom">]
  let retFrom(xs: int list) =
    let res = list { return! xs; return 0 }
    res |> should equal xs

  [<TestCaseSource "src_retFrom">]
  let yldFrom(xs: int list) =
    let res = list { yield! xs; yield 0 }
    res |> should equal (xs @ [0])
    res |> should equal (list { yield! xs; return 0 })

  let src_letBinding = [
    TestCaseData([10], ["20"])
    TestCaseData(([]: int list), ([]: string list))
  ]

  [<TestCaseSource "src_letBinding">]
  let letBinding(xs: int list, expected: string list) =
    let res = list {
      let! a = xs
      return a * 2 |> string
    }
    res |> should equal expected

  let src_letBindings = 
    let data (xs: int list, ys: int list, expected: string list) = TestCaseData(xs, ys, expected)
    [
      data ([10; 1], [5; 2], ["15"])
      data ([10; 1], [5],    ["15"])
      data ([10],    [5; 2], ["15"])
      data ([10], [],  [])
      data ([],   [5], [])
      data ([],   [],  [])
    ]

  [<TestCaseSource "src_letBindings">]
  let letBindings(xs: int list, ys: int list, expected: string list) =
    let res = list {
      let! a = xs
      let! b = ys
      return a + b |> string
    }
    res |> should equal expected

  let src_letBindings2 = 
    let data (xs: int list, ys: int list, expected: string list) = TestCaseData(xs, ys, expected)
    [
      data ([10; 1], [5; 2], ["15"; "12"; "6"; "3"])
      data ([10; 1], [5],    ["15"; "6"])
      data ([10],    [5; 2], ["15"; "12"])
      data ([10], [],  [])
      data ([],   [5], [])
      data ([],   [],  [])
    ]

  [<TestCaseSource "src_letBindings2">]
  let letBindings2(xs: int list, ys: int list, expected: string list) =
    let res = list {
      let! a = xs
      let! b = ys
      yield a + b |> string
    }
    res |> should equal expected

  let src_combine =
    let data (xs: int list, expected: int list) = TestCaseData(xs, expected)
    [
      data ([],   [])
      data ([11], [11])
      data ([18], [36])
      data ([1;2], [1])
    ]

  [<TestCaseSource "src_combine">]
  let combine(xs: int list, expected: int list) =
    let res = list {
      let! a = xs
      if a % 2 = 0 then
        return a * 2
      return a
    }
    res |> should equal expected

  let src_combine2 =
    let data (xs: int list, expected: int list) = TestCaseData(xs, expected)
    [
      data ([],   [])
      data ([11], [11])
      data ([18], [36; 18])
      data ([1..5], [1;4;2;3;8;4;5])
    ]

  [<TestCaseSource "src_combine2">]
  let combine2(xs: int list, expected: int list) =
    let res = list {
      let! a = xs
      if a % 2 = 0 then
        yield a * 2
      yield a
    }
    res |> should equal expected

  let src_combine3 =
    let data (xs: int list, expected: int list) = TestCaseData(xs, expected)
    [
      data ([],   [])
      data ([11], [11])
      data ([18], [36])
      data ([1..5], [1;4])
    ]

  [<TestCaseSource "src_combine3">]
  let combine3(xs: int list, expected: int list) =
    let res = list {
      let! a = xs
      if a % 2 = 0 then
        return a * 2
      yield a
    }
    res |> should equal expected

  [<TestCase(0, [|6|])>]
  [<TestCase(1, [|4|])>]
  [<TestCase(5, [|0|])>]
  let whileReturn(n: int, expected: int[]) =
    let counter = ref n
    let res = list {
      while (!counter < 5) do
        counter := !counter + 3
        if !counter % 2 = 0 then
          return !counter
      return 0
    }
    res |> should equal (expected |> Array.toList)

  [<TestCase(0, [|3; 6|])>]
  [<TestCase(1, [|4|])>]
  [<TestCase(5, [|0|])>]
  let whileReturn2(n: int, expected: int[]) =
    let counter = ref n
    let res = list {
      while (!counter < 5) do
        counter := !counter + 3
        if !counter % 2 = 0 then
          return !counter
        yield !counter
      return 0
    }
    res |> should equal (expected |> Array.toList)

  let src_forLoop =
    let data (xs: int list, expected: int list) = TestCaseData(xs, expected)
    [
      TestCaseData((None: int option), 0, (None: int option))
      TestCaseData( Some 1,            5,  Some 1)
      TestCaseData( Some -1,           3,  Some 0)
    ]

  [<Test>]
  let forLoop() =
    let res = list {
      for x in [1; 2; 3; 4; 0; 5; 6] do
        if x = 0 then
          return -1
        yield x
      return 10
    }
    res |> should equal [1; 2; 3; 4; -1]