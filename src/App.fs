module Lit.TodoMVC.App

open System
open Lit

Fable.Core.JsInterop.importSideEffects "./index.css"

type Expr =
    | SUM of left:int * right: int
    | SUB of left:int * right: int

type SolvedExpr =
    { Expr: Expr
      Solution: int }
    
    member this.IsCorrect () =
        match this.Expr with
        | SUM (left, right) -> left + right = this.Solution
        | SUB (left, right) -> left - right = this.Solution

let rnd =
    Random()

let generateNumberBetween x y =
    rnd.Next (x, y)    

let generateSumUpto x =
    let left = generateNumberBetween 1 x
    let right = generateNumberBetween 1 (x - left)
    SUM (left, right)

let generateSubUpto x =
    let left = generateNumberBetween 1 x
    let right = generateNumberBetween 1 left
    SUB (left, right)

let shuffle x = Seq.sortBy (fun _ -> rnd.Next()) x

let generateExprsUpto x =
    seq {
        for _ in 0 .. 9 do
          yield generateSubUpto x
          yield generateSumUpto x  
    }
    |> shuffle
    |> Seq.truncate 11

type StartedState =
    { Solved: SolvedExpr list
      Active: Expr
      Todo: Expr list }

type GameState =
    | NotStarted
    | Started of StartedState

let startNewGame () =
    let exprs = generateExprsUpto 10
    { Solved = []
      Active = Seq.head exprs
      Todo = List.ofSeq (Seq.tail exprs) }

let submitInput (input: int) (x: StartedState) =
    let nxt =
        x.Todo
        |> List.tryHead
        |> Option.defaultValue x.Active
    
    { x with Solved = { Expr = x.Active; Solution = input } :: x.Solved
             Active = nxt
             Todo = List.tail x.Todo }

let isGameFinished (x: StartedState) =
    x.Todo.IsEmpty

[<LitElement("math-app")>]
let MatchComponent() =
    let _ = LitElement.init(fun cfg ->
        cfg.useShadowDom <- false
    )
    
    let gameState, setGameState = Hook.useState(GameState.NotStarted)

    let history (startedState: StartedState) =
        let lis =
            [ for solvedExpr in startedState.Solved do
                  let left, operand, right =
                      match solvedExpr.Expr with
                      | Expr.SUB (left, right) -> left, "-", right
                      | Expr.SUM (left, right) -> left, "+", right
                  
                  let correct =
                      if solvedExpr.IsCorrect () then
                          "🟩"
                      else
                          "🟥" 
                   
                    
                  html $"""<li class="font-mono text-xl">{correct} {left} {operand} {right} = {solvedExpr.Solution}""" ]
        
        html $"""<ul>{lis}</ul>"""
    
    let score (startedState: StartedState) =
        let numberOfCorrect =
            startedState.Solved
            |> List.filter (fun s -> s.IsCorrect ())
            |> List.length
        
        let totalNumber = startedState.Solved.Length
        
        html $"""<span class="text-6xl">{numberOfCorrect} / {totalNumber}</span>"""
    
    match gameState with
    | GameState.NotStarted ->
        html $"""
            <div class="flex flex-row justify-center mt-20">
                <button
                    class="block p-6 rounded bg-gray-300 text-center leading-none font-6xl font-mono"
                    @click={Ev(fun _ -> setGameState (GameState.Started (startNewGame())) )}
                >
                    Start
                </button>
            </div>
        """
    | GameState.Started startedState when isGameFinished startedState ->
        html $"""
            <div class="space-y-4">
                <div class="flex flex-row justify-center">
                    {score startedState}
                </div>
                <div class="flex flex-row justify-center">
                    {history startedState}
                </div>
                <div class="flex flex-row justify-center">
                    <button
                        class="block p-6 rounded bg-gray-300 text-center leading-none font-6xl font-mono"
                        @click={Ev(fun _ -> setGameState (GameState.Started (startNewGame())) )}
                    >
                        Opnieuw
                    </button>
                </div>
            </div>
        """
    | GameState.Started startedState ->
        let currentExpr =
            match startedState.Active with
            | Expr.SUM (left, right) -> html $"""<span class="font-mono text-6xl">{left} + {right}</span>"""
            | Expr.SUB (left, right) -> html $"""<span class="font-mono text-6xl">{left} - {right}</span>"""
        
        let keyboard =
            let inline submit (x: int) =
                Ev(fun ev ->
                    ev.preventDefault()
                    setGameState (startedState |> submitInput x |> Started)
                )
            
            [ for x in 0 .. 10 do
                html $"""
                    <div class="w-14 px-1 mb-2">
                        <button
                            @click={submit x}
                            class="block w-full h-16 rounded bg-gray-300 text-center leading-none"
                        >{x}</button>
                    </div>
                """ ]

        html $"""
            <div class="space-y-4">
                <div class="flex flex-row justify-center">
                    {currentExpr}
                </div>
                <div class="flex flex-row justify-center">
                    {keyboard}
                </div>
                <div class="flex flex-row justify-center">
                    {history startedState}
                </div>
            </div>
        """