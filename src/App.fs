module Lit.TodoMVC.App

open System
open Lit

Fable.Core.JsInterop.importSideEffects "./index.css"

type Expr =
    | SUM of left:int * right: int
    | SUB of left:int * right: int
    
    member this.Solve () =
        match this with
        | SUM (left, right) -> left + right
        | SUB (left, right) -> left - right
    
    member this.LeftOperand =
        match this with
        | SUM (left, _)
        | SUB (left, _) -> left
    
    member this.RightOperand =
        match this with
        | SUM (_, right)
        | SUB (_, right) -> right


type Question =
    | SolveExpr of Expr
    
    member this.Solve () =
        match this with
        | SolveExpr expr -> expr.Solve ()

type QuestionAndAnswer =
    { Question: Question
      Solution: int }
    
    member this.IsCorrect () =
        this.Question.Solve () = this.Solution

let ``sums up to`` (max: int) =
    [ for x in 0 .. max do
          for y in 0 .. max do
              SUM (x, y) ]

let ``subs up to`` (max: int) =
    [ for x in 0 .. max do
          for y in 0 .. max do
              SUB (x, y) ]

let shuffle (xs: 'a list) = List.sortBy (fun _ -> Random().Next(0,100)) xs

let generateQuestions () =
    let subs =
        ``subs up to`` 20
        |> List.filter (fun sub -> sub.LeftOperand > 10 && sub.RightOperand <> 0 && sub.Solve () > 0 )
        |> shuffle
        |> List.take 8
    
    let sums =
        ``sums up to`` 20
        |> List.filter (fun sum -> sum.Solve() > 10 && sum.Solve() <= 20 && sum.LeftOperand <> 0 && sum.RightOperand <> 0)
        |> shuffle
        |> List.take 3
    
    [subs; sums]
    |> List.concat
    |> List.map Question.SolveExpr
    |> shuffle

type StartedState =
    { Solved: QuestionAndAnswer list
      Active: Question
      Todo: Question list }

type GameState =
    | NotStarted
    | Started of StartedState

let startNewGame () =
    let questions = generateQuestions ()
    { Solved = []
      Active = Seq.head questions
      Todo = List.ofSeq (Seq.tail questions) }

let submitInput (input: int) (x: StartedState) =
    let nxt =
        x.Todo
        |> List.tryHead
        |> Option.defaultValue x.Active
    
    { x with Solved = { Question = x.Active; Solution = input } :: x.Solved
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
    
    let answer (expected: int) (actual: int) =
        if expected = actual then
            html $"""<span>{expected}</span>"""
        else
            html $"""
                <span style="color:red;text-decoration:line-through" class="font-bold">
                    <span style="color:black">&nbsp;{actual}</span>
                </span>
                <span style="color:red;">&nbsp;{expected}</span>"""
        
    
    let history (startedState: StartedState) =
        let lis =
            [ for solvedExpr in startedState.Solved do
                  let left, operand, right =
                      match solvedExpr.Question with
                      | SolveExpr (Expr.SUB (left, right)) ->
                          (string left), "-", (string right)
                      | SolveExpr (Expr.SUM (left, right)) -> (string left), "+", (string right)

                  let prefix =
                      if solvedExpr.IsCorrect () then
                          "🟩"
                      else
                          "🟥"
                  
                  let solution = answer (solvedExpr.Question.Solve ()) solvedExpr.Solution
                  
                  match solvedExpr.Question with
                  | SolveExpr _ ->
                      html $"""<li class="font-mono text-xl">{prefix} {left} {operand} {right} = {solution}""" ]
        
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
            | SolveExpr (Expr.SUM (left, right)) ->
                html $"""<span class="font-mono text-6xl">{left} + {right}</span>"""
            | SolveExpr (Expr.SUB (left, right)) ->
                html $"""<span class="font-mono text-6xl">{left} - {right}</span>"""

        let keyboard =
            let inline submit (x: int) =
                Ev(fun ev ->
                    ev.preventDefault()
                    setGameState (startedState |> submitInput x |> Started)
                )
            
            [ for x in 0 .. 20 do
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