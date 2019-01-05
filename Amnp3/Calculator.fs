module Calculator
open FSharp.Data
open MathNet.Numerics.LinearAlgebra

type DataInput = CsvProvider<"./input.csv">

type Parameters = { y1: decimal; y2: decimal; u1: decimal; u2: decimal}

let toMatrix p =
    let m = matrix [[ float p.y1; float p.y2; float p.u1; float p.u2 ]]
    m

type RowList = DataInput.Row list

let negate (n: decimal) = System.Decimal.Negate n

let rowOnIdexOrDefault i rows = 
    match List.tryItem i rows with
        | Some x -> x 
        | None -> DataInput.Row(0M, 0M)

let getFi index (rows: DataInput.Row list)  = 
    let _1 = rowOnIdexOrDefault (index - 1) rows
    let _2 = rowOnIdexOrDefault (index - 2) rows
    { y1 = negate _1.Y; y2 = negate _2.Y; u1 = _1.U; u2 = _2.U }

let getPredictionError y fi prevParams = 
    //let change = negate(prevParams.y1 * fi.y1) + negate(prevParams.y2 * fi.y2) 
    //                + (prevParams.u1 * fi.u1) + (prevParams.u2 + fi.u2) |> float

    let change = ((toMatrix fi)).Multiply((toMatrix prevParams).Transpose()).Determinant()
    let error = (float y) + change;
    error

let inputData = DataInput.Load("./input.csv")

let rows = Seq.toList inputData.Rows

let _zeroFi = getFi 0 rows 

let sucasnaMatica = {y1 = 2M; y2 = 2M; u1 = 2M; u2 = 2M};
let predchMatica = {y1 = 1M; y2 = 1M; u1 = 1M; u2 = 1M};
let _error = getPredictionError 2 sucasnaMatica predchMatica
