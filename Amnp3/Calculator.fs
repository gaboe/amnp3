module Calculator
open FSharp.Data
open MathNet.Numerics.LinearAlgebra

[<Literal>]
let inputName = "./input-zasumeny.csv" 
type DataInput = CsvProvider<inputName, ";">

type RowList = DataInput.Row list

type CovariantMatrix = Matrix<float>

type Parameters = { y1: decimal; y2: decimal; u1: decimal; u2: decimal}

type State = {index: int; parameters: Parameters; covariantMatrix: CovariantMatrix}

let toMatrix p = matrix [[ float p.y1; float p.y2; float p.u1; float p.u2 ]]

let fromMatrix (m:Matrix<float>) =
    {y1 = decimal(m.Item(0, 0));
    y2 = decimal(m.Item(0, 1));
    u1 = decimal(m.Item(0, 2));
    u2 = decimal(m.Item(0, 3))}

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
    let change = ((toMatrix fi)).Multiply((toMatrix prevParams).Transpose()).Determinant()
    let error = (float y) + change;
    error

let activateCovariantMatrix (previousCovMat: CovariantMatrix) fi = 
    let fiMat = toMatrix fi
    let fiTranMat = fiMat.Transpose()

    let nominator = previousCovMat.Multiply(fiTranMat.Multiply(fiMat.Multiply(previousCovMat)))
    printfn "nominator %s" (nominator.ToString())
    
    let denominator = float 1 + fiMat.Multiply(previousCovMat.Multiply(fiTranMat))
    printfn "denominator %s" (denominator.ToString())

    let fraction = nominator.Divide(denominator.Determinant())

    let result = previousCovMat - fraction

    result

let activateParamVectors prevParams (covMat: CovariantMatrix) fi (err: float) =
    let right = covMat.Multiply((toMatrix fi).Transpose()).Multiply(err)
    printfn "right %s" (right.ToString())
    let result = (toMatrix prevParams) - right.Transpose()
    result

let iterate (rows: RowList) (state: State) (row: DataInput.Row) =
    let fi = getFi state.index rows
    let err = getPredictionError row.Y fi state.parameters 
    let covariantM = activateCovariantMatrix state.covariantMatrix fi
    let parameters = activateParamVectors state.parameters covariantM fi err |> fromMatrix
   
    (parameters, err),  {state with 
                            index = state.index + 1;
                            covariantMatrix = covariantM;
                            parameters = parameters}

let inputData = DataInput.Load(inputName)

let rows = Seq.toList inputData.Rows

let iterateRow = iterate rows


let _zeroFi = getFi 0 rows 

let sucasnaMatica = {y1 = -0.44M; y2 = -0.0546M; u1 = 0.44M; u2 = 0.0598M};
let predchMatica = {y1 = -0.44M; y2 = -0.0546M; u1 = 0.44M; u2 = 0.0598M};
let _error = getPredictionError 1.230033933M sucasnaMatica predchMatica

let initialCovariantMatrix =
    let milion = float 1_000_000
    let z = float 0
    matrix [[milion; z; z; z]
            [z; milion; z; z]
            [z; z; milion; z]
            [z; z; z; milion ]]

let m2 = activateCovariantMatrix initialCovariantMatrix sucasnaMatica

let nextParams = activateParamVectors predchMatica m2 _zeroFi _error

let initialState =
    { index = 0; 
    parameters = {y1 = 10M; y2 = 10M; u1 = 10M; u2 = 10M};
    covariantMatrix = initialCovariantMatrix}

let (output, state) = rows |> List.mapFold (fun state row -> iterateRow state row) initialState

let p = state.parameters

type OutputType = CsvProvider<Schema = "U1 (decimal), U2 (decimal), Y1 (decimal), Y2 (decimal), Error (float)", HasHeaders=false>

let o = output |> List.map(fun (e, err) -> OutputType.Row(e.u1, e.u2, e.y1, e.y2, err))

let myCsv = new OutputType(o)

myCsv.Save(__SOURCE_DIRECTORY__ + "/output-zasumeny.csv", ';')
