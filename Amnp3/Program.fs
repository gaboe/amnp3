// Learn more about F# at http://fsharp.org
// Poznámky z výuky
// Fí^T(k)=[-y(k-1)  -y(k-2)  u(k-1)  u(k-2)]
// e(k)=y(k)-Fí^T(k)*Baňka(k-1)
// c(k)=c(k-1)-( c(k-1)*Fi(k)*FiT(k)*c(k-1) )/( 1+FiT(k)*c(k-1)*Fi(k) )
// Baňka(k)=Baňka(k-1)+c(k)*Fí(k)*e(k)

open System

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    0 // return an integer exit code
