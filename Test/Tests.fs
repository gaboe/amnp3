namespace Test

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open Calculator

[<TestClass>]
type TestClass () =

    [<TestMethod>]
    member this.TestMethodPassing () =
        let sucasnaMatica = {y1 = 2M; y2 = 2M; u1 = 2M; u2 = 2M};
        let m2 = Calculator.activateCovariantMatrix Calculator.initialCovariantMatrix sucasnaMatica
        
        Assert.IsTrue(true);
