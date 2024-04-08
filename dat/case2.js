
// okay when using let/var lexical bound
function run() {
    let v = "let/var local-bound"
    function f() {
        console.log(v)
    }
    return f
}

let v = "outer-scope-value"

run()()

