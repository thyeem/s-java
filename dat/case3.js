
// use outer-scope: but read-only
function case3() {
    function f() {
        console.log(v)
    }
    return f
}

let v = "outer-scope-value"

case3()()

