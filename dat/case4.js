
// use arguments: shadowed
function case4(v) {
    function f() {
        console.log(v)
    }
    return f
}

let v = "outer-scope-value"

case4("shadowed")()

