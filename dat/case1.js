
// typical violation
function run() {
    v = "violation: setting v to something else"
    function f() {
        console.log(v)
    }
    return f
}

let v = "outer-scope-value"

r = run()

r()

