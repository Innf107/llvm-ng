module List = import("@std/list.pls")

let missing = !cat "src/LLVM/FFI/Missing.hs"

let functions = [function | let [_, function] <- regexpMatchGroups("([A-za-z_']+)\\s*::", missing)]

let isContainedInAny(function, files) = match files {
    [] -> false
    (file :: rest) -> match regexpMatch(function, !cat file) {
        [] -> isContainedInAny(function, rest)
        _ -> true
    } 
}

List.for(functions, \function -> {
    if not isContainedInAny(function, ["src/LLVM/Core.hs", "src/LLVM/InstructionBuilder.hs", "src/LLVM/Core/Context.hs"]) then {
        print("Unused FFI binding for ${function}")
    } else {}
})
