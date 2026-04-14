#!/usr/bin/env polaris
options {
    "--as-directives" as asDirective: "Print the result as pasteable TH directives"
}

module List = import("@std/list.pls")
module Async = import("@std/async.pls")

let missing = !cat "src/LLVM/FFI/Missing.hs"

let functions = [function | let [_, function] <- regexpMatchGroups("([A-za-z_']+)\\s*::", missing)]

let isContainedInAny(function, files) = match files {
    [] -> false
    (file :: rest) -> match regexpMatch(function, !cat file) {
        [] -> isContainedInAny(function, rest)
        _ -> true
    } 
}

let split(size, list) = match (size, list) {
    (0, _) -> ([], list)
    (_, []) -> ([], [])
    (n, x :: xs) -> {
        let (restTaken, restAfter) = split(size - 1, xs)
        (x :: restTaken, restAfter)
    }
}

let mapAsyncChunked(size, list, f) = match list {
    [] -> []
    _ -> {
        let (thisChunk, rest) = split(size, list)
        let thisChunk = async [f(x) | let x <- thisChunk]
        let rest = mapAsyncChunked(size, rest, f)
        List.append(await thisChunk, rest)
    }
}

let isUsedAsForeignPointerFinalizer(function) = match regexpMatch("AsForeignPtrWith\\s+\"${function}\"", !cat "src/LLVM/FFI/Missing.hs") {
    [] -> false
    _ -> true
}

let uncoveredFunctions = mapAsyncChunked(10, functions, \function -> {
    if not isContainedInAny(function, ["src/LLVM/Core.hs", "src/LLVM/InstructionBuilder.hs", "src/LLVM/Core/Context.hs", "src/LLVM/Core/Phi.hs", "src/LLVM/Target.hs"])
        && not isUsedAsForeignPointerFinalizer(function) then {
        if asDirective then
            Just("wrapDirectly 'Missing.${function} \"TODO\"")
        else
            Just("Unused FFI binding for ${function}")
    } else {
        Nothing
    }
})


List.for(uncoveredFunctions, \function -> match function {
    Just(message) -> print(message)
    _ -> ()
})
