(* Exit code management *)
let exitCodeValue : int ref = ref 0

let setExitCodeValue errorCode = 
  exitCodeValue := errorCode