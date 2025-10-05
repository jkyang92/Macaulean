import Macaulean
import Lean.Data.JsonRpc

def runJSONRPCTest :=
  do let (m2stdin,m2Process) <-
      IO.Process.spawn {cmd := "M2"
                       , args := #["--script", "./macaulean.m2"]
                       , cwd := .some "./m2/"
                       , env := .empty
                       , inheritEnv := true
                       , setsid := false
                       , stdin := .piped
                       , stdout := .piped} >>=
      IO.Process.Child.takeStdin
     let m2stdinStream := IO.FS.Stream.ofHandle m2stdin
     m2stdinStream.writeRequest
        { id := .num 1 ,
          method := "testMethod",
          param := ["1+1"] }
     m2stdinStream.flush
     let m2Output <- m2Process.stdout.getLine
     IO.println s!"Macaulay2 Output: {m2Output}"
     pure m2Process

def main : IO Unit :=
  do let m2Process <- runJSONRPCTest
     let returnCode <- m2Process.wait
     IO.println s!"Macaulay2 Return Code: {returnCode}"
     IO.println s!"Hello, {hello}!"
