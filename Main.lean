import Macaulean

def runJSONRPCTest :=
  do let (m2Process,m2Server) <- startM2Server
     let result1 <- m2Server.eval "1+1"
     IO.println s!"Macaulay2 Output: {result1}"
     let result2 <- m2Server.eval "factor 20"
     IO.println s!"Macaulay2 Output: {result2}"
     pure m2Process

def main : IO Unit :=
  do let m2Process <- runJSONRPCTest
     let returnCode <- m2Process.wait
     IO.println s!"Macaulay2 Return Code: {returnCode}"
     IO.println s!"Hello, {hello}!"
