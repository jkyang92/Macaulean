import Macaulean

def main : IO Unit :=
  do let m2Process <-
      IO.Process.spawn {cmd := "M2"
                       , args := #["--script", "./macaulean.m2"]
                       , cwd := .some "./m2/"
                       , env := .empty
                       , inheritEnv := true
                       , setsid := false
                       , stdin := .piped
                       , stdout := .piped}
     m2Process.stdin.putStr "1+1"
     m2Process.stdin.flush
     let m2Output <- m2Process.stdout.getLine
     IO.println s!"Macaulay2 Output: {m2Output}"
     let returnCode <- m2Process.wait
     IO.println s!"Macaulay2 Return Code: {returnCode}"
     IO.println s!"Hello, {hello}!"
