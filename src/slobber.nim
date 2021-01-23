import parser, scanner, strformat

proc go() = 
  echo("ass")
  var sc = initScanner("0FACEH/BYTE ABS(2.0E4)+::=33:ARRAY:=ABSABS = \"bork hole\" UNTIL=0FACEH+2")

  while (let t = next(sc); t != EOF):
    echo &"tok = {t}"

go()
