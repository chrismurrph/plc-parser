# plc-parser
Reads exported PLC code into a format that can be run by a virtual PLC

To start playing with this code create a REPL and look at user.clj for functions that you can run from there.
For example:

```
lein repl
user=> (refresh)
user=> (view-structure)
```

Wait some time for the function to complete then view its output in `output.clj`.