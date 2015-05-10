
Using the REPL
--------------

One of the easiest ways to experiment with Apollo is by using the read-evaluate-print loop. Fire it up as follows:

~~~
$ apollo --repl
Apollo repl, version 0.0.0.0: https://github.com/apollo-lang/apollo

Commands:
  :browse            See all current bindings and their types
  :export <name>     Export a name of type Music to `_repl.mid`
  :quit              Exit the repl

apollo>
~~~

At the `apollo>` prompt, you can enter an expression and it will be evaluated immediately:

~~~
apollo> 1 + 1
2
~~~

You can also define names and use functions from the prelude:

~~~
apollo> myList: [Int] = sequence(0,10)
apollo> mapII(\x: Int -> Int: x * x, myList)
[0,1,4,9,16,25,36,49,64,81]
~~~

To see a list of all bound names and their types, include the prelude, use the `:browse` command:

~~~
apollo> :browse
concatMapI : ([Int], (Int) -> Int, Int) -> [Int]
intercalateP : ([Pitch], [Pitch]) -> [Pitch]
intercalateI : ([Int], [Int]) -> [Int]
reverseP : ([Pitch]) -> [Pitch]
reverseI : ([Int]) -> [Int]
initP : ([Pitch]) -> [Pitch]
initI : ([Int]) -> [Int]
lastP : ([Pitch]) -> Pitch
lastI : ([Int]) -> Int
uniform : (Duration, Int) -> [Duration]
replicateP : ([Pitch], Int) -> [Pitch]
replicateI : ([Int], Int) -> [Int]
zip : ([Pitch], [Duration]) -> [Atom]
sequence : (Int, Int) -> [Int]
sumI : ([Int]) -> Int
foldrII : ((Int, Int) -> Int, Int, [Int]) -> Int
filterI : ((Int) -> Bool, [Int]) -> [Int]
concatP : ([Pitch], [Pitch]) -> [Pitch]
concatI : ([Int], [Int]) -> [Int]
lengthP : ([Pitch]) -> Int
lengthB : ([Bool]) -> Int
lengthI : ([Int]) -> Int
mapIB : ((Int) -> Bool, [Int]) -> [Bool]
mapID : ((Int) -> Duration, [Int]) -> [Duration]
mapIP : ((Int) -> Pitch, [Int]) -> [Pitch]
mapII : ((Int) -> Int, [Int]) -> [Int]
~~~

The export command allows you to export the contents of a Music-typed name to a file named "\_repl.midi":

~~~
apollo> myMusic: Music = [[ (A4, \4) ]]
apollo> :export myMusic
Music in `myMusic` exported to _repl.mid
~~~

The `:quit` command exits the repl.
