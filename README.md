ArgParser
=========

Declarative & minimalist command line app framework for haskell

This framework is inspired by the argparse package found in python batteries.

This library provides a small combinator dsl to specify a parser for a datatype.
Running the parser will automatically consume and convert command line
arguments. Default special action such as help/usage are automatically built
from the parser specification.

Here is a quick example. First, we need a datatype:

```haskell
data MyTest = MyTest Int Int
  deriving (Show) -- we will print the values
```

Then, we define a parser:

```haskell
myTestParser :: ParserSpec MyTest
myTestParser = MyTest
  `parsedBy` reqPos "pos1"
  `andBy` optPos 0 "pos2"
```
we proceed to build an interface and run it:

```haskell
main = do
  interface <- mkApp myTestParser
  runApp interface print
```

Building this app will produce an executable `foo` which will behave like this:

    $ foo 1 2
    MyTest 1 2
    $ foo 3
    MyTest 3 0
    
    
 For more information, please visit <haskage link>.