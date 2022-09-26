# Haskell

## History

The tutorial on this github repository is on the programming language Haskell. The first version, Haskell 1.0 was introduced in 1990. It was created in Portland, Oregon. It was written to solve the problem of other existing functional languages exhibited at the time. The existing languages didn’t have a stable foundation for real applications development. Haskell is mainly used for back-end programming in web development, CLI applications and for programming language research. In the past it’s been used in project like Hasura(an open-source GraphQL engine), Github’s Semantic(command-line tool for parsing, analyzing, and comparing source code), and by Meta in Fighting spam project. 

Haskell is a unique programming language due to its strong static system.  It is strongly statistically typed, similar to other functional programming languages. The strong typing gives Haskell the functionality of type inference, a powerful feature at eliminating hidden bugs. All these happens at compile-time, and it minimizes the size of the source code. 

The following words are reserved in Haskell. It is a syntax error to give a variable or a function one of these names. 
- case 
- class 
- data 
- deriving 
- do 
- else 
- if 
- import 
- in 
- infix 
- infixl 
- infixr 
- instance 
- let
- of 
- module 
- newtype 
- then 
- type 
- where





## Setting up Haskell in VS Code

-	Install Haskell-stack through the terminal
```
brew install haskell-stack
```
-	Start a new project 
```
stack new my-project
cd my-project
stack setup
```
-	Start VS Code
```
code .
```
-	Install the appropriate extensions in vs code
o	Haskell Syntax Highlighting
o	Haskell-linter
-	Install Haskell-ide-enginer through the terminal
```
stack ghc -- --version
cd ..
git clone https://github.com/haskell/haskell-ide-engine --recurse-submodules
cd haskell-ide-engine
o	stack ./install.hs hie-8.6.4
stack ./install.hs build-doc-8.6.4
```
-	Install Haskell Language Server VS Code extension

## Hello World
-	Go to Lib.hs file in the src folder in the project you created above. Change the default string to ‘Hello World’ and run the code



Names in Haskell must satisfy the following simple rules:
•	Types and typeclasses must start with an uppercase letter
•	Functions and variables must start with a lowercase letter
•	Top-level operator functions must start with any allowed symbol except for :
•	Constructors as operators must start with :
These rules are in the specifications and therefore checked by the compiler. 

Additionally, functions follow the lowerCamelCase style and types follow the UpperCamelCase style. There are just conventional standards, but using distinct styles doesn’t lead to a compiler error. 

The Haskell Prelude contains predefined classes, types, and functions that are implicitly imported into every Haskell program.

-	Booleans
  o	The boolean type Bool is an enumeration. The basic boolean functions are && (and), || (or), and not. The name otherwise is defined as True to make guarded expressions more readable.
-	Characters and Strings
  o	The character type Char is an enumeration whose values represent Unicode characters
  o	A string is a list of characters
      type  String  =  [Char]
-	Lists
  o	Lists are an algebraic datatype of two constructors, although with special syntax. The first constructor is the null list, written `[]' ("nil"), and the second is `:' ("cons"). 
-	Tuples
  o	Tuples are algebraic datatypes with special syntax with each tuple having a single constructor
  o	The constructor for a tuple is written by omitting the expressions surrounding the commas; thus (x,y) and (,) x y produce the same value. The same holds for tuple type constructors; thus, (Int,Bool,Int) and (,,) Int Bool Int denote the same type.
-	The Unit Datatype
  o	Functions are an abstract type: no constructors directly create functional values. The following simple functions are found in the Prelude: id, const, (.), flip, ($), and until.
-	The IO and IOError Types
  o	The IO type serves as a tag for operations (actions) that interact with the outside world. The IO type is abstract: no constructors are visible to the user. IO is an instance of the Monad and Functor classes. 
-	Other Types
  o	The Maybe type is an instance of classes Functor, Monad, and MonadPlus. The Ordering type is used by compare in the class Ord. The functions maybe and either are found in the Prelude.

Conversion between numerical types in Haskell must be done explicitly as can be seen in the data-types code. This is unlike many traditional languages (such as C or Java) that automatically coerce between numerical types.




References
- https://www.haskell.org/
- https://wiki.haskell.org/Keywords
- https://www.compsuccess.com/is-haskell-dynamically-typed/
- http://www2.informatik.uni-freiburg.de/~thiemann/haskell/haskell98-report-html/basic.html


