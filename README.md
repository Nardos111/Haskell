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

#### Booleans
  -	The boolean type Bool is an enumeration. The basic boolean functions are && (and), || (or), and not. The name otherwise is defined as True to make guarded expressions more readable.
#### 	Characters and Strings
  -	The character type Char is an enumeration whose values represent Unicode characters
  -	A string is a list of characters
      type  String  =  [Char]
#### Lists
  -	Lists are an algebraic datatype of two constructors, although with special syntax. The first constructor is the null list, written `[]' ("nil"), and the second is `:' ("cons"). 
#### 	Tuples
  -	Tuples are algebraic datatypes with special syntax with each tuple having a single constructor
  - The constructor for a tuple is written by omitting the expressions surrounding the commas; thus (x,y) and (,) x y produce the same value. The same holds for tuple type constructors; thus, (Int,Bool,Int) and (,,) Int Bool Int denote the same type.
#### 	The Unit Datatype
  -	Functions are an abstract type: no constructors directly create functional values. The following simple functions are found in the Prelude: id, const, (.), flip, ($), and until.
#### 	The IO and IOError Types
  -	The IO type serves as a tag for operations (actions) that interact with the outside world. The IO type is abstract: no constructors are visible to the user. IO is an instance of the Monad and Functor classes. 
#### Other Types
  - The Maybe type is an instance of classes Functor, Monad, and MonadPlus. The Ordering type is used by compare in the class Ord. The functions maybe and either are found in the Prelude.

Conversion between numerical types in Haskell must be done explicitly as can be seen in the data-types code. This is unlike many traditional languages (such as C or Java) that automatically coerce between numerical types.

## Control Statements and boolean values

Bool type in Haskell contains the two logical values: ```True``` and ```False```

The following conditional statements are available in Haksell as can be seen in the folder ```control-statements```
- if/else
- if/then/else
- if/elseif/else
- case statements

For ```case statements```, break is not necessary to get out of them.

The ```||``` and ```&&``` operators are control structures and they evaluate the first argument and then the second argument only if needed. 

## Functions 

Haskell has its own functional definition and declaration separately.
- Function declaration consists of the function name and its argument list along with its output.
- Function definition is where you actually define a function.

```
doubleNum :: Int -> Int
doubleNum n = 2*n
```

Here, the first line specifies the type of the function and the second line tells us how the output of doubleNum depends on its input. 

Haskell starts compiling the code from the main method. 

```
main = do 
   putStrLn "The double of the number is:"  
   print(doubleNum 2)    --calling a function 
```

The code will generate the following output −

```
The double of the number is:
4
```

We are not restricted to having single line definitions for functions. We can use multiple definitions combined with implicit **pattern matching**. For instance consider the function:


```
power :: Float -> Int -> Float
power x 0 = 1.0
power x n =  x * (power x (n-1))
```

Here, the first equation is used if the second argument to power is 0. If the second argument is not 0, the first definition does not match, so we proceed to the second definition. When multiple definitions are provided, they are scanned in order from top to bottom. This is usually used with haskell recursive functions


In addition, there is no difference between "pass-by-value" and "pass-by-reference" in Haskell, because it's not possible to assign to a variable in these languages. It's not possible to have changes to a method parameter in and values are immutable.

Haskell use the same lexical scoping as most other languages.

```
z=1
```
Results in a value referenced through z in the global scope, whereas

```
double z = z + z
```

will result in z being lexically scoped to the function double. 


## Loops

-	Haskell doesn’t have built-in functions of loops like other languages. However, as an alternate recursion can be used to simulate different types of loops. Like a regular recursion there needs to be a base condition so that the recursion doesn't get stuck in an infinite loop.


To implement the following **For loop**

```
==> For loop

for (i = 0; i < 10; i + = 2)
{
    do something
}
```
a user defined recursive function can be used.

```
forLoop :: Int -> Int -> Int -> IO()
forLoop loopNum maxLoopNum value =
   if loopNum < maxLoopNum
      then do
         print loopNum
         forLoop (loopNum+value) maxLoopNum value
   else putStrLn "Completed the loop"
```

**For Each**

```
foreach (char c in string) 
{
   num = num+1
}
```

Here we use pattern matching
```
forEach :: String -> Int
forEach [] = 0
forEach string = 1 + forEach string
```

**While loop**

```
While (a%10 !=0) {
  a = a+1
}
```

```
whileLoop :: Int -> IO ()
whileLoop a =
   if a `mod` 10 /=0
   then do
      print a
      whileLoop (a+1)
   else 
      putStrLn "Completed the loop"
```

## Classes and Objects

In Haskell, you can define a ```typeclass``` which is different from an object oriented class. Using the keyword ```class``` we can declare function names and type signatures which can be instantiated elsewhere for a particular data type.

Here:
- **Typeclasses are sets of type**
- **Types are sets of values**
 
Which can be analogous to java:

- **Java: Object → Class → Interface**
- **Haskell: Value → Type → Typeclass**

Typeclasses aren't the only way to define an interface. We can also define a data structure that has the functions as fields.

Haskell doesn’t have inheritance because it doesn’t have objects. It also doesn’t allow you to ```subclass``` a ```type```, but the same effect of inheritance can be obtained by using ```typeclasses```.





References
- https://www.haskell.org/
- https://wiki.haskell.org/Keywords
- https://www.compsuccess.com/is-haskell-dynamically-typed/
- http://www2.informatik.uni-freiburg.de/~thiemann/haskell/haskell98-report-html/basic.html
- https://www.educative.io/answers/loops-in-haskell-using-recursion
- https://www.tutorialspoint.com/haskell/haskell_functions.htm
- https://www.cmi.ac.in/~madhavan/courses/pl2009/lecturenotes/lecture-notes/node70.html
- https://www.youtube.com/watch?v=x3uF7fcQwWE
- https://stackoverflow.com/questions/5414323/does-haskell-support-object-oriented-programming
- https://mmhaskell.com/haskell-data/inheritance




