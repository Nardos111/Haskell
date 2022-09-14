# Haskell

## History

The tutorial on this github repository is on the programming language Haskell. The first version, Haskell 1.0 was introduced in 1990. It was created in Portland, Oregon. It was written to solve the problem of other existing functional languages exhibited at the time. The existing languages didn’t have a stable foundation for real applications development. Haskell is mainly used for back-end programming in web development, CLI applications and for programming language research. In the past it’s been used in project like Hasura(an open-source GraphQL engine), Github’s Semantic(command-line tool for parsing, analyzing, and comparing source code), and by Meta in Fighting spam project. 
More information about Haskell can be found [here](https://www.haskell.org/)

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
