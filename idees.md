# polymorfisme
    explicit forrall -> dynamic
    remove polymorfisme to concrete type 
# linear
    linear Vallue ; can be used ones as linear but many read only 
    linear closure ; can be used ones but no read 

# void type (c compatible )
# Language
lengte x = go x 0 {
 go (x:xs) n = go xs (n+x)
}
quiqsort : !A e -> !A e
quiqsort A = newA{
    pivot = head A
    (small , big) = split A (<pivot)
    tuple = (quiqsort small, quiqsort big)
    newA = merge tuple
 } 

## keys
row polymorfisme
replace taged union with pointer and a consant with 
## regions
```haskell
newregion :: region
newpointer :: region -> a -> ref a
lookup / !  :: region -> ref a -> a
remove :: region-> a  -> ()
```
if region is out of scope delte data (linear)
if region is used keep / region me be reused (not linear)
if ref outscope keep pointed data
if ref is used keep
region is fantom type, it exsit to verify (data flow analyse) correct use linear data ???
a region may never be a return value of be put in a data structure so linear data is linear for the caller  


## every typeclasse -> record , record -> typeclass
the separtion of typeclass and record is a separtion between logic,interface and data. This has his benefits. 
but there are some prombles:
* data types are closed, if you need a data type thats similar you can`t reuse exsisting functions, exsisting solution are inhertance
* typeclass don`t allow you to pattern match 

But what if you could pattern match with typeclass.
implicit(could also be explicit) ever data type give rise to a typeclass
and type classe support a new statement type. type defines a new kind of pattern match 
### example
``` haskell
data LambdaTerm = Lambda String LambdaTerm
                  | Appl LambdaTerm LambdaTerm
                  | Var String

class Lambdaterm a where 
    type :: a -> Lambdaterm
data TypedLambdaTerm = TLambdaterm  Sting type Lambdaterm 
                       | TAppl TLambdaTerm TLambdaTerm
                       | TVar String Type

```
this does not work with recursive types

typeclass -> record 

if every typclass give a record
but implementation is hidden you cant patternmatch 
but you can do with it what is diefined in the typeclasse(existenctial types)


### example 

``` haskell
class Show where
    show :: a -> String
list :: Show a => Show [a] -- is typeclass still necesari
list = [Show 1 , Show 'c'] -- Show is constructer
```

## syntax sugar modify record 

Now car{name = t } =  car{name = t+1}
car = car@name +1 

## extend data types / subtyping
### example

``` haskell
data Bnf a = Terminal a | NonTerminal index [bnf] 
    -- define bnf rule of terminal and 
    -- noterminal that have unique index and  are made of  other symbols
    -- bad exampel
data NamedBnf = bnf , extend  Nonterminal withe Name
or 
data NameBnf = Terminal a | Nonterminal index Name [bnf]

getTerminals ::  Bnf a -> [a]
-- getterminals also acept also NameBnf a

pprint ::Show a => Bnf a -> String
pprint ::  Show a =>  NameBnf a -> String
-- overloading
```

###  pro/con
con 

* overloading is already implement with typclase
* can already be done  withe previous name typclasse  extension

pro

* removes boilerplate
* suptyping is know sometime usefol 
* 
## linear/unique types

Variable withe one active reference

Unique variables can save ley deleted when out of scope

sharing linear type ?

Linear types express via kind
Bottom kind <: unique kind <: super kind
    <: multi kind

const linear ?

Linear multi ?

>bottom
>
>const unique  <: unique <: super
>
>protect multi <: multi  <:

Operations
* Supper : read create
* linear  : assignment  delete?  copy ? freeze  (protect multi cannot save ley deleted)
* multi  : ref

kinds init
* default const linear
* stack only variable can be super

### philosophy
what is better default for data linear or const?
linear data should be used with data that chages a lot and is not shared
at initialization all data is not shared.
data with a short life is not often shared.
you can alway convert linear to shared by freezing it.
the language should promote to create and modify the data before sharing/freezing it.
If its frozen is should not create mush  modified copies.
Alternatively you should make one mutable copy modifie and freeze
 

##syntax linear types

* haskell style

    go`s linar type out of scope when used left hand sided
``` haskell
inc :: !Int -> !Int
let y = inc x in x + x
```
correct
``` haskell
inc :: !Int -> !Int
let y = inc x + inc x
```
incorrect (x used twice in left side)

* c style statement
``` haskell
inc :: Int -> (
```
``` haskell
; :: Unit -> a -> a
```
``` haskell
f:: !Int -> !Int
f =
    inc x;
    x  + x
```
* linear arrow  -0   ?

## multi type (nonuniqe)

* delete out of scope ;  whole program analyse ;
* gc
* combination

## explixt box/unbox

Disallow list a =  [] | car a  (list a)

Allow list a =  [] | car a  box(list a)

## coroutine

### syntax

* Haskell  style
```haskell
numbers :: [Int] (overload list syntax)
numbers =
     let i = 1
         next x = inc x
     in yield (i,next , i)
```
* c style
``` haskell
numbers :: [Int]
numbers =
    let f x = yield x ;
        f $ inc x
    in  f 1
```
(!!wrong  x used twice)

### implementation

* implementation via closure
* multi stake
* nested yield wel/niet ?

## info

### example

``` haskell
arrayindex :: (Array a i):size(N) -> Int:<N -> Int

size :: Array a i -> Int

let f = let
    a :: (Array a i)::size(4)
    a = newarray [1,2,3,4]
    b = arrayindex a 3  -- b = 3
```
typechecks

``` haskell
fromlist :: [a] -> Array a e

let f list  = let
    a = fromlist  list
    b = arrayindex a 3  -- b = 3
```
fails can`t proof  a::size(N), 3\<N

``` haskell
(prommise / asser) :: f@(a->Bool) -> a -> a: f

let f list  = let
    a = fromlist  list
    b = prommise $ arrayindex a 3  -- b = 3
```

add rules
``` haskell
rule a :: <N, N<M -> <M
```
# implementation

## floating

compiler should have a more pricise reppresentation,
and convert to expect type after optimalisations,

## IR

* cps
* ANF

## Closure

* curring
* structure function env
# old

## typeclase

* typeclase is equivalent to record
* instance is instance of record

have to implement

* specific default typclase record
* specific different then default

 syntax

``` haskell
data Show a = Show {show :: a -> String }
default Show Int = Show {show = showit -- predefined }

customShowInt = Show {show = "int"}

print :: Show a => a -> IO()
print = printStr show a

>>> print[customshowint] 1
"int"
```
nested typclase :
* funNeedingOrd[Ord[MyEq]]
* funneedingord[myOrd]

import default record

###  pro/con
con

* mover verbose (can use sugar)
* less obvious
* import alone is sufficient to prevent most haskell prombles

pro

* if all types are know at compile type (not libaries ) implementation can still be inlined.
but if jou want to chose implementation at compile time you can use normal records
* less to implement ???
* you don't have to chose between typclase ore data type
* maybe first step to get nice syntax for (foreall a. show a => [a])
* it becomes more similar to oo
* you can pattern match agent data types
### how you can implement oo in this

``` haskell
data Cat = Cat {some state  of cat}

miauw :: Cat -> String
miauw _ = "miauw"

class Mammal a where 
    milk = a -> Maybe  Int --  how much milk is current produced

instance Mammal cat 

data Mammal a = Mammal{milk :: Maybe Int}

```


### Things to consider

* name clash
* how does a nested typclasse /record look

### possibilty

``` haskell
class Ord Eq a => Ord a

data  Ord int =  Ord
    { Eq = defaultEq
    , (>)  = ..
      ..
    }
```
###  pro/con
pro

* space efficient
con

* pointer indrection
* cache unfriendly


``` haskell
data  Ord int =  Ord
    { (==) = ..
      ..
    , (>)  = ..
      ..
    }
```
###  pro/con
pro

* cache friendly
* looks like  oo ??

con

* space inefficient

``` haskell
data Eq Int = Eq
    { (==) = ..
      ..
    }
data Ord Int = Ord
    { (>)  = ..
      ..
    }
```

###  pro/con
pro

* space efficient

con

* cache unfriendly
* more implicit arguments
## record variable should be scoped

records generate functions
(data Car = Car{name::string} generate function name).
This is gives name clash. Normally you can prevent name clash whith scoped variable names.
But now this would work. Other language solve this with (.) operator. You get mustang.name.
This works but: ugly syntax, inconsistent  with normal functions, how do you get function if you don't have a record (fmap name xs) maybe you could type this (fmap Car.name xs).
a different solution could be overload the function based on type.
You get the equivalent with creating typeclass and instance.
data Car = Car{name::string} generate function name
implies 
class  xxx a where 
    name :: a -> String
instance xxx Car where
    name  = name
problems name  class still occurs with function with different number of arguments.
This still better.
Solution allow functions with same and different type  completely. Code become so less readable.
All this implicit code makes it harder to understand what is happing in the background


