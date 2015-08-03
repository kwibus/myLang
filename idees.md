
# newtype

alias:
 : same as type(Haskell) define(c)

newtype:
 : same as alias but you can't reuse functions from original type, but is preserves typeclass but the can be overload

data:
 : same as in Haskell, make a new data type, data type with one term have the same representation as newtype and alias
   (data Any = Any Bool
    Any undefined === erro because stric semanticks  ) 
# polymorfisme
    explicit forrall -> dynamic?
    remove polymorfisme to concrete type  (monomorfic)?

# void type (c compatible )

# Language
lengte x = go x 0 {
 go (x : xs) n = go xs (n+x)
}
quiqsort : !A e -> !A e
quiqsort A = newA{
    pivot = head A
    (small , big) = split A (<pivot)
    tuple = (quiqsort small, quiqsort big)
    newA = merge tuple
 }

## keys
###  pro/con
row polymorfisme
replace tagged union with pointer and a contant with
## regions
```haskell
newregion :: region
newpointer :: region -> a -> ref a
lookup / !  :: region -> ref a -> a
remove :: region-> a  -> ()
```
if region is out of scope delete data (linear)
if region is used keep / region me be reused (not linear)
if ref out of scope keep pointed data
if ref is used keep
region is phantom type, it exist to verify (data flow analyse) correct use linear data ???
a region may never be a return value of be put in a data structure so linear data is linear for the caller


## every typeclass -> record , record -> typeclass
the separation of typeclass and record is a separation between logic,interface and data. This has his benefits.
but there are some problems:
* data types are closed, if you need a data type thats similar you can`t reuse exsisting functions, exsisting solution are inhertance
* typeclass don`t allow you to pattern match

But what if you could pattern match with typeclass.
implicit(could also be explicit) ever data type give rise to a typeclass
and type clause support a new statement type. type defines a new kind of pattern match
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

if every typeclass give a record
but implementation is hidden you cant pattern match
but you can do with it what is defined in the typeclass (existential types)


### example

``` haskell
class Show where
    show :: a -> String
list :: Show a => Show [a] -- is typeclass still necesari
list = [Show 1 , Show 'c'] -- Show is constructer
```
## syntax sugar modify record

Now car{name = t } =  car{name = t+1}
car = car@name + 1

## extend data types / subtyping  / row polymorfisme
### example

with product types

``` haskell
data P1 = P Int Int
data P2 = P .. Color

distance  :: P1 -> P1 -> Double
distance ( P 1 2 Red ) (P 1 3 blue ) -- correct

move:: P1 p => p -> P1 -> p
move ( P 1 2 Red ) (P 1 3  ) == P 2 5 Red-- correct

h :: P1 p , show p => p -> (String,Int)
h p@P(i,_) = (show p , i)
```
with sum types
``` haskell
data Exp  = Val Int | Plus Exp Exp
data Exp2 = Exp +| Min Exp Exp

eval2 :: Exp2 -> Int
eval2 (Val i) = i
eval2 (plus e1 e2) = eval2 e1 + eval2 e2
eval2 (Min e1 e2 ) = eval2 e1 - eval2 e2


eval3 (Min e1 e2 ) = eval3 e1 - eval3 e2
eval3 t = eval1  t -- fail recutions

eval2 (e::Exp2) -- correct
eval2 (e:: Exp) -- incorect

eval1 :: Exp -> Int
eval1 (Val i) = i
eval1 (plus e1 e2) = eval1 e1 + eval1 e2
eval1 e = e

eval1 (Min (Val 1) (Val ) :: Exp2) == Min (Val 1) (Val ) -- correct
eval1 (e:: Exp) -- correct
```

with product and sum

``` haskell
data A1 = A Int Int
        | B Int
data A2 = A .. Color
        +| C
```
###  pro/con
con
 * make type system more complicated
 * makes searching for fitting  function more difficult, you can`t see from te type signature P2 that it can be applied inplace of P1
 * more ways to do things
 * some opptimazations are inposiable ore harder ?
 * sum type generates error when there no default action (eval1 )
 * you need open functions to use this with sum types efective
pro

 * removes boilerplate
 * if structere are past by refrence than ther is no need for code duplications in funcion call.
    because the size is know at the location were the variable is defined
    but the size is not need in funcioncall becaus it only need offset and because P2 extend P1 those are the same

# linear/unique types

Variable withe one active reference

Unique variables can save ley deleted when out of scope

sharing linear type ?

Linear types express via kind
Bottom kind <: unique kind <: super kind
    <: multi kind

const linear ?

Linear multi ?

>bottom

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

## philosophy

> What is better default for data linear or const?
linear data should be used with data that chages a lot and is not shared
at initialization all data is not shared.
data with a short life is not often shared.
you can alway convert linear to shared by freezing it.
the language should promote to create and modify the data before sharing/freezing it.
If its frozen is should not create mush  modified copies.
Alternatively you should make one mutable copy modifie and freeze


##syntax linear types

* haskell style

    go`s linear type out of scope when used left hand sided
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
id :: a -> a
id x = x

id :: a -> !a
id x = deepcopy x

double a -> (&a,&a)
double a = (a, a)

double a -> (a,!a)
double a = (a, deepcopy a)

* linear arrow  -0   ?

## multi type (non unique)

* delete out of scope ;  whole program analyse ;
* gc
* combination

## explixt box/unboxed

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
type checks

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

## optimization
* use nullpointer at list and maybe

* remove unneeded patter matches

```haskell
  f = g Just 1 -- jump to @

  g Nothing = 1
  g Just _= @ 2
```
* remove unneeded polymofisme

```haskell
    let a = a    ===  \a -> a
```

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

## typeclass

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
### how you can implement OO in this

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

### possible inplemtations

``` haskell
#### pointer in record
class Ord Eq a => Ord a

data  Ord int =  Ord
    { Eq = defaultEq
    , (>)  = ..
      ..
    }
```
pro

* space efficient
con

* pointer indrection
* cache unfriendly

#### flat record
``` haskell
data  Ord int =  Ord
    { (==) = ..
      ..
    , (>)  = ..
      ..
    }
```
pro

* cache friendly
* looks like  OO ??

con

* space inefficient

#### multi explicit record args
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

# type error messages

to many args
 get all args 

infinity typ
\a.a a
get location of bothe 
outer is in first application
inner can be storred 


 \a.a a
---------------------------------------------------
can`t construct infinity type for a in 
    \a.|a| |a| 
Typeof (a) :=:  Typeof(a) -> b / a ~ a -> b
---------------------------------------------------

type mismatch
    mismatch function defenition and use 

    let f = (+) in f True 
    ---------------------------------------------------
    f is applied to the wrong kind of variable
    at pos
        let f = (+) in f True 
        f :: Double -> Double -> Double
        True  :: Bool
    ---------------------------------------------------

    ---------------------------------------------------
1:1: TypeError
    (\a.\b. a+b) expects a Double as it's 2 argument
    but it's  (\a.a) :: a->a
    in "(\a.\b. a+b) 1.0 (\a.a)"
      (\a.\b. a+b) ::Double -> Double -> Double
                      #1        #2       #result
       #1 1.0    :: Double 
       #2 (\a.a) :: a -> a b 

    ---------------------------------------------------



    mismatch in use's ( \a. (a 1 , a True ))

push enter pull appley 

