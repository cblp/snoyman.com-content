---
title: Async exception handling in Haskell
---

## Async exception handling in Haskell

* Michael Snoyman
* VP of Engineering
* FP Complete webinar
* April 11, 2018

<div style="text-align:center">
<div><img src="/static/fpcomplete-logo.png" style="border:0;margin:0"></div>
</div>

---

## From the beginning

* Many languages have synchronous exceptions
* Double-edged sword
    * Arguably easier to write correct code
    * Can lead to lack of resource cleanup
* GHC Haskell has asynchronous exceptions too
    * Let's just say "Haskell," you know what I mean now
* Gotta cover exceptions to get to async exceptions

----

## What we'll cover today

* Defining different types of exceptions
* Correct synchronous exception handling
* How bottom values play in
* Basics of async exceptions
* Masking and uninterruptible masking
* Helper libraries
* Some more complex examples

Lots of ground to cover before we talk about async stuff!

----

## Fear is the mind-killer

* Async exceptions _are_ tricky
* They aren't nearly as terrifying as lore makes them out as
* Usually: use the right helper library, everything's good

----

## Are exceptions good or bad?

* Not our topic today!
* Lots of healthy debate inside and outside the Haskell community
* However: runtime exceptions are the reality of GHC Haskell today
* Whether you like it or not: need to deal with it

----

## Teaser

Goal for this talk: you should see multiple reasons I call this
function `badRace`:

```haskell
badRace :: IO a -> IO b -> IO (Either a b)
badRace ioa iob = do
  mvar <- newEmptyMVar
  tida <- forkIO $ ioa >>= putMVar mvar . Left
  tidb <- forkIO $ iob >>= putMVar mvar . Right
  res <- takeMVar mvar
  killThread tida
  killThread tidb
  return res
```

---

## Motivating example

* Most complexity around scarce resource handling
* File handling great example, we'll use it
    * Open the file, may fail
    * Interact with the file handle, may fail
    * Close the file handle regardless
    * File descriptor are scarce!
* Start without any exceptions, build up from there
* Slight detour though...

----

## Pure code

* Cannot catch exceptions in pure code
* Makes sense: no resource allocation in pure code
* Except...
* Can throw from pure code &#x1f641;
* Can use `unsafePerformIO` for allocations
* Memory can be allocated implicitly
    * Not a contradiction! Memory ain't scarce
* Technically can use `unsafePerformIO` to catch

Overall: our focus is on non-pure, `IO` code. Slight reference to
transformers later.

----

## The land of no exceptions

Haskell without any runtime exceptions (great rejoicing in the land)

```haskell
openFile :: FilePath -> IOMode
         -> IO (Either IOException Handle)
hClose :: Handle -> IO () -- assume it can never fail
usesFileHandle :: Handle -> IO (Either IOException MyResult)

myFunc :: FilePath -> IO (Either IOException MyResult)
myFunc fp = do
  ehandle <- openFile fp ReadMode
  case ehandle of
    Left e -> return (Left e)
    Right handle -> do
      eres <- usesFileHandle handle
      hClose handle
      return eres
```

----

## Land of synchronous exceptions

Add two new primitives for synchronous exceptions

```haskell
throwIO :: IOException -> IO a
try :: IO a -> IO (Either IOException a)
```

__Synchronous exceptions are exceptions which are generated directly
from the `IO` actions you are calling.__

----

## Rewrite our function

```haskell
openFile :: FilePath -> IOMode -> IO Handle
hClose :: Handle -> IO ()
usesFileHandle :: Handle -> IO MyResult

myFunc :: FilePath -> IO MyResult
myFunc fp = do
  handle <- openFile fp ReadMode
  res <- usesFileHandle handle
  hClose handle
  return res
```

* Code is shorter
* Can't tell whether `openFile` and `hClose` can fail
* No need to pattern match on `openFile` result
* But wait! What if `usesFileHandle` throws an exception?

----

## Try and throw

Fix it!

```haskell
myFunc :: FilePath -> IO MyResult
myFunc fp = do
  handle <- openFile fp ReadMode
  eres <- try (usesFileHandle handle)
  hClose handle
  case eres of
    Left e -> throwIO e
    Right res -> return res
```

(Synchronous) exception safe!

----

## Capture the pattern

```haskell
withFile :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
withFile fp mode inner = do
  handle <- openFile fp mode
  eres <- try (inner handle)
  hClose handle
  case eres of
    Left e -> throwIO e
    Right res -> return res

myFunc :: FilePath -> IO MyResult
myFunc fp = withFile fp ReadMode usesFileHandle
```

__General principle__ Avoid using functions which only allocate or
only clean up whenever possible.

__Question__ What if `cleanup` throws an exception?

---

## Extensible exceptions

* We assumed `IOException` above
* GHC has OO-style extensibility, like Java
    * Please don't vomit

```haskell
data SomeException = forall e . Exception e => SomeException e

class (Typeable e, Show e) => Exception e where
  toException   :: e -> SomeException
  fromException :: SomeException -> Maybe e

throwIO :: Exception e => e -> IO a
try :: Exception e => IO a -> IO (Either e a)
```

----

## Example exception, no hierarchy

```haskell
data InvalidInput = InvalidInput String
  deriving (Show, Typeable)
instance Exception InvalidInput where
  toException ii = SomeException ii
  fromException (SomeException e) = cast e -- part of Typeable
```

`toException` and `fromException` have defaults, so...

```haskell
instance Exception InvalidInput
```

----

## Example of hierarchy

```haskell
data MyAppException
  = InvalidInput String
  | SomethingElse SomeException
  deriving (Show, Typeable)
instance Exception MyAppException
```

```haskell
data SubException = NetworkFailure String
  deriving (Show, Typeable)
instance Exception SubException where
  toException = toException . SomethingElse . SomeException
  fromException se = do
    SomethingElse (SomeException e) <- fromException se
    cast e
```

```haskell
main :: IO ()
main = do
  e <- try $ throwIO $ NetworkFailure "Hello there"
  print (e :: Either SomeException ())
```

---

## Exception in pure code

* Why call it `throwIO` and not `throw`?

```haskell
throw :: Exception e => e -> a
```

* Not an async exception!
* I call them __impure exceptions__
* Create bottom values

----

## Creating impure exceptions

* Using the `throw` function directly
* Using a function which calls `throw`, like `error`
* Using partial functions like `head`
* Incomplete pattern matches (GHC automatically inserts the equivalent
  of a call to `throw`)
* Creating infinite loops in pure code, where GHC's runtime _may_
  detect the infinite loop and throw a runtime exception

----

## Preaching to the choir

* Partiality is bad, m'kay?
* Avoid creating these impure exceptions

----

## Challenge: what's the output?

```haskell
import Control.Exception
import Data.Typeable

data Dummy = Dummy
  deriving (Show, Typeable)
instance Exception Dummy

printer :: IO (Either Dummy ()) -> IO ()
printer x = x >>= print
```

```haskell
main :: IO ()
main = do
  printer $ try $ throwIO Dummy
  printer $ try $ throw Dummy
  printer $ try $ evaluate $ throw Dummy
  printer $ try $ return $! throw Dummy
  printer $ try $ return $ throw Dummy
```

----

## Case 1

```
printer $ try $ throwIO Dummy
Left Dummy
```

We're using proper runtime exceptions via `throwIO`, and therefore
`Dummy` is thrown immediately as a runtime exception. Then `try` is
able to catch it, and all works out well.

----

## Case 2

```
printer $ try $ throw Dummy
Left Dummy
```

We generate a value of type `IO ()` which, when evaluated, will throw
a `Dummy` value. Passing this value to `try` forces it immediately,
causing the runtime exception to be thrown. The result ends up being
identical to using `throwIO`.

----

## Case 3

```
printer $ try $ evaluate $ throw Dummy
Left Dummy
```

`throw Dummy` has type `()`. The `evaluate` function then forces
evaluation of that value, which causes the `Dummy` exception to be
thrown.

----

## Case 4

```
printer $ try $ return $! throw Dummy
Left Dummy
```

This is almost identical; it uses `$!`, which under the surface uses
`seq`, to force evaluation. We're not going to dive into the
difference between `evaluate` and `seq` today.

----

## Case 5

```
printer $ try $ return $ throw Dummy
Right Main.hs: Dummy
```

* Odd man out
* Create thunk with `throw Dummy` of type `()`
* `return` wraps it into `IO ()`
* `try` forces evaluation of `IO ()`, which doesn't force evaluation of the `()`
* End up with value of type `Either Dummy ()`
* Equivalent to `Right (throw Dummy)`
* `printer` tries to print it, forces `throw Dummy`, causes crash

----

## What's the upshot?

* Not passing judgement, but: don't use `throw` and `error`
    * If you use exceptions, use `throwIO`
* Pure exceptions seem to appear at "random"
* But the trigger for it getting thrown is always local
    * Forcing evaluation inside `IO`
* Therefore, by our definition, impure exceptions _are_ synchronous
  exceptions
* We'll treat them as such, but mostly just ignore them, because...

----

## Impure exceptions are irrelevant

Who cares if `inner` returns a partial/bottom value?

```haskell
withFile :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
withFile fp mode inner = do
  handle <- openFile fp mode
  eres <- try (inner handle)
  hClose handle
  case eres of
    Left e -> throwIO e
    Right res -> return res
```

We never evaluate it in `withFile`, so it doesn't break anything

---

## Motivating async exceptions
