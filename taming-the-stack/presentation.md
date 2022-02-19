class: center, middle

# Taming the Stack

## _in PureScript_

---

name: stack-safety

# Stack Safety

---

template: stack-safety

* PureScript lacks loops.

--
* Instead, we use recursion.

--
* Recursive calls take up stack space.

--
* Stack overflows abound.

---
template: stack-safety

* Tail-call optimization (TCO) turns first-order, self-recursion in tail-position to a JavaScript loop.

--
* TCO is a static transformation.

--
* General tail-call elimination (TCE) is a runtime property.

---

template: stack-safety

```haskell
data List a = Nil | Cons a (List a)

sum :: List Int -> Int
sum bin = case bin of
  Cons x xs ->
    x + sum xs
  Nil ->
    0
```

---

template: stack-safety

```haskell
data List a = Nil | Cons a (List a)

sum :: List Int -> Int
sum bin = case bin of
  Cons x xs ->
*   x + sum xs
  Nil ->
    0
```

_Not in tail-position!_

---

template: stack-safety

```haskell
data List a = Nil | Cons a (List a)

sum :: List Int -> Int
sum = (\bin -> go bin 0)
  where
  go bin acc = case bin of
    Cons x xs ->
      go xs (acc + x)
    Nil ->
      acc
```

---

template: stack-safety

```haskell
data List a = Nil | Cons a (List a)

sum :: List Int -> Int
sum = (\bin -> go bin 0)
  where
  go bin acc = case bin of
    Cons x xs ->
*     go xs (acc + x)
    Nil ->
      acc
```

_Tail-position!_

---

template: stack-safety

* Any first-order, non-tail-position recursion can be mechanically transformed into tail-recursion.

--
* Often requires trading stack for heap allocation.

---

# An Example

--
```haskell
data Bin a
  = Tip
  | Branch (Bin a) a (Bin a)
```

--
```haskell
mapBin :: forall a b. (a -> b) -> Bin a -> Bin b
mapBin f bin = case bin of
  Tip ->
    Tip
  Branch l a r ->
    Branch (mapBin f l) (f a) (mapBin f r)
```

---

# An Example

```haskell
data Bin a
  = Tip
  | Branch (Bin a) a (Bin a)
```

```haskell
mapBin :: forall a b. (a -> b) -> Bin a -> Bin b
mapBin f bin = case bin of
  Tip ->
    Tip
  Branch l a r ->
*   Branch (mapBin f l) (f a) (mapBin f r)
```

Two-separate calls, _neither in tail-position_.

---

name: transform

# A Transformation

---

template: transform

```haskell
mapBin :: forall a b. (a -> b) -> Bin a -> Bin b
mapBin f bin = case bin of
  Tip ->
    Tip
  Branch l a r ->
*   Branch (mapBin f l) (f a) (mapBin f r)
```

---

template: transform

```haskell
mapBin :: forall a b. (a -> b) -> Bin a -> Bin b
mapBin f bin = case bin of
  Tip ->
    Tip
  Branch l a r -> do
*   let l' = mapBin f l
*   let a' = f a
*   let r' = mapBin f r
    Branch l' a' r'
```

* Move function arguments into bindings.

--
* Makes evaluation order and recursion position obvious.

---

template: transform

```haskell
mapBin :: forall a b. (a -> b) -> Bin a -> Bin b
*mapBin = (\f bin -> go f bin identity) -- Identity cont.
  where
  go f bin cont = case bin of
    Tip ->
      cont Tip
    Branch l a r ->
*     go f l \l' -> do -- Lhs cont.
        let a' = f a
*       go f r \r' -> -- Rhs cont.
          cont (Branch l' a' r')
```

* Convert to continuation-passing (callbacks).

--
* The continuation is the accumulator.

---

template: transform

```haskell
mapBin :: forall a b. (a -> b) -> Bin a -> Bin b
*mapBin = (\f bin -> go f bin identity) -- Identity cont.
  where
  go f bin cont = case bin of
    Tip ->
      cont Tip
    Branch l a r ->
      go f l (contLhs f a r cont)

* contLhs f a r cont = \l' -> do -- Lhs cont.
    let a' = f a
    go f r (contRhs l' a' cont)

* contRhs l' a' cont = \r' -> -- Rhs cont.
    cont (Branch l' a' r')
```

* Move callbacks into bindings.

--
* Makes closures explicit.

---

template: transform

```haskell
data MapAccum a b
* = ContLhs (a -> b) a (Bin a) (MapAccum a b)
* | ContRhs (Bin b) b (MapAccum a b)
* | ContIdentity
```

* Turn closures into a data type.

---

template: transform

```haskell
mapBin :: forall a b. (a -> b) -> Bin a -> Bin b
mapBin = (\f bin -> go f bin ContIdentity)
  where
  go f bin cont = case bin of
    Tip ->
      eval cont Tip
    Branch l a r ->
      go f l (ContLhs f a r cont)

* eval cont bin = case cont of
*   ContLhs f a r next -> do
*     let a' = f a
*     go f r (ContRhs bin a' next)
*   ContRhs l' a' next ->
*     eval next (Branch l' a' bin)
*   ContIdentity ->
*     bin
```

* Turn callback bindings into eval/case.

---

template: transform

```haskell
data MapCall a b
* = MapGo (a -> b) (Bin a) (MapAccum a b)
* | MapEval (MapAccum a b) (Bin b)
```

* Turn mutually-recursive go/eval calls into a data type.

---

template: transform

```haskell
mapBin :: forall a b. (a -> b) -> Bin a -> Bin b
mapBin = (\f bin -> go (MapGo f bin ContIdentity))
  where
  go call = case call of
*   MapGo f bin cont -> case bin of
      Tip ->
        go (MapEval cont Tip)
      Branch l a r ->
        go (MapGo f l (ContLhs f a r cont))
*   MapEval cont bin -> case cont of
      ContLhs f a r next -> do
        let a' = f a
        go (MapGo f r (ContRhs bin a' next))
      ContRhs l' a' next ->
        go (MapEval next (Branch l' a' bin))
      ContIdentity ->
        bin
```

* Turn calls into case.

---

template: transform

```haskell
mapBin :: forall a b. (a -> b) -> Bin a -> Bin b
mapBin = (\f bin -> go (MapGo f bin ContIdentity))
  where
  go call = case call of
    MapGo f bin cont -> case bin of
      Tip ->
*       go (MapEval cont Tip)
      Branch l a r ->
*       go (MapGo f l (ContLhs f a r cont))
    MapEval cont bin -> case cont of
      ContLhs f a r next -> do
        let a' = f a
*       go (MapGo f r (ContRhs bin a' next))
      ContRhs l' a' next ->
*       go (MapEval next (Bin l' a' bin))
      ContIdentity ->
        bin
```

* All recursive calls in first-order tail-position.

---

class: center, middle

 # Let's keep going

---

# TailRec

```haskell
mapBin :: forall a b. (a -> b) -> Bin a -> Bin b
*mapBin = (\f bin -> tailRec go (MapGo f bin ContIdentity))
  where
  go call = case call of
    MapGo f bin cont -> case bin of
      Tip ->
*       Loop (MapEval cont Tip)
      Branch l a r ->
*       Loop (MapGo f l (ContLhs f a r cont))
    MapEval cont bin -> case cont of
      ContLhs f a r next -> do
        let a' = f a
*       Loop (MapGo f r (ContRhs bin a' next))
      ContRhs l' a' next ->
*       Loop (MapEval next (Bin l' a' bin))
      ContIdentity ->
*       Done bin
```

* `tailRec` is tail-recursive by construction.

---

name: gfp

# Greatest Fixpoint

---

template: gfp

```haskell
step :: forall a b. MapCall a b -> MapCall a b
step call = case call of
  MapGo f bin cont -> case bin of
    Tip ->
      MapEval cont Tip
    Branch l a r ->
      MapGo f l (ContLhs f a r cont)
  MapEval cont bin -> case cont of
    ContLhs f a r next -> do
      let a' = f a
      MapGo f r (ContRhs bin a' next)
    ContRhs l' a' next ->
      MapEval next (Bin l' a' bin)
    ContIdentity ->
      MapEval ContIdentity bin
```

---

template: gfp

```haskell
eval :: forall a b. MapCall a b -> Bin b
eval call = case step call of
  MapEval ContIdentity result ->
    result
  next ->
    eval next
```

* Small-step semantics.

---

template: gfp

```haskell
data Step a b
  = Loop a
  | Done b
```

---

template: gfp

```haskell
data Either a b
* = Left a  -- Loop
* | Right b -- Done
```

---

template: gfp

```haskell
data Either a b
* = Left a  -- Done
* | Right b -- Loop
```

--
* Flip the meaning.

---

template: gfp

```haskell
import Data.Functor.Nu (Nu, observe)

fix :: forall b. Nu (Either b) -> b
fix nu = case observe nu of
  Left ret ->
    ret
  Right nu' ->
    fix nu'
```

---

template: gfp

```haskell
import Data.Functor.Nu (unfold)

eval' :: forall a b. MapCall a b -> Nu (Either (Bin b))
eval' = (\call -> unfold call (check <<< step))
  where
  check = case _ of
    MapEval ContIdentity result ->
      Left result
    next ->
      Right next

eval :: forall a b. MapCall a b -> Bin b
eval = fix <<< eval'
```

---

name: lfp

# Least fixpoint

---

template: lfp

```haskell
type Nu (Either b) = exists a. Tuple (a -> (Either b) a) a
```

--
```haskell
type Nu f = exists a. Tuple (a -> f a) a
```

--
* Existentials can be eliminated through closure isomorphism

---

template: lfp

```haskell
type CanShow = exists a. Tuple (a -> String) a
```

--
```haskell
type CanShow =
  forall r. (forall a. (a -> String) -> a -> r) -> r
```

--
```haskell
box :: CanShow
box = \k -> k show 42

elim :: String
elim = box \toString val -> toString val
```

--
```haskell
type CanShow = Unit -> String
```

---

template: lfp

--
```haskell
type Nu f = exists a. Tuple (a -> f a) a
```

--
```haskell
type Wat f = Unit -> f ???
```

--
```haskell
type Wat f = mu r. Unit -> f r -- Fixpoint quantification
```

--
```haskell
data Mu f = In (Unit -> f (Mu f))
```

--
```haskell
data Mu f = In (f (Mu f))
type DelayedMu f = Mu (Compose (Function Unit) f)
```

---

template: lfp

```haskell
import Data.Functor.Compose (Compose(..))
import Data.Functor.Mu (Mu, unroll)

type DelayedMu f = Mu (Compose (Function Unit) f)

fix :: forall b. DelayedMu (Either b) -> b
fix mu = case unroll mu of
  Compose thunk ->
    case thunk unit of
      Left result ->
        result
      Right mu' ->
        fix mu'
```

---

template: lfp

```haskell
mapBin :: forall a b. (a -> b) -> Bin a -> Bin b
*mapBin = (\f bin -> fix (go (MapGo f bin ContIdentity)))
  where
* go call = roll $ Compose $ \_ -> case call of
    MapGo f bin next ->
      case bin of
        Tip ->
          Right (go (MapEval next Tip))
        Branch l a r ->
          Right (go (MapGo f l (ContLhs f a r next)))
    MapEval cont bin ->
      case cont of
        ContLhs f a r next -> do
          let a' = f a
          Right (go (MapGo f r (ContRhs bin a' next)))
        ContRhs l' a' next ->
          Right (go (MapEval next (Branch l' a' bin)))
        ContIdentity ->
          Left bin
```

---

template: lfp

```haskell
mapBin :: forall a b. (a -> b) -> Bin a -> Bin b
mapBin = (\f bin -> fix (go f bin ContIdentity))
  where
  go f bin cont = roll $ Compose $ \_ -> case bin of
    Tip ->
      eval cont Tip
    Branch l a r ->
      Right (go f l (ContLhs f a r cont))

  eval cont bin = case cont of
    ContLhs f a r next -> do
      let a' = f a
      Right (go f r (ContRhs bin a' next))
    ContRhs l' a' next ->
      eval next (Branch l' a' bin)
    ContIdentity ->
      Left bin
```

---

template: lfp

```haskell
mapBin :: forall a b. (a -> b) -> Bin a -> Bin b
*mapBin = (\f bin -> fix (go f bin contId))
  where
  go f bin cont = roll $ Compose $ \_ -> case bin of
    Tip ->
      Right (cont Tip)
    Branch l a r ->
      Right (go f l (contLhs f a r cont))

* contId = roll <<< Compose <<< const <<< Left

* contLhs f a r cont = \l' -> do
    let a' = f a
    go f r (contRhs l' a' cont)

* contRhs l' a' cont = \r' ->
    cont (Branch l' a' r')
```

---

template: lfp

```haskell
mapBin :: forall a b. (a -> b) -> Bin a -> Bin b
*mapBin = (\f bin -> fix (go f bin done))
  where
* go f bin cont = roll $ Compose $ Right \_ -> case bin of
    Tip ->
      cont Tip
    Branch l a r ->
*     go f l \l' -> do
        let a' = f a
*       go f r \r' ->
          cont (Branch l' a' r')

* done = roll <<< Compose <<< const <<< Left
```

---

template: lfp

```haskell
type Rec b = Mu (Compose (Function Unit) (Either b))
```

--
```haskell
data Rec b = Rec (Unit -> Either b (Rec b))
```

--
```haskell
data Rec b
  = Left (Unit -> b)
  | Right (Unit -> Rec b)
```

--
```haskell
data Rec b
  = Done (Unit -> b)
  | Loop (Unit -> Rec b)
```

--
```haskell
data Rec b
  = Done b
  | Loop (Unit -> Rec b)
```

---

template: lfp

```haskell
data Rec b
  = Done b
  | Loop (Function Unit (Rec b))
```

--
```haskell
data Rec' f b
  = Done b
  | Loop (f (Rec' f b))

type Rec a = Rec' (Function Unit) a
```

--
```haskell
data Free f b
  = Pure b
  | Roll (f (Free f b))

type Trampoline a = Free (Function Unit)
```

---

template: lfp

```haskell
import Control.Monad.Trampoline (Trampoline, runTrampoline, delay)

*suspend :: forall a. (Unit -> Trampoline a) -> Trampoline a
*suspend = join <<< delay

mapBin :: forall a b. (a -> b) -> Bin a -> Bin b
*mapBin = (\f bin -> runTrampoline (go f bin))
  where
  go :: (a -> b) -> Bin a -> Trampoline (Bin b)
  go f bin = suspend \_ -> case bin of
    Tip ->
      pure Tip
    Branch l a r -> do
*     l' <- go f l
*     let a' = f a
*     r' <- go f r
      pure (Branch l' a' r')
```

---

template: lfp

```haskell
mapBin :: forall a b. (a -> b) -> Bin a -> Bin b
mapBin = (\f bin -> runTrampoline (go f bin))
  where
  go f bin = suspend \_ -> case bin of
    Tip ->
      pure Tip
    Branch l a r ->
      Branch <$> go f l <*> pure (f a) <*> go f r
```

---

class: center, middle

# Thanks!
