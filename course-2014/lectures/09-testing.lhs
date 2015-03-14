Testing
=======

CIS 194 Week 09\
30 October 2014

Suggested reading:

-   [RWH Chapter 11: Testing and Quality
    Assurance](http://book.realworldhaskell.org/read/testing-and-quality-assurance.html),
    but note that the QuickCheck library’s interface has evolved a bit
    since this was written
-   [HUnit 1.0 User’s
    Guide](http://www.haskell.org/haskellwiki/HUnit\_1.0\_User’s\_Guide)

> {-# LANGUAGE RankNTypes #-}
> import Test.QuickCheck
> import Test.HUnit
> import Data.List
> import Control.Monad

Unit tests
----------

Let’s say I want to merge two sorted lists.

> -- | Assuming the input lists are sorted, combine the lists into a
> -- sorted output.
> merge1 :: Ord a => [a] -> [a] -> [a]
> merge1 (x:xs) (y:ys)
>   | x < y     = x : merge1 xs ys
>   | otherwise = y : merge1 xs ys
> merge1 _      _      = []

Does this function work as expected? I could run a few tries in GHCi,
but that’s a little unsatisfactory: I have to do the work to think up a
test, but I get to use it only once. Instead, it’s much better to write
the test in my file, and that way I can re-run it every time I update my
merge function.

HUnit and QuickCheck both provide ways of testing our programs. They
both focus on *unit testing*, where one small part of a program is
scrutinized, looking for problems. The idea is that if all the small
parts of a program work, then it is more likely that all the parts
together will work. Testing a whole program is called *integration
testing*. HUnit and QuickCheck are both capable of integration testing,
though sometimes it’s harder to use these tools in that setting. Today,
we’ll focus on using these tools only for unit testing.

HUnit gives us the ability to run a function against a certain input and
to observe whether or not it produces the desired output. Let’s do this
for `merge1`:

> test1_merge1 :: Test
> test1_merge1 = "alternating numbers: [1,3,5] [2,4,6]" ~:
>                merge1 [1,3,5] [2,4,6] ~?= [1,2,3,4,5,6]

Before unpacking that line, let’s run the test with `runTestTT` to see
what happens:

    *Main> runTestTT test1_merge1
    ### Failure in: "alternating numbers: [1,3,5] [2,4,6]"
    expected: [1,2,3,4,5,6]
     but got: [1,3,5]
    Cases: 1  Tried: 1  Errors: 0  Failures: 1
    Counts {cases = 1, tried = 1, errors = 0, failures = 1}

Oh. The function’s wrong. What a surprise.

Happily, the output is quite helpful, though. It tells us the name of
the test, what was expected, and what actually happened. There is also a
summary at the bottom, in case we had run many tests, only some of which
failed.

Now, let’s unpack what’s going on. We’ll start, as usual, with the
types:

``` {.haskell}
(~?=) :: (Show a, Eq a) => a -> a -> Test
(~:) :: Testable t => String -> t -> Test
runTestTT :: Test -> IO Counts
```

The `(~?=)` operator builds a `Test` by comparing the equality of its
two operands, which must be `Show`able. That’s perfect for our needs: on
one side, we put the call to the function we’re testing, and on the
other side, we put the desired output. By convention, HUnit testing
operators put a `?` toward the side where the failure might occur –
that’s how HUnit knew which value was expected and which was the actual
result. HUnit also provides `(~=?)` which swaps the arguments (but has
the same type, as the types don’t capture the distinction between actual
and expected).

The `(~:)` operator serves only to label `Test`s with a string
describing the test. Because HUnit supports a few different ways to
build tests, `(~:)`’s type allows any type in the `Testable` class;
`Test` is surely `Testable` (as you can see in HUnit’s documentation).
The test above would have worked without the use of `(~:)`, but the
output would be less helpful. Also, note that `(~?=)` has a higher
precedence than `(~:)`, meaning that we don’t need parentheses. (You can
query the precedence of an operator in GHCi. If I say `:info ~:`, I get
`infixr 0 ~:`, which means that `(~:)` associates to the right with the
lowest possible precedence.

Finally, `runTestTT` is the function that runs tests and produces
output. It also returns a `Counts` object, which is printed in the last
line of the GHCi transcript above – it contains a summary of the testing
that can then be operated on programmatically, if `runTestTT` is called
from a larger program’s `main` action.

\*\* The `Test` type \*\*

HUnit actually exports the details of its `Test` type:

``` {.haskell}
data Test
  = TestCase Assertion
  | TestList [Test]
  | TestLabel String Test
```

You can build up tests using these constructors. (See the documentation
for more information about `Assertion`.) The only one that tends to get
used is `TestList`, which conveniently builds up a master test from many
sub-tests.

> test2_merge1 :: Test
> test2_merge1 = TestList [ "one element: [1] []" ~:
>                           merge1 [1] [] ~?= [1]
>                         , test1_merge1 ]

    *Main> runTestTT test2_merge1
    ### Failure in: 0:"one element: [1] []"
    expected: [1]
     but got: []
    ### Failure in: 1:"alternating numbers: [1,3,5] [2,4,6]"
    expected: [1,2,3,4,5,6]
     but got: [1,3,5]
    Cases: 2  Tried: 2  Errors: 0  Failures: 2
    Counts {cases = 2, tried = 2, errors = 0, failures = 2}

That’s nice, helpful output. Not only does it give us names, it gives us
locations within the list of tests.

HUnit has a bunch more stuff that can be useful. Read its documentation.

Before we go on fixing `merge`, it’s helpful to generalize the tests, so
we can run them on *any* merge function, not just `merge1` in
particular. You won’t generally need to do this in your tests, but it’s
a nice example of functional programming.

> type MergeFun = Ord a => [a] -> [a] -> [a]
> test2 :: MergeFun -> Test
> test2 merge = TestList [ "one element: [1] []" ~:
>                          merge [1] [] ~?= [1]
>                        , "alternating numbers: [1,3,5] [2,4,6]" ~:
>                          merge [1,3,5] [2,4,6] ~?= [1,2,3,4,5,6]
>                        ]

OK. Now let’s make a better job at merge. It seems we forgot to include
the greater of the two elements when comparing.

> merge2 :: MergeFun
> merge2 all_xs@(x:xs) all_ys@(y:ys)
>   | x < y     = x : merge2 xs all_ys
>   | otherwise = y : merge2 all_xs ys
> merge2 _             _             = []

    *Main> runTestTT (test2 merge2)
    ### Failure in: 0:"one element: [1] []"
    expected: [1]
     but got: []
    ### Failure in: 1:"alternating numbers: [1,3,5] [2,4,6]"
    expected: [1,2,3,4,5,6]
     but got: [1,2,3,4,5]
    Cases: 2  Tried: 2  Errors: 0  Failures: 2
    Counts {cases = 2, tried = 2, errors = 0, failures = 2}

This is better (?). Now we’re not dropping everything, but it doesn’t
seem to work once one of the lists is exhausted. Let’s take yet another
stab:

> merge3 :: MergeFun
> merge3 all_xs@(x:xs) all_ys@(y:ys)
>   | x < y     = x : merge3 all_ys xs
>   | otherwise = y : merge3 all_xs ys
> merge3 xs            []            = xs
> merge3 _             _             = []

    *Main> runTestTT (test2 merge3)
    Cases: 2  Tried: 2  Errors: 0  Failures: 0
    Counts {cases = 2, tried = 2, errors = 0, failures = 0}

Huzzah!

Let’s add another test:

``` {.haskell}
test3 :: MergeFun -> Test
test3 merge = "empty lists: [] []" ~:
              merge [] [] ~?= []
```

    *Main> :load "09-testing.lec.lhs"
    [1 of 1] Compiling Main             ( 09-testing.lec.lhs, interpreted )

    /Users/rae/work/cis194/haskell/weeks/09-testing/09-testing.lec.lhs:194:17:
        No instance for (Ord a0) arising from a use of ‘merge’
        The type variable ‘a0’ is ambiguous
        Note: there are several potential instances:
          instance Integral a => Ord (GHC.Real.Ratio a)
            -- Defined in ‘GHC.Real’
          instance Ord Integer -- Defined in ‘integer-gmp:GHC.Integer.Type’
          instance Ord time-1.4.2:Data.Time.LocalTime.LocalTime.LocalTime
            -- Defined in ‘time-1.4.2:Data.Time.LocalTime.LocalTime’
          ...plus 28 others
        In the first argument of ‘(~?=)’, namely ‘merge [] []’
        In the second argument of ‘(~:)’, namely ‘merge [] [] ~?= []’
        In the expression: "empty lists: [] []" ~: merge [] [] ~?= []

    /Users/rae/work/cis194/haskell/weeks/09-testing/09-testing.lec.lhs:194:29:
        No instance for (Show a0) arising from a use of ‘~?=’
        The type variable ‘a0’ is ambiguous
        Note: there are several potential instances:
          instance Show Double -- Defined in ‘GHC.Float’
          instance Show Float -- Defined in ‘GHC.Float’
          instance (Integral a, Show a) => Show (GHC.Real.Ratio a)
            -- Defined in ‘GHC.Real’
          ...plus 39 others
        In the second argument of ‘(~:)’, namely ‘merge [] [] ~?= []’
        In the expression: "empty lists: [] []" ~: merge [] [] ~?= []
        In an equation for ‘test3’:
            test3 merge = "empty lists: [] []" ~: merge [] [] ~?= []
    Failed, modules loaded: none.

Yuck! What’s going on here? The problem is that GHC cannot figure out
what type we want for our empty lists. The `Show` and `Ord` instances
very much care what type we choose, so GHC doesn’t want to choose for
us. We need to add a type annotation to fix the problem:

> test3 :: MergeFun -> Test
> test3 merge = "empty lists: [] []" ~:
>               merge [] [] ~?= ([] :: [Integer])

The `:: [Integer]` chooses the type for us, so GHC knows what to do.

Are we done? Is our function perfect?

Property-based testing
----------------------

Writing test cases is boring. And, it’s easy to miss out on unexpected
behavior. Much better (and, more along the lines of *wholemeal
programming*) is to define *properties* we wish our function to have.
Then, we can get the computer to generate the test cases for us.

QuickCheck is the standard Haskell library for property-based testing.
The ides is that you define a so-called *property*, which is then tested
using pseudo-random data.

For example:

> prop_numElements_merge3 :: [Integer] -> [Integer] -> Bool
> prop_numElements_merge3 xs ys
>   = length xs + length ys == length (merge3 xs ys)

This property is saying that the sum of the lengths of the input lists
should be the same as the length of the output list. (It is customary to
begin property names with `prop_`.) Let’s try it!

    *Main> quickCheck prop_numElements_merge3
    *** Failed! Falsifiable (after 5 tests and 4 shrinks): 
    []
    [0]

(Your results may differ slightly. Remember: it’s using randomness.)

The first thing we notice is that our function is clearly wrong, with
lots of stars and even an exclamation point! We then see that QuickCheck
got through 5 tests before discovering the failing test case, so our
function isn’t terrible. QuickCheck tells us what the failing arguments
are: `[]` and `[0]`. Indeed GHCi tells us that `merge3 [] [0]` is `[]`,
which is wrong.

What’s so nice here is that QuickCheck found us such a nice, small test
case to show that our function is wrong. The way it can do so is that it
uses a technique called *shrinking*. After QuickCheck finds a test case
that causes failure, it tries successively smaller and smaller arguments
(according to a customizable definition of “smaller”) that keep failing.
QuickCheck then reports only the smallest failing test case. This is
wonderful, because otherwise the test cases QuickCheck produces would be
unwieldy and hard to reason about.

A final note about this property is that the type signature tells us
that the property takes lists of integers, not any type `a`. This is so
that GHC doesn’t choose a silly type to test on, such as `()`. We must
always be careful about this when writing properties of polymorphic
functions. Numbers are almost always a good choice.

\*\* Implications \*\*

As we did above, let’s generalize our test over implementations of our
merging operation. Again, you likely won’t need to do this.

> prop_numElements :: MergeFun -> [Integer] -> [Integer] -> Bool
> prop_numElements merge xs ys
>   = length xs + length ys == length (merge xs ys)

And, we take another stab at our function:

> merge4 :: MergeFun
> merge4 all_xs@(x:xs) all_ys@(y:ys)
>   | x < y     = x : merge4 xs all_ys
>   | otherwise = y : merge4 all_xs ys
> merge4 xs            ys            = xs ++ ys

    *Main> quickCheck (prop_numElements merge4)
    +++ OK, passed 100 tests.

Huzzah!

Is that it? Are we done? Not quite. Let’s try another property:

> prop_sorted1 :: MergeFun -> [Integer] -> [Integer] -> Bool
> prop_sorted1 merge xs ys
>   = merge xs ys == sort (xs ++ ys)

    *Main> quickCheck (prop_sorted1 merge4)
    *** Failed! Falsifiable (after 4 tests and 3 shrinks): 
    []
    [1,0]

Drat. QuickCheck quite reasonably tried the list `[1,0]` as an input to
our function. Of course, this isn’t going to work because it’s not
already sorted. We need to specify an implication property:

> prop_sorted2 :: MergeFun -> [Integer] -> [Integer] -> Property
> prop_sorted2 merge xs ys
>   = isSorted xs && isSorted ys ==> merge xs ys == sort (xs ++ ys)
>
> isSorted :: Ord a => [a] -> Bool
> isSorted (a:b:rest) = a <= b && isSorted (b:rest)
> isSorted _          = True    -- must be fewer than 2 elements

In `prop_sorted`, we see the use of the operator `(==>)`. Its type is
`Testable prop => Bool -> prop -> Property`. (This is a *different*
`Testable` than HUnit’s.) It takes a `Bool` and a `Testable` thing and
produces a `Property`. Note how `prop_sorted` returns a `Property`, not
a `Bool`. We’ll sort these types out fully later, but I wanted to draw
your attention to the appearance of `Property` there.

Let’s see how this works:

    *Main> quickCheck (prop_sorted2 merge4)
    *** Gave up! Passed only 21 tests.

(And that took maybe 20 seconds.) There aren’t any failures, but there
aren’t a lot of successes either. The problem is that QuickCheck will
run the test only when both randomly-generated lists are in sorted
order. The odds that a randomly-generated list of length `n` is sorted
is `1/n!`, which is generally quite small odds. And we need *two* sorted
lists. This isn’t going to work out well.

\*\* QuickCheck’s types \*\*

How does QuickCheck generate the arbitrary test cases, anyway? It uses
the `Arbitrary` class:

``` {.haskell}
class Arbitrary a where
  arbitrary :: Gen a
  shrink    :: a -> [a]
```

We’ll leave `shrink` to the online documentation and focus on
`arbitrary`. The `arbitrary` method gives us a `Gen a` – a generator for
the type `a`. Of course, the `arbitrary` method for lists doesn’t care
about ordering (indeed, it can’t, due to parametricity), but we do.
Luckily, this is a common problem, and QuickCheck offers a solution in
the form of `OrderedList`, a wrapper around lists that have the right
`Arbitrary` instance for our needs:

``` {.haskell}
newtype OrderedList a = Ordered { getOrdered :: [a] }
instance (Ord a, Arbitrary a) => Arbitrary (OrderedList a) where ...
```

(`newtype` is almost just like `data`. Poke around online for more
info.)

Now, let’s rewrite our property:

> prop_sorted3 :: MergeFun
>              -> OrderedList Integer -> OrderedList Integer -> Bool
> prop_sorted3 merge (Ordered xs) (Ordered ys)
>   = merge xs ys == sort (xs ++ ys)

    *Main> quickCheck (prop_sorted3 merge4)
    +++ OK, passed 100 tests.

Huzzah! Just by changing the types a bit, we can affect instance
selection to get what we want.

Yet, this all seems like black magic. How does QuickCheck do it? Let’s
look more in depth at the types.

``` {.haskell}
quickCheck :: Testable prop => prop -> IO ()

class Testable prop where ...
instance Testable Bool where ...
instance Testable Property where ...
instance (Arbitrary a, Show a, Testable prop) => Testable (a -> prop) where ...
```

We can `quickCheck` anything that’s `Testable`. Boolean values are
`Testable`, as are the somewhat mysterious `Property`s. But it’s the
last instance listed here of `Testable` that piques our curiosity. It
says that a *function* is `Testable` as long as its argument has an
`arbitrary` method, the argument can be printed (in case of failure),
and the result is `Testable`.

Is `[Integer] -> [Integer] -> Bool` `Testable`? Sure it is. Recall that
`[Integer] -> [Integer] -> Bool` is equivalent to
`[Integer] -> ([Integer] -> Bool)`. Because `[Integer]` has both an
`Arbitrary` instance and a `Show` instance, we can use the last instance
above as long as `[Integer] -> Bool` is `Testable`. And that’s
`Testable` because we (still) have an `Arbitrary` and a `Show` instance
for `[Integer]`, and `Bool` is `Testable`. So, that’s how `quickCheck`
works – it uses the `Arbitrary` instances for the argument types. And,
that’s how changing the argument types to `OrderedList Integer` got us
the result we wanted.

\*\* Generating arbitrary data \*\*

When you want to use QuickCheck over your own datatypes, it is necessary
to write an `Arbitrary` instance for them. Here, we’ll learn how to do
so.

Let’s say we have a custom list type

> data MyList a = Nil | Cons a (MyList a)
>
> instance Show a => Show (MyList a) where
>   show = show . toList
>   
> toList :: MyList a -> [a]
> toList Nil           = []
> toList (a `Cons` as) = a : toList as
>
> fromList :: [a] -> MyList a
> fromList []     = Nil
> fromList (a:as) = a `Cons` fromList as

If we want an `Arbitrary` instance, we must define the `arbitrary`
method, of type `Gen (MyList a)`. Luckily for us, `Gen` is a monad (did
you see that coming?), so some of its details are already familiar. We
also realize that if we want arbitrary lists of `a`, we’ll need to make
arbitrary `a`s. So, our instance looks like

> instance Arbitrary a => Arbitrary (MyList a) where
>   arbitrary = genMyList1 

At this point, it’s helpful to check out the combinators available in
the “Generator combinators” section of the [QuickCheck
documentation](http://hackage.haskell.org/package/QuickCheck-2.7.6/docs/Test-QuickCheck.html).

At this point, it’s helpful to think about how you, as a human, would
generate an arbitrary list. One way to do it is to choose an arbitrary
length (say, between 0 and 10), and then choose each element
arbitrarily. Here is an implementation:

> genMyList1 :: Arbitrary a => Gen (MyList a)
> genMyList1 = do
>   len <- choose (0, 10)
>   vals <- replicateM len arbitrary
>   return $ fromList vals

Let’s try it out:

    *Main> sample genMyList1
    [(),(),(),(),(),()]
    []
    [(),(),(),(),(),(),(),(),()]
    [(),(),(),(),(),()]
    [(),(),(),(),(),(),(),(),()]
    [()]
    [(),(),(),(),(),(),(),()]
    [(),(),(),(),(),(),(),(),()]
    [(),(),()]
    [(),(),(),(),(),()]
    [(),(),(),(),(),(),(),(),(),()]

The arbitrary lengths are working, but the element generation sure is
boring. Let’s use a type annotation to spruce things up (and override
GHC’s default choice of `()`)!

    *Main> sample (genMyList1 :: Gen (MyList Integer))
    [0,0,0,0,0,0,0,0,0,0]
    []
    [-2,3,1,0,4,-1]
    [-5,0,2,1,-1,-3]
    [-5,-6,-7,-2,-8,7,-3,4,-6]
    [4,-3,-3,2,-9,9]
    []
    [10,-1]
    [9,-7,-16,3,15]
    [0,14,-1,0]
    [3,18,-13,-17,-20,-8]

That’s better.

This generation still isn’t great, though, because perhaps a function
written over `MyList`s fails only for lists longer than 10. We’d like
unbounded lengths. Here’s one way to do it:

> genMyList2 :: Arbitrary a => Gen (MyList a)
> genMyList2 = do
>   make_nil <- arbitrary
>   if make_nil    -- type inference tells GHC that make_nil should be a Bool
>      then return Nil
>      else do
>        x <- arbitrary
>        xs <- genMyList2
>        return (x `Cons` xs)

    *Main> sample (genMyList2 :: Gen (MyList Integer))
    [0,0,0,0,0,0]
    []
    [3,-3]
    []
    []
    [-1,-1]
    [-10]
    []
    []
    [11]
    [-20,-14]

The lengths are unbounded (you’ll just have to trust me there), but
we’re getting a *lot* of empty lists. This is because at every link in
the list, there’s a 50% chance of producing `Nil`. That means that a
list of length `n` will appear only one out of `2^n` times. So, lengths
are unbounded, but very unlikely.

The way to make progress here is to use the `sized` combinator.
QuickCheck is set up to try “simple” arbitrary things before “complex”
arbitrary things. The way it does this is using a size parameter,
internal to the `Gen` monad. The more generating QuickCheck does, the
higher this parameter gets. We want to use the size parameter to do our
generation.

Let’s look at the type of `sized`:

``` {.haskell}
sized :: (Int -> Gen a) -> Gen a
```

An example is the best way of explaining how this works:

> genMyList3 :: Arbitrary a => Gen (MyList a)
> genMyList3 = sized $ \size -> do
>   len <- choose (0, size)
>   vals <- replicateM len arbitrary
>   return $ fromList vals

    *Main> sample (genMyList3 :: Gen (MyList Integer))
    []
    [-2]
    [-1,3,4]
    [-4,-2,1,-1]
    []
    []
    [12,3,11,0,3,-12,10,5,11,12]
    [-4,-8,-9,2,14,5,8,11,-1,7,11,-8,2,-6]
    [6,10,-5,15,6]
    [-3,-18,-4]
    [9,19,13,-19]

That worked nicely – the lists tend to get longer the later they appear.
The idea is that `sized` takes a *continuation*: the thing to do with
the size parameter. We just use a lambda function as the one argument to
`sized`, where the lambda binds the `size` parameter, and then we can
use it internally. If that’s too painful (say we just want to produce
the size parameter, without using a continuation), you could always do
something like this:

> getSize :: Gen Int
> getSize = sized return

I’ll leave it to you to figure out how that works. Follow the types!

As one last example, we can also choose arbitrary generators from a list
based on frequency. Although the length method of `genMyList3` works
well for lists, the following technique is much better for trees:

> genMyList4 :: Arbitrary a => Gen (MyList a)
> genMyList4 = sized $ \size -> do
>   frequency [ (1, return Nil)
>             , (size, do x <- arbitrary
>                         xs <- {- resize (size - 1) -} genMyList4
>                         return (x `Cons` xs) )]

    *Main> sample (genMyList4 :: Gen (MyList Integer))
    []
    [2,0]
    [4,0,-1]
    [-6,-2,3,-1,-1]
    [6,6]
    [2,2,2,6,6]
    [-6,-9,5,-5]
    [8,7,5,7,7,-1,-2,-1,-5,-3]
    [15,-12,14,13,-5,-10,-9,-8,-2]
    [12,-11,-8,6,-6,-4,11,11]
    [-7,1,-3,4,-3,-9,4,6,-2,10,-9,-7,5,7,1]

Let’s look at the type of `frequency`:

``` {.haskell}
frequency :: [(Int, Gen a)] -> Gen a
```

It takes a list of `(Int, Gen a)` pairs and produces a `Gen a`. The
numbers in the list give the likelihood of choosing that element. Above,
we fixed the frequency of `Nil` at 1, but let the likelihood of `Cons`
vary according to the desired size. Then, in the recursive call to
`genMyList4`, we used `resize` to lower the `size` parameter. Otherwise,
it’s too likely to get a runaway list that goes on toward infinity.

* * * * *

`Generated 2014-12-04 13:37:47.213713`
