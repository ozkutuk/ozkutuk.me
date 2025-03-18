---
layout: post
title:  "In praise of affine optics"
date:   2025-03-16 12:00:00 +0300
categories: haskell
draft: true
---

Recently at work, I was tasked with picking a suitable optics library for our
team to use. More specifically, a library that we could migrate to from
`microlens`. In the end, we went with the [`optics`][optics] library. The
reasons for the migration and what went into the decision process could be the
topic of another blog post (one that I will hopefully write soon). However, in
this short post, I will talk about one of the—in my opinion, undersold—features
provided by the `optics` library: affine optics. Considering that affine optics
do not exist in the `lens` library, one might assume that they are some obscure
    optics that exist solely for novelty's sake. In this post I will try to
    convince you that, on the contrary, affine optics are _very_ useful in
    practice and should be something that exists in any working Haskell
    programmer's toolkit.

The rest of this post assumes basic familiarity with optics, specifically folds
and traversals. I will start with a _very_ brief recap of what those are, but if
you need a more complete introduction to optics in general, I can suggest
[`Control.Lens.Tutorial`][lens-tutorial].

## Folds and traversals, a short recap

In the documentation of `optics`, `Fold` is explained as follows:

> A `Fold S A` has the ability to extract some number of elements of type `A`
> from a container of type `S`. 

The "some number" of elements mentioned in the quote could be zero, one, or any
other non-negative number. That is, it can be conceptualized as retrieving a
list of values. In fact, we can do exactly that by using the `toListOf`
eliminator:

```haskell
-- Given a Fold "pairs" that focuses both elements of pairs in a list, 
>>> toListOf pairs [(1, 2), (3, 4), (5, 6)]
[1,2,3,4,5,6]
```

`Traversal` can be thought of as `Fold` that can also be used to update the
value of its foci. This implies that a `Traversal` can be used wherever a `Fold`
is expected, but not vice versa. In fact, the `pairs` `Fold` we had in the
previous code snippet (whose definition is of no importance) is actually a
`Traversal`, so we can use it to, say, increment each focused element:

```haskell
>>> over pairs succ [(1, 2), (3, 4), (5, 6)]
[(2,3),(4,5),(6,7)]
```

Now that we are done with the recap, let's get to the meat of the post (I told
the recap would be _very_ brief, didn't I?)

## Enter affine optics

`Traversal` and `Fold` also have affine counterparts, aptly-named as
`AffineTraversal` and `AffineFold`. Whereas a `Traversal`[^1]  targets
potentially zero, one, or multiple elements, an `AffineTraversal` can only
target either zero or one element. Consequently, you can now use `preview` to
view the target value instead of `toListOf`:

```haskell
-- _head :: AffineTraversal' [a] a
>>> preview _head [1, 2, 3]
Just 1
>>> preview _head []
Nothing
```

The difference might seem insignificant at first glance. However, I would argue
that distinguishing affine optics from their regular counterparts is important
for a few reasons.

### Affine optics are a natural extension

As mentioned above, `Traversal` can focus many elements. Therefore, `Traversal`
can be conceptualized as a list of values. That is, if you have a `Traversal' S
A` at hand[^2], you can think of it as something that allows you to get a hold
of an `[A]` given an `S`.

If you can target a list of values through `Traversal`, how can you target an
optional value (i.e. a `Maybe`)? That is exactly what `AffineTraversal` is for!
Just like how lists are ubiquituous in Haskell, `Maybe` is also indispensable
and ever-present in every Haskell codebase. Therefore, having an optic kind that
perfectly matches this pattern is certainly quite useful.

### Affine optics provide additional type safety

One can read the previous point and think, "well, `lens` users have been doing
without affine optics just fine". Certainly, whatever `AffineTraversal`
provides, one can do pretty much the same with `Traversal`, just as one can
live without `Maybe` and just use singleton lists instead. Is that what we want
though? As Haskell programmers, we enjoy the fact that we can model the problem
domain precisely through the types. Preaching the motto "make illegal states
unrepresentable", we shouldn't settle for a `Traversal` if we know that it
could focus at most a single value anyway. `AffineTraversal` perfectly captures
this notion. 

To put it in concrete terms, let's try to `preview` a `Traversal` using both
`lens` and `optics` libraries and contrast the result:

```haskell
-- lens
>>> preview pairs [(1, 2), (3, 4), (5, 6)]
Just 1

-- optics
>>> preview pairs [(1, 2), (3, 4), (5, 6)]

<interactive>:1:1: error: [GHC-64725]
    • A_Traversal cannot be used as An_AffineFold
```

The difference is abundantly clear: `optics` code does not typecheck since a
`Traversal` (or a `Fold` for that matter) cannot be used as an `AffineFold`,
whereas `lens` happily accepts it by returning the first target and silently
discarding the rest.

Why is the `Traversal` being used as an `AffineFold` in the first place? Here
is the type of `preview` from `optics`[^3]:

```haskell
preview :: AffineFold s a -> s -> Maybe a
```

As you can see, `preview` expects an `AffineFold`. It makes sense, since you
are trying to potentially extract a single value. What if you want the behavior
of `lens` though, i.e. you want to extract the first target of the `Fold`? In
that case, you need to use a specialized combinator instead:

```haskell
>>> headOf pairs [(1, 2), (3, 4), (5, 6)]
Just 1

-- ...or you can turn "pairs" into an "AffineFold" that targets just
-- the first focus of the original "Fold" by using "pre":
>>> preview (pre pairs) [(1, 2), (3, 4), (5, 6)]
Just 1
```

The point I am trying to get across is that `optics` (through making
`AffineTraversal` a distinct optic) forces the developer to make an explicit
decision on the behavior: you either use a `Fold` with `toListOf` to obtain a
list of values, or you _explicitly_ pick the first target of it.

### Affine optics are everywhere

What about `Prism`s? They also can capture the notion of a single value
potentially existing, right? Well, yes, but conceptually `Prism` generalizes the
notion of a constructor. This means that to be able to create a `Prism`, one
also has to provide a way to construct the "bigger" type from the "smaller" one.
This makes `Prism`s much less generally useful compared to `AffineTraversal`s.

In contrast, once I started using the `optics` library, I have started to notice
affine optics everywhere. In hindsight, this is not a surprising result. Below
is the subtyping relation of different optic kinds:

![Subtyping hierarchy of optics][optics-subtyping]

Notice the arrows going into `AffineTraversal` from `Lens` and `Prism`. This
means that both `Lens` and `Prism` can be used as an `AffineTraversal`. A direct
implication of this is that you obtain an `AffineTraversal` if you compose a
`Lens` and a `Prism`. This `Lens`-`Prism` composition is very common in business
code that have lots of different record types modelling the problem domain.
Ignoring my lack of creativity for a moment, let's take a look into this example
featuring the `Animal` type:

```haskell
data Animal
  = Dog String DogBreed
  | Cat String CatBreed

data DogBreed
 = Bulldog
 | Pomeranian
 | Other

-- data CatBreed = ...
```

Then, the breed of a dog could be targeted like this[^4]:

```haskell
>>> let myDog = Dog "Missile" Pomeranian

>>> preview (#_Dog % _2) myDog
Just Pomeranian

-- can also be used for updates
>>> set (#_Dog % _2) Bulldog myDog
Dog "Missile" Bulldog
```

The optic used here is an `AffineTraversal`, because it is the composition of
`#_Dog` (which is a `Prism`) and `_2` (which is a `Lens`). You simply cannot
get by with a `Prism` here, because there is no general way to construct an
`Animal` given a `DogBreed`: what would the name of the dog be?

## Conclusion

Hopefully, this short post was enough to share my enthusiasm of affine optics. I
have been a happy `optics` user for a multitude of reasons, but I consider the
existence of affine optics to be one of the most prominent ones. If you are
looking for an optics library for your next project, consider giving `optics` a
go! Maybe you will enjoy using affine optics as much as I do.

## Addendum: Why doesn't `lens` have it?

TODO: this section could be a nice addition.

[^1]: And likewise, a `Fold`. For the rest of this post, I will only talk about
    `Traversal`s unless the distinction between a `Fold` and `Traversal`
    matters for the point at hand.
[^2]: If you are not too familiar with the optics libraries, the "\'" suffix
    means that the `Traversal` is a "simple" one, i.e. one that is not
    type-changing. The difference is not important for the purposes of this
    post.
[^3]: The actual type of `preview` is a bit more complicated, because it
    captures the subtyping relation through the use a class constraint. This
    allows one to, say, pass an `AffineTraversal` to `preview` (instead of just
    `AffineFold`). I took the liberty to simplify the type so as not to
    obfuscate the point I am trying to get across.
[^4]: This example makes use of the label optics derived through `Generic`, but
    handwritten optics would work just as well. See the
    [`Optics.Label`][label-optics] module if you want to follow along.

[optics]: https://hackage.haskell.org/package/optics
[lens-tutorial]: https://hackage.haskell.org/package/lens-tutorial-1.0.5/docs/Control-Lens-Tutorial.html
[optics-subtyping]: https://hackage.haskell.org/package/optics-0.4.2/docs/diagrams/optics.png
[label-optics]: https://hackage.haskell.org/package/optics-core-0.4.1/docs/Optics-Label.html
