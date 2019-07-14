# type-errors-pretty

![pretty-bug](https://user-images.githubusercontent.com/4276606/61183911-eda86300-a679-11e9-9b8e-34c129469075.png)
[![Build status](https://img.shields.io/travis/chshersh/type-errors-pretty.svg?logo=travis)](https://travis-ci.org/chshersh/type-errors-pretty)
[![Hackage](https://img.shields.io/hackage/v/type-errors-pretty.svg?logo=haskell)](https://hackage.haskell.org/package/type-errors-pretty)
[![Stackage Lts](http://stackage.org/package/type-errors-pretty/badge/lts)](http://stackage.org/lts/package/type-errors-pretty)
[![Stackage Nightly](http://stackage.org/package/type-errors-pretty/badge/nightly)](http://stackage.org/nightly/package/type-errors-pretty)
[![MPL-2.0 license](https://img.shields.io/badge/license-MPL--2.0-blue.svg)](LICENSE)

> "It is important that we forgive ourselves for making mistakes. We need to learn
> from our errors and move on."
>
> ― Steve Maraboli, Life, the Truth, and Being Free

Combinators for writing pretty type errors easily. The word **pretty** here
doesn't mean that the resulting type errors will be pretty (though, I believe
they will be awesome), but the way the type errors are defined in your code is
pretty.

If you're interested in motivation behind using type errors at first place, you
can read the following blog post:

* [A story told by Type Errors](https://kodimensional.dev/type-errors)

The `type-errors-pretty` library allows you to write text of custom compile-time
error messages with less hassle in the following way:

```haskell
import Type.Errors.Pretty (type (<>), type (%))


type MessageText (e1 :: k) (e2 :: k) (es :: [k])
    = "You require the following two effects from your computation:"
    % ""
    % "    '" <> e1 <> "' and '" <> e2 <> "'"
    % ""
    % "However, your monad is capable of performing only the following effects:"
    % ""
    % "    " <> es
```

## Acknowledgement

Icons made by [Freepik](http://www.freepik.com) from [www.flaticon.com](https://www.flaticon.com/) is licensed by [CC 3.0 BY](http://creativecommons.org/licenses/by/3.0/).
