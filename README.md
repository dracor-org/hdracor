# HDraCor -- Type Safe Functional Drama Analysis in Haskell #

`HDraCor` is a wrapper around the API of
[dracor.org](https://dracor.org) for the
[Haskell](https://www.haskell.org/) programming language. It offers
functions for parsing the JSON data returned by the API functions into
Haskell data types.

## Installation ##

[Stack](https://haskellstack.org/), the tool for
building Haskell projects, is required. After installing `stack` just
clone this repo, `cd` into it and do:

	stack build

This will take some time and download quite a number of libraries. You
can run the test suite by

	stack test


## Usage of the library ##

`HDraCor` is minimal. Therefore it does not define a specific HTTP
library, but abstracts the HTTP layer away in a way seen in many other
functional programming libraries. All the library does is parsing JSON
to the records defined in `Text.DraCor.Types`. You can use
e.g. ['http-conduit'](https://hackage.haskell.org/package/http-conduit-2.3.7.3/docs/Network-HTTP-Conduit.html)
for HTTP:

	import Network.HTTP.Conduit
	import qualified Data.ByteString.Lazy as B

	fetch :: String -> IO B.ByteString
	fetch path = simpleHttp $ "https://dracor.org/api" ++ path


Now you can pass that `fetch` function to the functions from
`Text.DraCor.API` that wrap the API. E.g.:

	import Text.DraCor.API

	corpus <- getCorpus fetch "ger"

The result is wrapped in a `Maybe` functor and is `Nothing` if the
body of the HTTP response could not be parsed.

See the [`crawler`](crawler/app/Main.hs) for an example usage of the
library.

## Tolerant and intolerant parsers ##

The library offers two sets of JSON parsers: One set is fault-tolerant
and will succeed parsing despite issues #83-89 of
[dracor-api](https://github.com/dracor-org/dracor-api). The other
one--which is still under construction--is intolerant an can be used
for application testing.

Caveat: The to sets live in the modules `Text.DraCor.TolerantJSON` and
`Text.DraCor.IntolerantJSON` respectively and are *orphan
instances*. So, don't try to import both of them!

## Crawler ##

There is a crawler in the [`crawler`](./crawler) folder. It is
intended for testing the API and as a demo application. It can be
built and installed with

	stack build
	stack install

Then do

	crawldracor -h

to see what it can do.

## License

Licensed under either of:

- [BSD-3-Clause license](https://opensource.org/licenses/BSD-3-Clause)
- [Apache License, version 2.0](https://opensource.org/licenses/Apache-2.0)

As a user, you may use this code under either license, at your option.
