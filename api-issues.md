# Inconsistent keys and types #

Keys and value types in a play meta data are inconsistent:

/corpora/{corpusname}/   vs. /corpora/{corpusname}/metadata

yearPremiered :: Int  vs. premiereYear :: String
yearPrinted :: Int    vs. printYear :: String
yearWritten :: Int    vs. writtenYear :: String
yearNormalized :: Int vs. [not present]


# Redundancy of keys #

There seems to be a redundancy in the play-objects returned by
`corpora/{corpusname}/metadata`: Both, the key `name` and the key
`playName` have the same value and there seems to be no semantic
difference.

Maybe the type of `maxDegreeIds` should be a list of strings. Now it's
a string and if several characters share the maximum they are
delimited by by the pipe character, `"|"`.

`"ruffus|quintus"` should be `["ruffus", "quintus"]`, I think.
If there's a single character with max degree, then this list should
be of length 1, i.e. `["karl"]`, not `"karl"`. 


# Type of averageClustering #

Hi,

for some plays in `corpora/{corpusname}/metadata` the data type of
`averageClustering` is the string `"0"`, but it should be either a
floating point value or `null`.

Ids of plays: ger000048 ...356 ...260 ...370

Regards,
Chris


Example:

  {
    "size": 3,
    "genre": null,
    "averageClustering": "0",
    "numOfPersonGroups": 0,
    "density": 0.6666666666666666,
    "averagePathLength": 1.3333333333333333,
    "maxDegreeIds": "ariadne",
    "averageDegree": 1.3333333333333333,
    "name": "brandes-ariadne-auf-naxos",
    "diameter": 2,
    "yearPremiered": 1775,
    "yearPrinted": null,
    "maxDegree": 2,
    "numOfSpeakers": 3,
    "yearNormalized": 1775,
    "numConnectedComponents": 1,
    "numOfSpeakersUnknown": 0,
    "yearWritten": null,
    "id": "ger000048",
    "numOfSpeakersFemale": 2,
    "numOfSegments": 2,
    "numOfSpeakersMale": 1,
    "wikipediaLinkCount": 4,
    "numOfActs": 0,
    "playName": "brandes-ariadne-auf-naxos"
}


# Type of betweenness #

Hi,

sometimes on the api function
`/corpora/{corpusname}/play/{playname}/metrics` the type of
`betweenness` is a string, but should be an floating point number or
`null`.

If found `"betweenness": "0"` on the play `alberti-brot` on some
objects in `nodes`.

Regards,
Chris


# Type of source #

Hi,

in API function `corpora/{corpusname}` the type of `source` is a
String, whilel in API function `corpora/{corpusname}/play/{playname}`
its type is an object with two keys, `name` and `url`. This
information isn't present present on the plays listed in
`corpara/{corpusname}/metadata` - which does not matter.

The inconsistency of the type makes writing a wrapper for the API in a
statically typed language difficult.

Regards,
Chris


## Type of degree etc. ##

Hi,

using the crawler from hdracor I found some false types in the
returned json for
`/corpora/ger/play/blumenthal-im-weissen-roessl/metrics`. The
`degree`, `closeness` and `weightedDegree` for the node with the `id`
`portier` are strings, but should be integers or `null`. Seems to be a
matter of infinitesimal values.

    {
      "weightedDegree": "0",
      "degree": "0",
      "closeness": "0",
      "eigenvector": 4.343654944477457e-14,
      "id": "portier",
      "betweenness": "0"
    }

Regards,
Christian

PS. I made hdracor tolerant against this type inconsistency. Use
commit 3a84bba65676b073759cb10fb2ff1a668c496056 to reproduce. There
are 24 more plays in gerdracor with the same issue.
