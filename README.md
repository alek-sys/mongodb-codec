# mongodb-codec

## Overview
The idea is to provide support of automatic case classes serialisation / deserialisation usign standard MongoDB Scala driver. Intention is not to use reflection in runtime but instead create codecs in compile time using (macro annotations)[http://docs.scala-lang.org/overviews/macros/annotations.html]. 

## How to run
Clone repo, cd into code folder. Then `sbt` to get into sbt console and 'core/run' to run sample code.
