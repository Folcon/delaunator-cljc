# delaunator-cljc

Fast 2D Delaunay triangulation in Clojure. A port of [Delaunator](https://github.com/mapbox/delaunator).

## Overview

This is a straight port of the javascript library, it's not as performant,
and it's a goal to try and keep the code in reasonably readable clojure/script.
(It's really not successful at present...)

Part of the use-case of this library is also to ensure that the code in it
can be used from Clojure as well as Clojurescript.

This library was primarily intended to be used for tinkering and projects
that I have.

## Development

To get an interactive development environment run:

    lein fig:dev

This will auto compile and send all changes to the browser without the
need to reload. After the compilation process is complete, you will
get a Browser Connected REPL. An easy way to try it is:

    (js/alert "Am I connected?")

and you should see an alert in the browser window.

To clean all compiled files:

	lein clean

To create a production build run:

	lein clean
	lein fig:min


## License

Copyright © 2019 Folcon

Distributed under the MIT License.
