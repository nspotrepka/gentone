# gentone

Gentone is a music composition library for generating tree-based melodic
sequences using functional genetic programming.

## Experimenting

First, make sure you have Leiningen.

```sh
$ brew install leiningen
```

To run an experiment, use the ```foo``` function.

```sh
$ lein repl

user=> (foo 4 300 5 5)
```

## Testing

Unit testing is necessary for good workflow.

```sh
$ lein test
```

## License

Copyright © 2016 Nathaniel Potrepka

Distributed under the MIT License.
