# FlexArray

An implementation of immutable functional arrays using Braun trees.
See: "ML for the working programmer".

### pre-reqs:

requirements are:
```
OCaml
Oasis (0.3+)
oUnit (for tests)
core (to run sample)
pa_pipebang (to run sample)
```

### Installing

```
oasis setup
ocaml setup.ml -all
ocaml setup.ml -install # may require sudo
```

### Usage

For now all important operations run in logarithmic time as expected. However
some operations can be reduced to O(n) from O(nlogn) most likely

See `lib/flexArray.mli`

### License

LGPL-2.1
