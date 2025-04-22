### v0.9.1
   Changed `writePretty` to truncate files when writing to them.

### v0.9.0
   Changed `writePretty` to use Java interop directly rather than `Files` which has been removed from Flix's stdlib.

### v0.8.0
   Removed use of bang suffix for mutating functions.

### v0.7.0
   Added `isEmpty` introspective function.
   Renamed conditional functions e.g. `optionalParens` becomes `conditionalParens`.
   Added introspective functions that use the `optional` prefix e.g. `optionalParens` only prints parens if the body is not empty.
   Fixed `encloseSep` so it does not print an initial space after the left opening.

### v0.6.0
   Added `besideSoftSpace` and its operator `<^>`.
   Changed capitalization `besideSoftline` to `besideSoftLine`.
   Added `SemiGroup` and `Moniod` instance. The `Add` instance may be removed in future.

### v0.5.0
   Removed horizontal concat without space operator `<>`, use `+` instead.
   Added `Doc` instance of `Add` trait so we can use the `+` operator.

### v0.4.1
   Updated to use `StringBuilder.empty()`

### v0.4.0
   Added `Pretty` trait.
   Updated acknowledgement in `PrettyPrint.flix`.
   `maybeParens` renamed `optionalParens` and added `optionalBraces` etc.

### v0.3.3
   Updated to handle changes to `Files` module in the standard library (`IOError`).

### v0.3.2
   Updated to use the renamed `Files` module in the standard library.

### v0.3.1
   Improved `besideSoftLine` doc and added test.

### v0.3.0
   Updated effects signatures - thanks Matthew.
   Some internal tidying up and added API comments.

### v0.2.1
   Fixed some uses of old `namespace` syntax.

### v0.2.0
   Changed to use `mod` syntax rather than `namespace`.
   
### v0.1.0
   Initial release.
