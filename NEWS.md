# utile.tables 0.1.5
* Addressed `tibble::is.tibble()` depreciation.
* `build_row()`/`build_event_row()`: Removed column name logging to console.
* Backend function changes:
  - `.row_coxph()`: Rewrote to work with `utile.tools::tabulate_model()` version 0.2.0.
  - `.row_` Functions:
    - Rewrote with a functional programming design.
    - Removed redundant code from calls of `utile.tools::paste_freq()`.

# utile.tables 0.1.4
* First public release
