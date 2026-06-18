# Number Formatting Utilities

Internal utilities for locale-aware integer formatting of participant
counts in selecta diagrams. Counts are always integers, so the formatter
only needs a thousands separator (no decimal mark for the value itself,
though some preset locales still set one for completeness).

## Global Option

The default number format can be set once per session:

      options(selecta.number_format = "eu")

This avoids passing `number_format` to every function call.
