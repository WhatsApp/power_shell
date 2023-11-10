# Changelog
Every release has a corresponding tag. Use `main` branch for fresh-from-the-oven code.

## 1.3.0
  - Add support for map comprehensions from OTP 26

## 1.2.2
  - Bugfix enabling recursive power_shell invocations

## 1.2.1
  - Bugfix for configurable -on_load() support

## 1.2.0
  - Added support for hot-code loaded exports

## 1.1.7
  - Dropped support for OTP 20, added support for future OTP 25 release

## 1.1.6
  - Configurable support for -on_load() in evaluated files

## 1.1.5
  - Support for OTP 23

## 1.1.4
  - Fixed incorrect anonymous function name when raising exception

## 1.1.3
  - Fixed bindings leaking into function defined locally, but called externally

## 1.1.2
  - Added cover (Coverage Analysis Tool for Erlang) support, evaluated functions are now
    displayed as 'covered'

## 1.1.1:
  - Fixed handling of try ... catch clauses

## 1.1.0:
  - Added support for cover-compiled files
  - Added support for loading *.erl files when *.beam files are missing
  - Added support for preloaded *.beam files (prim_file, erlang, ...)
  - Added support for built-in functions called directly (e.g. erlang:self()).

## 1.0.0:
  - Initial release
  - Implemented calling non-exported function
  - Fixed erl_eval deficiency (existing in escript as well, inability to make local calls for `fun local_name/Arity`)
  - Added shell integration
