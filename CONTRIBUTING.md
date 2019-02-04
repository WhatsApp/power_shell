# Contributing to power_shell
We want to make contributing to this project as easy and transparent as
possible.

## Our Development Process
We require 100% test coverage.
All exported functions must include -spec(). specification.

## Pull Requests
We actively welcome your pull requests.

1. Fork the repo and create your branch from `master`.
2. Ensure that tests suite run with `rebar3 ct` passes and cover 100% source code.
   Use `rebar3 cover` and examine coverage.
3. If you've changed APIs, update the documentation. Use `rebar3 edoc` to
   auto-generate documentation from source code.
4. Make sure your code compiles with no warnings.
5. Ensure Dialyzer does not report any problems.
6. If you haven't already, complete the Contributor License Agreement ("CLA").

## Contributor License Agreement ("CLA")
In order to accept your pull request, we need you to submit a CLA. You only need
to do this once to work on any of Facebook's open source projects.

Complete your CLA here: <https://code.facebook.com/cla>

## Issues
We use GitHub issues to track public bugs. Please ensure your description is
clear and has sufficient instructions to be able to reproduce the issue.

Facebook has a [bounty program](https://www.facebook.com/whitehat/) for the safe
disclosure of security bugs. In those cases, please go through the process
outlined on that page and do not file a public issue.

## Coding Style
* Use 4 spaces for indentation
* Avoid lines longer that 100 characters
* Do not commit commented-out code or files that are no longer needed

## License
By contributing to power_shell, you agree that your contributions will be licensed
under the LICENSE file in the root directory of this source tree.
