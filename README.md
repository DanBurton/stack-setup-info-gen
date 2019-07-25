This project provides an executable that, given a ghc version string,
can generate a stack.yaml which will allow you to use stack to install
that version of GHC.

Sample usage, from the project root:

```bash
stack run ghc-8.6.1-beta1
```

If the input files don't already exist in the repo,
you can add the --cache-local flag to fetch them.
(Upon reflection, this is an awkwardly named flag.)

```bash
stack run -- ghc-8.6.5 --cache-local
```

If the script doesn't know how to "discover ghc version",
implement `discoverDateVer` the right way, or just add the translation
by hand.

The output of this script, for some ghc versions, is included in this repo
under the output/ folder.

This project is still somewhat experimental.
Be cautious about relying on it behaving consistently over time.
