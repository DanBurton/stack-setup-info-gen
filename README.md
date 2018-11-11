--- Under Construction ---

This project provides an executable that, given a ghc version string,
can generate a stack.yaml which will allow you to use stack to install
that version of GHC.

Sample usage, from the project root:

```bash
stack run ghc-8.6.1-beta1
```

The output of this script, for some ghc versions, is included in this repo
under the output/ folder.

This project is still very experimental.
Please don't rely on it behaving consistently over time.
