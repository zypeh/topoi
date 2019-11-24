# topoi


### style guide
This style guide I will put it as a file under `docs/`.

* Use only these two forms of import:
    ```hs
    import qualified Very.Special.Module as VSM
    import Another.Important.Module (printf, (<|>), )
    ```
    According to the [Haskell wiki - Import modules properly](https://wiki.haskell.org/Import_modules_properly).

* Follow the hlint.yamk.