# glossAVL

Visualization of AVL Trees using Gloss.

![GlossAVL GIF](https://media.giphy.com/media/cChJPFbarNFSvdHwGs/giphy.gif)

## Example

Using the library is as easy as:

```haskell
import GlossAVL
main = do
  model <- initModel :: IO (Model Int) -- typecast to any (Ord,Read,Show) type
  run model
```

To compile and run this project you'll need [stack](https://docs.haskellstack.org/):

```
$ stack setup
$ stack build
$ stack exec glossAVL-exe < test/tree.in
```

## Input format

The input is a sequence of commands to perform on an initially empty
AVL tree. Supported tree operations are the insertion and removal of
values and the corresponding commands are `+k` and `-k`.
Invalid input is ignored.

Below an example of valid input for a model of type `Int`:

```
+50
+25
+75
+30
+60
-30
+20
+80
-50
```

You can find input examples for bigger trees in `test/`.