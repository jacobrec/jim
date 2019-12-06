# jtodo ord-mode

this plugin is like emacs' org-mode but for
[jtodo](github.com/jacobrec/jtodo).

## installation

make sure this folder is in your asdf search path and add the
following line to your `~/.jimrc`

```lisp
(ql:quickload :jtodo-org :silent t)
```

## usage

jtodo-org automatically saves changes to your jtodo list

to start jtodo, run `:jtodo`. to quit run `:q`

keybindings

| key | effect |
|-----|--------|
| `j` | next item |
| `k` | previous item |
| `n` | new item |
| `c` | clear finished item |
| `<ret>` | toggle doneness of current item |