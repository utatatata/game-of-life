# Game of Life


## Requirements

- The Haskell Tool Stack Version 2.1.3 (Resolver LTS-14.14)
- GLUT (freeglut 3.2.1-1)


## Build & Run

```
$ stack build

$ stack exec life -- --help

# or

$ stack run -- --help
```

## Usage

```
$ stack run -- --help
```

e.g.

```
$ stack run -- ./resources/gosper.txt
```

```
$ stack run -- ./resources/glider.txt --gui
```


## References

[コモナドを使った抽象化の威力をライフゲームで試してみた](https://qiita.com/lotz/items/fbc4788b8ea8313cbf76){:target="_blank"}
