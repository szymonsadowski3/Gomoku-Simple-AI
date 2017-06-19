# GOMOKU AI

This project is an attempt to make simple AI capable of playing Gomoku game.

## Requirements

This project was developed with Stack (https://www.haskellstack.org/). Other prerequisites are listed in .cabal file. While building project, stack should install them automatically.

### Prerequisites

Current listing from cabal file (should be installed automatically while building with stack): 

```
split, universe-base, process
```

### Installing

```
git clone https://github.com/szymonsadowski3/Gomoku-Simple-AI.git
cd Gomoku-Simple-AI/Gomoku/
stack build
stack exec Gomoku-exe
```

### How to run

After succesfull build, please run this command:

```
stack exec Gomoku-exe
```

### Important notes

Playing Gomoku on 19x19 board is very computationally-expensive, so please be patient while waiting for AI's next move :P

### Example run for 19x19

![run19x19](https://i.snag.gy/2QVpuy.jpg "Screen")

