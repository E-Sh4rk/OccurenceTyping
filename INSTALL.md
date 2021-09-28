# Compiling this project

## The OPAM environment

```
sudo apt install opam
opam init
eval `opam config env`
opam switch 4.07.1
```

## Compiling and installing CDuce

```
eval `opam config env`
opam install ocamlfind num pxp dune
opam pin add cduce-types 'git+https://gitlab.math.univ-paris-diderot.fr/cduce/cduce#polymorphic'
opam pin add cduce 'git+https://gitlab.math.univ-paris-diderot.fr/cduce/cduce#polymorphic'
```


## Compiling the project

```
eval `opam config env`
git clone https://github.com/E-Sh4rk/OccurenceTyping.git
cd OccurenceTyping
opam install menhir
make
./main.native
```
