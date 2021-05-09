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
opam pin add cduce-types 'git+https://gitlab.math.univ-paris-diderot.fr/cduce/cduce#devel'
opam pin add cduce 'git+https://gitlab.math.univ-paris-diderot.fr/cduce/cduce#devel'
opam install cduce
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


## If it does not work

You can still download a minimal virtual machine with everything installed:
https://mega.nz/#!NhJEwI6R!Ig4dqSsr-FUgvdJ5L-Os-m5MC6kYUJ5vWfMQSWclNnk

OS: Ubuntu 18 x64 (need 9 Go on hard disk)
Format: Oracle VM VirtualBox
Username: Ubuntu
Password: ubuntu
