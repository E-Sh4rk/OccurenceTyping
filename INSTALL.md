# Compiling this project

## The OPAM environment

```
sudo apt install opam
opam init
eval `opam config env`
opam switch 4.07.0
```

## Compiling and installing CDuce

```
eval `opam config env`
git clone -b cduce-next git@gitlab.math.univ-paris-diderot.fr:cduce/cduce.git
cd cduce
sudo apt install m4 libexpat1-dev libcurl4-gnutls-dev pkg-config libpcre3-dev
opam install num ocaml-expat ocurl pxp pcre
./configure --prefix=~/usr/local
make all
make install
cp lib/* ~/.opam/4.07.0/lib/cduce/
```

### Compiling CDuce with js_of_ocaml
```
eval `opam config env`
git clone -b cduce-next git@gitlab.math.univ-paris-diderot.fr:cduce/cduce.git
cd cduce
sudo apt install m4
opam install num js_of_ocaml js_of_ocaml-ppx js_of_ocaml-camlp4
./configure  --without-pxp --without-expat --without-netclient --without-netstring --prefix=~/usr/local
make all
make install
cp lib/* ~/.opam/4.07.0/lib/cduce/
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

NOTE: there are a lot of dependencies in the file `_tags`. Actually, the only dependency used is `package(cduce)`. Other dependencies are only used by `cduce` itself. Depending on the packages you had when you installed it, your cduce installation may have different dependencies. In this case you can freely add/remove dependencies in the `_tags` file in order to match those of your cduce installation.

## If it does not work

You can still download a minimal virtual machine with everything installed:
https://mega.nz/#!NhJEwI6R!Ig4dqSsr-FUgvdJ5L-Os-m5MC6kYUJ5vWfMQSWclNnk

OS: Ubuntu 18 x64 (need 9 Go on hard disk)  
Format: Oracle VM VirtualBox  
Username: Ubuntu  
Password: ubuntu
