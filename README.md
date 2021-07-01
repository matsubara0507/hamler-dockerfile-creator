# hamler-dockerfile-creator

usage:

```
stack build
stack exec -- hdc --hamler=0.4 > example/Dockerfile

cd example
docker build -t hamler .
docker run -it --rm hamler repl
```