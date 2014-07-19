![cern.jpg](cern.jpg)

### Dependencies

```
sudo apt-get install xutils-dev makedepend libmotif-dev
```

### 32-bit build

In `build32` select which version to build - `2005` or `2006` and then run:

```
$ ./build32
```

### 64-bit build

In `build64` select which version to build - `2005` or `2006` and then run:

```
$ ./build64
```

### Debugging

In order to enable debugging, add `CERN\_DEBUG` environment variable, for example:

```
$ CERN_DEBUG=1 ./build32
```
