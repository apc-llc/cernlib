![cern.jpg](cern.jpg)

# CERN Program Library

This is an unofficial fork of http://cernlib.web.cern.ch/cernlib/

The CERN Program Library is a large collection of general purpose libraries and modules maintained and offered in both source and object code form on the CERN central computers. The two most popular applications based on CERNLIB are PAW and GEANT 3.21. Most of these programs were developed at CERN and are therefore oriented towards the needs of a physics research laboratory that is general mathematics, data analysis, detectors simulation, data-handling etc... applicable to a wide range of problems. Some tools like cfortran.h are developed outside CERN.

The development and support for CERNLIB has been discontinued. Libraries will be continued to be provided "as is"

The purpose of this fork is to bring up support for building CERNLIB on modern Linux systems with modern GNU compiler toolchain. This setup has been tested on Ubuntu 12.04, gfortran 4.6.3.

### Dependencies

```
sudo apt-get install xutils-dev libmotif-dev libxt-dev libxft-dev
```

### 32-bit build

Run `build32` choosing which version to build - `2005` or `2006`:

```
$ ./build32 2006
```

Resulting libraries will be placed into `2005/lib` or `2006/lib`.

### 64-bit build

Run `build64` choosing which version to build - `2005` or `2006`:

```
$ ./build64 2006
```

Resulting libraries will be placed into `2005/lib` or `2006/lib`.

### Debugging

In order to enable debugging, add `CERN\_DEBUG` environment variable, for example:

```
$ CERN_DEBUG=1 ./build32
```
