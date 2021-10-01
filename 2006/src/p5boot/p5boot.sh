#!/bin/sh

mkdir -p $CERN_ROOT/build/p5boot
cd $CERN_ROOT/build/p5boot
cp -p -r $CERN_ROOT/src/p5boot/* .

AR="ar cr"

cd $CERN_ROOT/build/p5boot/p5lib
$FCOMP -c *.f
$AR ../p5lib.a *.o

cd $CERN_ROOT/build/p5boot

$FCOMP -o nypatchy nypatchy.f p5lib.a $CERN_ROOT/lib/libkernlib.a

cd $CERN_ROOT/bin
ln -sf $CERN_ROOT/build/p5boot/nypatchy ypatchy

cd $CERN_ROOT/build
