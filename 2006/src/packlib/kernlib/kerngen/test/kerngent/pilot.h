#if 0
* This pilot patch was created from kerngent.car patch _kerngent
*           Interface pilot to Micky
#endif
#if defined(CERNLIB_IBMVM)||defined(CERNLIB_IBMMVS)
#ifndef CERNLIB_IBM
#define CERNLIB_IBM
#endif
#endif
#if defined(CERNLIB_VAXVMS)||defined(CERNLIB_VAXULX)
#ifndef CERNLIB_VAX
#define CERNLIB_VAX
#endif
#endif
#if 0
*     for DECstation (VAX/MIPS)
#endif
#if defined(CERNLIB_DECS)
#ifndef CERNLIB_QMVMI
#define CERNLIB_QMVMI
#endif
#ifndef CERNLIB_QUNIX
#define CERNLIB_QUNIX
#endif
#endif
#if 0
*  for VAX Alpha OSF
#endif
#if (defined(CERNLIB_ALPHA))&&(defined(CERNLIB_OSF))
#ifndef CERNLIB_QMVAO
#define CERNLIB_QMVAO
#endif
#ifndef CERNLIB_QUNIX
#define CERNLIB_QUNIX
#endif
#endif
#if 0
*     for HP Unix work station
#endif
#if defined(CERNLIB_HPUX)
#ifndef CERNLIB_QMHPX
#define CERNLIB_QMHPX
#endif
#ifndef CERNLIB_QUNIX
#define CERNLIB_QUNIX
#endif
#endif

#if 0
*    for IBM Risc 6000
#endif
#if defined(CERNLIB_IBMRT)
#ifndef CERNLIB_QMIRT
#define CERNLIB_QMIRT
#endif
#ifndef CERNLIB_QUNIX
#define CERNLIB_QUNIX
#endif
#endif
#if 0
*    for PC with Linux
#endif
#if defined(CERNLIB_LINUX)
#ifndef CERNLIB_QMLNX
#define CERNLIB_QMLNX
#endif
#ifndef CERNLIB_QUNIX
#define CERNLIB_QUNIX
#endif
#endif
#if 0
*    for PC with MSDOS 
#endif
#if defined(CERNLIB_MSDOS)
#ifndef CERNLIB_QMDOS
#define CERNLIB_QMDOS
#endif
#ifndef CERNLIB_QUNIX
#define CERNLIB_QUNIX
#endif
#endif
#if 0
*      for Microware OS-9
#endif
#if defined(CERNLIB_OS9)
#ifndef CERNLIB_QMOS9
#define CERNLIB_QMOS9
#endif
#ifndef CERNLIB_QUNIX
#define CERNLIB_QUNIX
#endif
#endif
#if 0
*  for PC with WINDOWS/NT
#endif
#if defined(CERNLIB_WINNT)
#ifndef CERNLIB_QMDOS
#define CERNLIB_QMDOS
#endif
#ifndef CERNLIB_QUNIX
#define CERNLIB_QUNIX
#endif
#endif
#if defined(CERNLIB_MACMPW)
#ifndef CERNLIB_QMMPW
#define CERNLIB_QMMPW
#endif
#ifndef CERNLIB_QUNIX
#define CERNLIB_QUNIX
#endif
#endif
#if 0
*      for Silicon Graphics
#endif
#if defined(CERNLIB_SGI)
#ifndef CERNLIB_QMSGI
#define CERNLIB_QMSGI
#endif
#ifndef CERNLIB_QUNIX
#define CERNLIB_QUNIX
#endif
#endif
#if 0
*      for SUN work station
#endif
#if defined(CERNLIB_SUN)
#ifndef CERNLIB_QMSUN
#define CERNLIB_QMSUN
#endif
#ifndef CERNLIB_QUNIX
#define CERNLIB_QUNIX
#endif
#endif
#if 0
*                  for DEC VAX
#endif
#if defined(CERNLIB_VAX)
#if 0
* This pilot patch was created from kerngent.car patch _mvax
*    For DEC VAX
#endif
#ifndef CERNLIB_QMVAX
#define CERNLIB_QMVAX
#endif
#endif
#if 0
* This pilot patch was created from kerngent.car patch _mnorm
*   for most machines
#endif
#ifndef CERNLIB_A4
#define CERNLIB_A4
#endif
#ifndef CERNLIB_B32
#define CERNLIB_B32
#endif
#if 0
* This pilot patch was created from kerngent.car patch _micky
#endif
#ifndef CERNLIB_CDE
#define CERNLIB_CDE
#endif
#if 0
*         Character set is ASCII
#endif
#if !defined(CERNLIB_QEBCDIC)
#ifndef CERNLIB_QASCII
#define CERNLIB_QASCII
#endif
#endif
#if 0
*                       Hollerith exists
#endif
#ifndef CERNLIB_QHOLL
#define CERNLIB_QHOLL
#endif
#if 0
*          EQUIVALENCE Hollerith/Character ok
#endif
#if defined(CERNLIB_QHOLL)
#ifndef CERNLIB_EQUHOLCH
#define CERNLIB_EQUHOLCH
#endif
#endif
#if 0
*                      do test VXINVB/C
#endif
#ifndef CERNLIB_TVXINV
#define CERNLIB_TVXINV
#endif
