#if 0
* This pilot patch was created from kernfor.car patch _kuux
#endif
#if 0
*               pilot for an unknown UNIX machine
#endif
#ifndef CERNLIB_QMUUX
#define CERNLIB_QMUUX
#endif
#if !defined(CERNLIB_B64)
#ifndef CERNLIB_B32
#define CERNLIB_B32
#endif
#endif
#if !defined(CERNLIB_A8)
#ifndef CERNLIB_A4
#define CERNLIB_A4
#endif
#endif
#ifndef CERNLIB_HEX
#define CERNLIB_HEX
#endif
#if 0
*               running Unix
#endif
#ifndef CERNLIB_QS_UNIX
#define CERNLIB_QS_UNIX
#endif
#if 0
*                 IEEE floating point representation
#endif
#ifndef CERNLIB_QIEEE
#define CERNLIB_QIEEE
#endif
#if 0
*               ISA standard routines, ISHFT, IOR, etc
#endif
#ifndef CERNLIB_QISASTD
#define CERNLIB_QISASTD
#endif
#if 0
*               Posix call for setjmp/longjmp
#endif
#ifndef CERNLIB_QSIGJMP
#define CERNLIB_QSIGJMP
#endif
#ifndef CERNLIB_QDEFAULT
#define CERNLIB_QDEFAULT
#endif
#if !defined(CERNLIB_P5BOOT)
#ifndef CERNLIB_CCGEN
#define CERNLIB_CCGEN
#endif
#ifndef CERNLIB_CCGENCX
#define CERNLIB_CCGENCX
#endif
#ifndef CERNLIB_CCGENCF
#define CERNLIB_CCGENCF
#endif
#ifndef CERNLIB_CCGENCI
#define CERNLIB_CCGENCI
#endif
#endif
#if !defined(CERNLIB_P5BOOT)
#ifndef CERNLIB_XVECT
#define CERNLIB_XVECT
#endif
#ifndef CERNLIB_TCGEN
#define CERNLIB_TCGEN
#endif
#endif
#if defined(CERNLIB_P5BOOT)
#ifndef CERNLIB_P5_CC
#define CERNLIB_P5_CC
#endif
#ifndef CERNLIB_P5_CI
#define CERNLIB_P5_CI
#endif
#endif
#if 0
*   possible other options
#endif
#if !defined(CERNLIB__KUUX)
#if 0
.USE, QXNO_SC.               external names without underscores
#endif
#if 0
.USE, QXCAPT.                external names capital
#endif
#if 0
.USE, QSYSBSD.               running Unix system BSD
#endif
#if 0
.USE, QENVBSD.               BSD version for SETENVF
#endif
#if 0
.USE, QSIGBSD.               signal handling with BSD sigvec
#endif
#if 0
.USE, QSIGPOSIX.             signal handling with Posix sigaction
#endif
#if 0
.USE, QGETCWD.               system has no getwd but getcwd
#endif
#if 0
.USE, QMILSTD.               MIL standard routines, IBITS, MVBITS, etc
#endif
#if 0
.USE, QCCINDAD.              Double indirect adr for externals
#endif
#if 0
.USE, QORTHOLL, T=INH.       Hollerith storage not orthodox
#endif
#endif
