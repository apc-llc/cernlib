#if 0
* This pilot patch was created from kernibm.car patch _kivm
#endif
#if 0
*       Pilot IBM, system VM/CMS
#endif
#ifndef CERNLIB_SYVM
#define CERNLIB_SYVM
#endif
#if defined(CERNLIB_SIEMENS)
#ifndef CERNLIB_QF_SIEM
#define CERNLIB_QF_SIEM
#endif
#endif
#if !defined(CERNLIB_SIEMENS)
#ifndef CERNLIB_QF_IBM
#define CERNLIB_QF_IBM
#endif
#endif
#if defined(CERNLIB_QF_SIEM)
#ifndef CERNLIB_XVECTSI
#define CERNLIB_XVECTSI
#endif
#endif
#ifndef CERNLIB_XVECTIBM
#define CERNLIB_XVECTIBM
#endif
#if 0
*       inhibit this if you have only 360 instructions
#endif
#ifndef CERNLIB_IBM370
#define CERNLIB_IBM370
#endif
#ifndef CERNLIB_IBM360
#define CERNLIB_IBM360
#endif
#ifndef CERNLIB_XVECT
#define CERNLIB_XVECT
#endif
#ifndef CERNLIB_TCGEN
#define CERNLIB_TCGEN
#endif
#ifndef CERNLIB_SYCERN_ABEND
#define CERNLIB_SYCERN_ABEND
#endif
