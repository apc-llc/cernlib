#if 0
* This pilot patch was created from kerngen.car patch _kerngen
#endif
#if defined(CERNLIB_IBMVM)||defined(CERNLIB_IBMMVS)
#ifndef CERNLIB_IBM
#define CERNLIB_IBM
#endif
#ifndef CERNLIB_F77
#define CERNLIB_F77
#endif
#endif
#if (defined(CERNLIB_CONVEX))&&(defined(CERNLIB_SINGLE))
#ifndef CERNLIB_CONVEX64
#define CERNLIB_CONVEX64
#endif
#endif
#if (defined(CERNLIB_CONVEX))&&(!defined(CERNLIB_CONVEX64))
#ifndef CERNLIB_CONVEX32
#define CERNLIB_CONVEX32
#endif
#endif
#if (defined(CERNLIB_IBMVM))&&(defined(CERNLIB_CERN)||defined(CERNLIB_LEPICS)||defined(CERNLIB_SLAC))
#ifndef CERNLIB_SLACBATCH
#define CERNLIB_SLACBATCH
#endif
#endif
#if (defined(CERNLIB_ALLIANT))&&(defined(CERNLIB_QMINTEL))
#ifndef CERNLIB__KALI
#define CERNLIB__KALI
#endif
#endif
#if (defined(CERNLIB_ALLIANT))&&(!defined(CERNLIB_QMINTEL))
#ifndef CERNLIB__KALT
#define CERNLIB__KALT
#endif
#endif
#if defined(CERNLIB_AMIGAUX)
#ifndef CERNLIB__KAMX
#define CERNLIB__KAMX
#endif
#endif
#if (defined(CERNLIB_APOLLO))&&(!defined(CERNLIB_APOF77))
#ifndef CERNLIB__KAPO
#define CERNLIB__KAPO
#endif
#endif
#if (defined(CERNLIB_APOLLO))&&(defined(CERNLIB_APOF77))
#ifndef CERNLIB__KAPOF77
#define CERNLIB__KAPOF77
#endif
#endif
#if defined(CERNLIB_CDC)
#ifndef CERNLIB__KCDC
#define CERNLIB__KCDC
#endif
#endif
#if defined(CERNLIB_CRAY)
#ifndef CERNLIB__KCRU
#define CERNLIB__KCRU
#endif
#endif
#if defined(CERNLIB_CONVEX64)
#ifndef CERNLIB__KCVX
#define CERNLIB__KCVX
#endif
#endif
#if defined(CERNLIB_CONVEX32)
#ifndef CERNLIB__KCVY
#define CERNLIB__KCVY
#endif
#endif
#if defined(CERNLIB_DGE)
#ifndef CERNLIB__KDGE
#define CERNLIB__KDGE
#endif
#endif
#if defined(CERNLIB_MSDOS)
#ifndef CERNLIB__KDOS
#define CERNLIB__KDOS
#endif
#endif
#if (defined(CERNLIB_DECS))&&(!defined(CERNLIB_QMVAOS))
#ifndef CERNLIB__KVMI
#define CERNLIB__KVMI
#endif
#endif
#if (defined(CERNLIB_DECS))&&(defined(CERNLIB_QMVAOS))
#ifndef CERNLIB__KVAOS
#define CERNLIB__KVAOS
#endif
#endif
#if defined(CERNLIB_HPUX)
#ifndef CERNLIB__KHPX
#define CERNLIB__KHPX
#endif
#endif
#if defined(CERNLIB_IBMAIX)
#ifndef CERNLIB__KIBX
#define CERNLIB__KIBX
#endif
#endif
#if defined(CERNLIB_IBMMVS)
#ifndef CERNLIB__KIMVSSP
#define CERNLIB__KIMVSSP
#endif
#endif
#if defined(CERNLIB_IBMRT)
#ifndef CERNLIB__KIRT
#define CERNLIB__KIRT
#endif
#endif
#if (defined(CERNLIB_IBMVM))&&(defined(CERNLIB_CERN))
#ifndef CERNLIB__KICERNV
#define CERNLIB__KICERNV
#endif
#endif
#if (defined(CERNLIB_IBMVM))&&(!defined(CERNLIB_SLACBATCH))
#ifndef CERNLIB__KIVM
#define CERNLIB__KIVM
#endif
#endif
#if (defined(CERNLIB_IBM))&&(!defined(CERNLIB_CERN))
#ifndef CERNLIB__KIBMVS
#define CERNLIB__KIBMVS
#endif
#endif
#if defined(CERNLIB_LINUX)
#ifndef CERNLIB__KLNX
#define CERNLIB__KLNX
#endif
#endif
#if defined(CERNLIB_MACMPW)
#ifndef CERNLIB__KMPW
#define CERNLIB__KMPW
#endif
#endif
#if defined(CERNLIB_NORD500)
#ifndef CERNLIB__KNORD3
#define CERNLIB__KNORD3
#endif
#endif
#if defined(CERNLIB_NECSX)
#ifndef CERNLIB__KNSX
#define CERNLIB__KNSX
#endif
#endif
#if defined(CERNLIB_NEXT)
#ifndef CERNLIB__KNXT
#define CERNLIB__KNXT
#endif
#endif
#if 0
* Added at release 94B
#endif
#if defined(CERNLIB_OS9)
#ifndef CERNLIB__KOS9
#define CERNLIB__KOS9
#endif
#endif
#if defined(CERNLIB_SGI)
#ifndef CERNLIB__KSGI
#define CERNLIB__KSGI
#endif
#endif
#if defined(CERNLIB_SUN)
#ifndef CERNLIB__KSUN
#define CERNLIB__KSUN
#endif
#endif
#if defined(CERNLIB_TMO)
#ifndef CERNLIB__KTMO
#define CERNLIB__KTMO
#endif
#endif
#if (defined(CERNLIB_VAXVMS))&&(!defined(CERNLIB_QMALPH))
#ifndef CERNLIB__KVAX
#define CERNLIB__KVAX
#endif
#endif
#if (defined(CERNLIB_VAXVMS))&&(defined(CERNLIB_QMALPH))
#ifndef CERNLIB__KALPH
#define CERNLIB__KALPH
#endif
#endif
#if defined(CERNLIB_IBM)
#ifndef CERNLIB_QMIBMXA
#define CERNLIB_QMIBMXA
#endif
#endif
#if (defined(CERNLIB_IBMVM))&&(defined(CERNLIB_CERN))
#ifndef CERNLIB_3090J
#define CERNLIB_3090J
#endif
#endif
#if defined(CERNLIB_CRAY)
#ifndef CERNLIB_CRAYC
#define CERNLIB_CRAYC
#endif
#endif
#if defined(CERNLIB_QX_SC)
#ifdef CERNLIB_QXNO_SC
#undef CERNLIB_QXNO_SC
#endif
#endif
#if defined(CERNLIB_QXNO_SC)
#ifdef CERNLIB_QX_SC
#undef CERNLIB_QX_SC
#endif
#endif
#if defined(CERNLIB_QXNO_SC)||defined(CERNLIB_QX_SC)
#ifdef CERNLIB_QXCAPT
#undef CERNLIB_QXCAPT
#endif
#endif
#if defined(CERNLIB_APOLLO)
#ifndef CERNLIB_QSYSBSD
#define CERNLIB_QSYSBSD
#endif
#endif
#if defined(CERNLIB_SHIFT)
#ifndef CERNLIB_PROJSHIFT
#define CERNLIB_PROJSHIFT
#endif
#endif
