*
* $Id: tcnum.h,v 1.1.1.1 1996/02/15 17:54:16 mclareni Exp $
*
* $Log: tcnum.h,v $
* Revision 1.1.1.1  1996/02/15 17:54:16  mclareni
* Kernlib
*
*
* This directory was created from kernfor.car patch tcnum
#if !defined(CERNLIB_TCNUM)

      GENSEC ROUTINES EXTRACTED FROM KERNNUM PAM
      THIS PATCH SHOULD ONLY BE +USE'ED TO BUILD A GENSEC LIBRARY
      BUT NOT FOR MAKING A FULL KERNLIB

#endif
#if defined(CERNLIB_B32)
#ifndef CERNLIB_NUMLOPRE
#define CERNLIB_NUMLOPRE
#endif
#endif
#if defined(CERNLIB_B60M)
#ifndef CERNLIB_NUME293
#define CERNLIB_NUME293
#endif
#endif
#if defined(CERNLIB_QMND3)||defined(CERNLIB_QMVAX)
#ifndef CERNLIB_NUME38
#define CERNLIB_NUME38
#endif
#endif
#if !defined(CERNLIB_NUMLOPRE)
#ifndef CERNLIB_NUMHIPRE
#define CERNLIB_NUMHIPRE
#endif
#endif
#if (!defined(CERNLIB_NUME38))&&(!defined(CERNLIB_NUME293))
#ifndef CERNLIB_NUME75
#define CERNLIB_NUME75
#endif
#endif
