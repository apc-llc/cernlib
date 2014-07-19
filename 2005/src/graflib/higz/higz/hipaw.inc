*
* $Id: hipaw.inc,v 1.2 2001/12/07 14:57:44 couet Exp $
*
* $Log: hipaw.inc,v $
* Revision 1.2  2001/12/07 14:57:44  couet
* - HILINK common block move to the hilink.inc include file.
*
* Revision 1.1.1.1  1996/02/14 13:10:13  mclareni
* Higz
*
*
#ifndef CERNLIB_HIGZ_HIPAW_INC
#define CERNLIB_HIGZ_HIPAW_INC
*
*
* hipaw.inc
*
*-- Author : O.Couet
      COMMON /PAWC/ NWPAW,IXPAWC,IHBOOK,IXHIGZ,IXKU,IFENCE(5)
     +,             LMAIN, IPAW(9989)
      DIMENSION IQ(2),Q(2),LQ(8000)
      EQUIVALENCE (Q(1),IQ(1),LQ(9)),(LQ(1),LMAIN)
      COMMON /QUEST/ IQUEST(100)
      DIMENSION RQUEST(100)
      EQUIVALENCE (IQUEST(1),RQUEST(1))
#include "higz/hilink.inc"
      COMMON /HIPTR/  ITBPTR,INTPTR,IFLPTR,ISTPTR
     +,               IIAPTR,IRAPTR
      COMMON /HISIZE/ INSIZ,IISIZ,IFSIZ,ISSIZ
     +,               IRNSIZ,IRISIZ,IRFSIZ,IRSSIZ
      COMMON /HINUM/  NBPICT,INBNT,INBLEV
#endif
