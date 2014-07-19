*
* $Id: sitrac.F,v 1.1.1.1 1995/12/12 14:36:20 mclareni Exp $
*
* $Log: sitrac.F,v $
* Revision 1.1.1.1  1995/12/12 14:36:20  mclareni
* Imported sources
*
*
#include "sigma/pilot.h"
*CMZ :  1.09/00 23/07/93  11.28.13  by  Carlo E. Vandoni
*-- Author :
      SUBROUTINE SITRAC(C)
C
C
C
C    ....................................................
C
C
C         PURPOSE
C            GENERAL TRACING
C
C         USAGE
C            CALL SITRAC(' XXXXXX  ')
C
C         DESCRIPTION OF PARAMETERS
C            NAME OF CALLIN GROUTINE
C
C         COMM. BLOCKS USED
C            COM1
C            COMVAR USED: INAME(),ITAM
C            CNAME,ITYPE,IPOINT,ISTAPO,IADDR,DYNA(IADDR)
C
C         SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED
C            ITRAK
C
C...  PAW VERSION  ...  MAY 1988
C
C    ......................................................
C
C
C     ITRAK =  1   FOR INAME PRINTOUT
C     ITRAK =  2
C     ITRAK =  3
C     ITRAK =  4   FOR COMPILER TRACING: COMPILE,DIS,GETAREA,RELAREA
C     ITRAK =  5   SICMPR,SIINPS,OUTPUT
C     ITRAK =  6   SICOMA
C     ITRAK =  7   SICOMA, MAINLO
C     ITRAK = 10   SITRAX
C     ITRAK = 12   COMPIL,OUTPUT
C     ITRAK = 20   GENERAL TRACING
C     ITRAK = 30
C     ITRAK = 33   SIASSI,
C     ITRAK = 34   SINDXI
C     ITRAK = 36   NUMCOD
C     ITRAK = 37   SIDROP,ORDER,TRACOP
C     ITRAK = 38   SIDROP,ORDER,TRACOP,SUMN
C     ITRAK = 49   SIDRTM AND IN RETEND TO CALL DUMP
C
#include "sigma/sicsig.inc"
#include "sigma/sigc.inc"
#include "sigma/pawc.inc"
C
      CHARACTER*(*) C
C

      IF(SITRAK(3).EQ.1)  PRINT *,C,MODE
      IF(SITRAK(19).EQ.1) CALL MZLOGL (0,4)
      IF(SITRAK(20).EQ.0) GOTO 999
      PRINT *,C,CNAME,ITYPE,ISTAPO,KIT,IADDR
C
 999  END
