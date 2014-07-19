*
* $Id: xzftoc.F,v 1.5 1998/09/25 09:24:57 mclareni Exp $
*
* $Log: xzftoc.F,v $
* Revision 1.5  1998/09/25 09:24:57  mclareni
* Modifications for the Mklinux port flagged by CERNLIB_PPC
*
* Revision 1.4  1997/10/23 13:26:24  mclareni
* NT mods
*
* Revision 1.3  1997/09/02 08:46:26  mclareni
* WINNT mods, mostly cpp defines
*
* Revision 1.2  1997/01/17 08:56:15  gunter
* call vxinvb for linux too.
*
* Revision 1.1.1.1  1996/03/08 15:44:30  mclareni
* Cspack
*
*
#include "cspack/pilot.h"
      SUBROUTINE XZFTOC(CHFIN,CHFOUT,LRECL,CHOPT,IRC)
 
      CHARACTER*(*) CHFIN,CHFOUT
      CHARACTER*4   CHOPI,CHOPO
#include "cspack/hcmail.inc"
#include "cspack/czunit.inc"
#include "cspack/czsock.inc"
#include "cspack/zmach.inc"
#include "cspack/pawc.inc"
      DIMENSION IQ(2),Q(2),LQ(8000)
      EQUIVALENCE (LQ(1),LMAIN),(IQ(1),LQ(9)),(Q(1),IQ(1))
      DIMENSION    ITEST(5)
      DIMENSION    BUFFER(8192)
#if defined(CERNLIB_IPSC)
      DATA         IPATT/'0123CDEF'X/
#elif defined(CERNLIB_WINNT)
      DATA         IPATT/Z'0123CDEF'/
#elif defined(CERNLIB_DECS) || defined(CERNLIB_LINUX) 
      DATA         IPATT/X'0123CDEF'/
#elif defined(CERNLIB_VAX)
      DATA         IPATT/Z0123CDEF/
#endif
#include "cspack/quest.inc"
#include "cspack/czopts.inc"
 
      IRC    = 0
 
      LCHFI  = LENOCC(CHFIN)
      LCHFO  = LENOCC(CHFOUT)
      LCHOPT = LENOCC(CHOPT)
 
      IF(IDEBXZ.GE.1) PRINT *,'XZFTOC. enter for ',
     +   CHFIN(1:LCHFI),' ',CHFOUT(1:LCHFO),' ',
     +   LRECL,' ',CHOPT
 
      IF(LCHFI.EQ.0.OR.LCHFO.EQ.0) THEN
         IF(IDEBXZ.GE.0) PRINT *,'XZFTOC. error - input or ',
     +      'output file name missing'
         IRC = -1
         GOTO 99
      ENDIF
 
      CHOPI    = 'I'
      IF(IOPTC.NE.0) CHOPI = 'CI'
      CHOPO    = 'DON'
      IF(IOPTC.NE.0) CHOPO = 'CDON'
 
      IF(IOPTR.NE.0) THEN
         LCHOPO = LENOCC(CHOPO)
         CHOPO(LCHOPO:LCHOPO) = ' '
      ENDIF
 
      IF(LRECL.LE.0) THEN
         IF(IOPTX.EQ.0) THEN
            IF(IDEBXZ.GE.-3) PRINT *,'XZFTOC. input record length ',
     +         'must be specified for non-Zebra exchange format files'
            IRC = -1
            RETURN
         ELSE
*
*     Get record length from file
*
            JRECL = 80
            IF(IDEBXZ.GE.3) PRINT *,'XZFTOC. call SZOPEN for ',
     +         CHFIN(1:LCHFI),JRECL,CHOPI
            CALL SZOPEN(LUNXZI,CHFIN(1:LCHFI),JRECL,CHOPI,IRC)
            IF(IRC.NE.0) THEN
               IF(IDEBXZ.GE.-3) PRINT *,'XZFTOC. error opening ',
     +            'input file, IRC = ',IRC
               GOTO 99
            ENDIF
 
            READ(LUNXZI,IOSTAT=ISTAT) ITEST
            IF(ISTAT.NE.0) THEN
               PRINT *,'XZFTOC. error ',ISTAT,' reading input file'
               CLOSE(LUNXZI)
               GOTO 99
            ENDIF
 
#if defined(CERNLIB_VAX)||defined(CERNLIB_DECS) || (defined(CERNLIB_LINUX) && !defined(CERNLIB_PPC)) || defined(CERNLIB_WINNT)
            IF(ITEST(1).NE.IPATT) CALL VXINVB(ITEST(5),1)
#endif
#if !defined(CERNLIB_CRAY)
            JRECL = JBYT(ITEST(5),1,24) * 4
#endif
#if defined(CERNLIB_CRAY)
            JRECL = JBYT(ITEST(3),1,24) * 4
#endif
*
*     Check if JRECL is reasonable
*
            IF(JRECL.GT.32756.OR.JRECL.LT.0) THEN
               PRINT *,'XZFTOC. cannot determine record length of ',
     +            'input file. How was this file created?'
               IRC = JRECL
               CLOSE(LUNXZI)
               GOTO 99
            ENDIF
 
            IF(IDEBXZ.GE.2) PRINT *,'XZFTOC. record length of input ',
     +         'file is ',JRECL,' bytes'
*
*     Close input file
*
            CLOSE(LUNXZI)
         ENDIF
      ELSE
         JRECL = LRECL
      ENDIF
 
      CALL SZOPEN(LUNXZI,CHFIN(1:LCHFI),JRECL,CHOPI,IRC)
 
      IF(IDEBXZ.GE.3) PRINT *,'XZFTOC. call SZOPEN for ',
     +   CHFOUT(1:LCHFO),JRECL,CHOPO
      CALL SZOPEN(LUNXZO,CHFOUT(1:LCHFO),JRECL,CHOPO,IRC)
 
      IF(IRC.EQ.28) THEN
         IF(IDEBXZ.GE.0) PRINT *,'XZFTOC. file ',CHFOUT(1:LCHFO),
     +      ' already exists - specify R option to replace'
         CLOSE(LUNXZI)
         RETURN
      ENDIF
 
      NWORDS = JRECL / IQCHAW
 
      CALL XZCONV(LUNXZI,LUNXZO,BUFFER,NWORDS,'C',IRC)
 
      CLOSE(LUNXZI)
      CLOSE(LUNXZO)
 
99    CONTINUE
      END
