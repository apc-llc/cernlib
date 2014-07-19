*
* $Id: xzrtof.F,v 1.2 1998/07/10 10:51:42 jamie Exp $
*
* $Log: xzrtof.F,v $
* Revision 1.2  1998/07/10 10:51:42  jamie
* LRECL undefined - problems transferring multiple files with different LRECL
*
* Revision 1.1.1.1  1996/03/08 15:44:32  mclareni
* Cspack
*
*
#include "cspack/pilot.h"
      SUBROUTINE XZRTOF(CHRZ,CHFZ,IRECL,CHOPT,IRC)
 
      CHARACTER*(*) CHRZ,CHFZ
      CHARACTER*4   CHOPO,CHOPF
 
#include "cspack/hcmail.inc"
#include "cspack/czunit.inc"
#include "cspack/czsock.inc"
#include "cspack/zmach.inc"
#include "cspack/pawc.inc"
      CHARACTER*8  RZOPTS
      DIMENSION IQ(2),Q(2),LQ(8000)
      EQUIVALENCE (LQ(1),LMAIN),(IQ(1),LQ(9)),(Q(1),IQ(1))
#include "cspack/quest.inc"
#include "cspack/czopts.inc"
 
      IRC    = 0
      LCHRZ  = LENOCC(CHRZ)
      LCHFZ  = LENOCC(CHFZ)
      LCHOPT = LENOCC(CHOPT)
 
      IF(LCHRZ.EQ.0.OR.LCHFZ.EQ.0) THEN
         IF(IDEBXZ.GE.0) PRINT *,'XZRTOF. error - input or ',
     +      'output file name missing'
         IRC = -1
         GOTO 99
      ENDIF
 
      IF(IDEBXZ.GE.1) PRINT *,'XZRTOF. enter for ',CHRZ(1:LCHRZ),
     +   ' ',CHFZ(1:LCHFZ),' ',CHOPT(1:LCHOPT)
*
*     Open input file
*
      LOPTS  = 0
      RZOPTS = ' '
      LRECL  = 0
 
      IF(IOPTC.NE.0) THEN
         LOPTS = LOPTS + 1
         RZOPTS(LOPTS:LOPTS) = 'P'
      ENDIF
 
      IF(IOPTX.NE.0) THEN
         LOPTS = LOPTS + 1
         RZOPTS(LOPTS:LOPTS) = 'X'
      ENDIF
 
      CALL RZOPEN(LUNXZI,'RZ',CHRZ(1:LCHRZ),RZOPTS,LRECL,IRC)
 
      IF(IRC.NE.0) THEN
         IF(IDEBXZ.GE.0) PRINT *,'XZRTOF. error from RZOPEN = ',IRC
         GOTO 99
      ENDIF
 
      RZOPTS = ' '
      IF(IOPTX.NE.0) RZOPTS = 'X'
 
      CALL RZFILE(LUNXZI,'RZ',RZOPTS)
 
      IRC = IQUEST(1)
      IF(IRC.NE.0) THEN
         IF(IDEBXZ.GE.0) PRINT *,'XZRTOF. error from RZFILE = ',IRC
         CLOSE(LUNXZI)
         GOTO 99
      ENDIF
 
      IF(IOPTS.NE.0) CALL RZSTAT('//RZ',99,' ')
*
*     Open the output file
*
      JRECL = IRECL
#if !defined(CERNLIB_UNIX)
      CHOPO = 'ON'
      CHOPF = 'XO'
#endif
#if defined(CERNLIB_UNIX)
      CHOPO = 'DON'
      CHOPF = 'DO'
#endif
      IF(IOPTA.NE.0) THEN
         JRECL = 80
         CHOPO = 'FON'
         CHOPF = 'AO'
      ENDIF
 
      LCHOPO   = LENOCC(CHOPO)
      IF(IOPTR.NE.0) CHOPO(LCHOPO:LCHOPO) = ' '
      LCHOPO   = LENOCC(CHOPO) + 1
      IF(IOPTC.NE.0) CHOPO(LCHOPO:LCHOPO) = 'C'
 
      IF(IDEBXZ.GE.3) PRINT *,'XZRTOF. call SZOPEN for ',
     +   LUNXZO,CHFZ(1:LCHFZ),JRECL,CHOPO
      CALL SZOPEN(LUNXZO,CHFZ(1:LCHFZ),JRECL,CHOPO,IRC)
 
      IF(IRC.EQ.28) THEN
         IF(IDEBXZ.GE.0) PRINT *,'XZRTOF. file ',CHFZ(1:LCHFZ),
     +      ' already exists - specify R option to replace'
         CLOSE(LUNXZI)
         GOTO 98
      ENDIF
 
      IF(IRC.NE.0) THEN
         IF(IDEBXZ.GE.0) PRINT *,'XZRTOF. error from SZOPEN = ',IRC
         CLOSE(LUNXZI)
         GOTO 98
      ENDIF
*
*     Issue FZFILE
*
      CALL FZFILE(LUNXZO,JRECL/4,CHOPF)
      IRC   = IQUEST(1)
      IF(IRC.NE.0) THEN
         IF(IDEBXZ.GE.0) PRINT *,'XZRTOF. error from FZFILE = ',IRC
         GOTO 98
      ENDIF
 
      CALL FZLOGL(LUNXZO,IDEBXZ)
*
*     Convert the file
*
      CALL RZTOFZ(LUNXZO,'C')
*
*     Terminate and close
*
      CALL FZENDO(LUNXZO,'TE')
98    CONTINUE
      CLOSE(LUNXZO)
      CALL RZCLOS('RZ',' ')
 
99    CONTINUE
 
      END
