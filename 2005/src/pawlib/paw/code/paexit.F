*
* $Id: paexit.F,v 1.1.1.1 1996/03/01 11:38:39 mclareni Exp $
*
* $Log: paexit.F,v $
* Revision 1.1.1.1  1996/03/01 11:38:39  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.06/18 28/02/95  13.26.01  by  O.Couet
*-- Author :    Rene Brun   03/01/89
      SUBROUTINE PAEXIT
*
*                Called by KUIP/EXIT to leave PAW in a clean way
*
#include "hbook/hcdire.inc"
#include "hbook/hcpiaf.inc"
#include "paw/pawlun.inc"
#include "hbook/czsock.inc"
*
      CHARACTER*32 CHF
      SAVE IFIRST
      DATA IFIRST/0/
*________________________________________________________
*
*
      IF(IFIRST.EQ.0)THEN
#if defined(CERNLIB_CZ)
*
*           Close remote communications
*
         DO 1 I=1,NCHTOP
            IF (ICHTOP(I).GT.1000 .AND. ICHLUN(I).EQ.0 .AND.
     +                                           .NOT.SERVPF) THEN
               ISKIN=MOD(ICHTOP(I),10000)
               ISKOUT=ICHTOP(I)/10000
*--- otherwise it is a file connection to the Piaf server
               CALL CZCLOS(ISTAT)
               IF(ISKIN.EQ.CLINPF) THEN
                  CONNPF=.FALSE.
               ENDIF
            ENDIF
   1     CONTINUE
#endif
*
*           Remove COMIS shared library files from /tmp
*
         IF (.NOT.SLAVPF) CALL CSSHLD
*
*           Close other files
*
         DO 10 LUN=1,99
            IFREE=LUNIT(LUN)
            IF(IFREE.LE.0.OR.IFREE.EQ.10)GO TO 10
            IF(IFREE.EQ.6.OR.IFREE.EQ.7.OR.IFREE.EQ.8)GO TO 10
            IF(LUNIT(LUN).EQ.2)THEN
               CALL FZENDO(LUN,'TX')
            ENDIF
            IF(LUNIT(LUN).GT.2.AND.LUNIT(LUN).LT.6)THEN
               DO 5 I=1,NCHTOP
                  IF(ICHTOP(I).EQ.LUN)THEN
                     CHF=CHTOP(I)
                     CALL RZCDIR('//'//CHF,' ')
                     CALL HRENDC(CHF)
                     GOTO 7
                  ENDIF
   5           CONTINUE
               GOTO 7
            ENDIF
            CALL PACLOS(LUN)
   7        LUNIT(LUN)=0
  10     CONTINUE
*
         IF (.NOT.SERVPF) CALL IGEND
*
         PRINT *,'Exiting from PAW.'
         IFIRST=1
         CALL PALOGS(2)
         CALL PAWUSR(2)
      ENDIF
*
      END
