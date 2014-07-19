*
* $Id: paobs.F,v 1.4 1998/12/09 10:54:51 couet Exp $
*
* $Log: paobs.F,v $
* Revision 1.4  1998/12/09 10:54:51  couet
* - Commands in OBSOLETE/GRAPHICS/ATTRIBUTES/ print now an error
*   message instead of a warning and the command is NOT executed.
*
* Revision 1.3  1996/09/10 13:37:27  couet
* - NT/MERGE is obsolete (use HMERGE instead)
*
* Revision 1.2  1996/03/14 11:15:33  couet
* Is now the action routine of MASK
*
* Revision 1.1.1.1  1996/03/01 11:38:42  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.07/14 20/09/95  16.49.00  by  O.Couet
*-- Author :    O.Couet   31/01/95
      SUBROUTINE PAOBS
#include "paw/pcpatl.inc"
      CHARACTER*8 CHNEW
      CALL KUPATL(CHPATL,NPAR)
      IF(CHPATL.EQ.'SLN')THEN
         CHNEW = 'SET LTYP'
         GO TO 99
      ENDIF
      IF(CHPATL.EQ.'SFAIS')THEN
         CHNEW = 'SET FAIS'
         GO TO 99
      ENDIF
      IF(CHPATL.EQ.'SFASI')THEN
         CHNEW = 'SET FASI'
         GO TO 99
      ENDIF
      IF(CHPATL.EQ.'SFACI')THEN
         CHNEW = 'SET FACI'
         GO TO 99
      ENDIF
      IF(CHPATL.EQ.'SPLCI')THEN
         CHNEW = 'SET PLCI'
         GO TO 99
      ENDIF
      IF(CHPATL.EQ.'SPMCI')THEN
         CHNEW = 'SET PMCI'
         GO TO 99
      ENDIF
      IF(CHPATL.EQ.'STXCI')THEN
         CHNEW = 'SET TXCI'
         GO TO 99
      ENDIF
      IF(CHPATL.EQ.'STXFP')THEN
         CHNEW = 'SET TXFP'
         GO TO 99
      ENDIF
      IF(CHPATL.EQ.'SCHH')THEN
         CHNEW = 'SET CHHE'
         GO TO 99
      ENDIF
      IF(CHPATL.EQ.'SLWSC')THEN
         CHNEW = 'SET LWID'
         GO TO 99
      ENDIF
      IF(CHPATL.EQ.'SMK')THEN
         CHNEW = 'SET MTYP'
         GO TO 99
      ENDIF
*
      IF(CHPATL.EQ.'MASK')THEN
         CALL KUEXEC('HELP OBSOLETE/MASK')
         RETURN
      ENDIF
*
      IF(CHPATL.EQ.'MERGE')THEN
         CALL KUEXEC('HELP OBSOLETE/NTUPLE/MERGE')
         RETURN
      ENDIF
*
   99 CALL KUALFA
      PRINT*,' '
      PRINT*,'*** ERROR: ',CHPATL(1:LENOCC(CHPATL)),
     +       ' is obsolete, use: ',CHNEW
      PRINT*,' '
      END
