*
* $Id: cstrer.F,v 1.2 1996/03/14 13:42:59 berezhno Exp $
*
* $Log: cstrer.F,v $
* Revision 1.2  1996/03/14 13:42:59  berezhno
* mods for output logical vars for NT
*
* Revision 1.1.1.1  1996/02/26 17:16:17  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.18/14 03/11/94  16.55.44  by  Vladimir Berezhnoi
*-- Author : V.Berezhnoi
      SUBROUTINE CSTRER(IERR,I)
***----------------------------------
*       types error message and invokes editor
***----------------------------------

    
      INTEGER CSIMED
      CHARACTER PRONAME*32
#include "comis/cslun.inc"
#include "comis/cspar.inc"
#include "comis/csrec.inc"
#include "comis/cskeys.inc"
#include "comis/cspnts.inc"
         COMMON/CSGSCM/IGS,J,N,CSJUNK(3)
#if !defined(CERNLIB_PAW)
         LOGICAL INTRAC
#endif
      CALL UHTOC(IDEN,4,PRONAME,NCIDEN)
      NOLINE=NSTRG
      N1=J-JMC
      CALL CCOPYS(JMC,JSR1,N1)
      REC1(N1+1:N1+3)=' ^ '
      IF(REC1(:N1+1).EQ.' ')THEN
        NOLINE=NSTRG-1
        IND=JPMB
        DO K=1,NOLINE
          CALL CSGETL(IND,REC1,NMC,M)
        ENDDO
        REC1(NMC+1:NMC+3)=' ^ '
      ELSE
        CALL CCOPYS(J,JSR1+N1+3,N)
      ENDIF
      WRITE(*,77)PRONAME(:NCIDEN),NOLINE
 77   FORMAT(' CS-TR-ERR: routine ',A,', line ',I4)
      CALL CSSOUT(REC1(:NMC+3))
      CALL CSTERR(IERR)
#if !defined(CERNLIB_PAW)
      IF(MDEDIT.EQ.1 .OR. LUNINP.NE.5 )THEN
#endif
#if defined(CERNLIB_PAW)
*-- INTRAC returns .FALSE. for PAW++ because input comes from pipe
      IF(MDEDIT.EQ.1 .OR. LUNINP.NE.5)THEN
#endif
        I=0
      ELSE
        CALL CSSINP(REC,'MED>',NSR)
        I=CSIMED(JSR,NSR)
      ENDIF
      END
