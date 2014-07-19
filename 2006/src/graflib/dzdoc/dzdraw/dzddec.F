*
* $Id: dzddec.F,v 1.1.1.1 1996/03/04 16:13:08 mclareni Exp $
*
* $Log: dzddec.F,v $
* Revision 1.1.1.1  1996/03/04 16:13:08  mclareni
* Dzdoc/Zebpack
*
*
#include "dzdoc/pilot.h"
      SUBROUTINE DZDDEC(CPRMPT,IVAL)
      CHARACTER*(*) CPRMPT
      CHARACTER*12 CTEMP
      CHARACTER*4 CHFORM
   10 CONTINUE
      CTEMP = ' '
      CALL KUPROC(CPRMPT,CTEMP,NCH)
      IF(NCH.LE.0)GOTO 999
      IF(CTEMP .NE.  ' ')THEN
            ILC = INDXBC(CTEMP,' ')
            IF(ILC.GT.9)ILC=9
            IFC=INDEX(CTEMP,'$')
            IF(IFC.LE.0)THEN
                IFC=1
                WRITE(CHFORM,'(A,I1,A)') '(I',ILC,')'
            ELSE
                IFC=IFC+1
                WRITE(CHFORM,'(A,I1,A)') '(Z',ILC,')'
            ENDIF
            READ(CTEMP(IFC:ILC),CHFORM,ERR=20 )IVAL
      ENDIF
      GOTO 999
   20 CONTINUE
      WRITE(*,*)' Input conversion error, please retype'
      GOTO 10
  999 END
