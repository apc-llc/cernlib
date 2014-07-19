*
* $Id: dzdcti.F,v 1.1.1.1 1996/03/04 16:13:08 mclareni Exp $
*
* $Log: dzdcti.F,v $
* Revision 1.1.1.1  1996/03/04 16:13:08  mclareni
* Dzdoc/Zebpack
*
*
#include "dzdoc/pilot.h"
      SUBROUTINE DZDCTI(CHIN,IVAL)
      CHARACTER*(*) CHIN
      CHARACTER*4 CHFORM
      CHARACTER*1 CHF1
      IVAL = 0
      IF(CHIN .NE.  ' ')THEN
            IFC = INDEXC(CHIN,' ')
            IF(IFC.LE.0)IFC=1
            IF(CHIN(IFC:IFC).EQ.'$')THEN
               IFC=IFC+1
               CHF1='Z'
            ELSE
               CHF1='I'
            ENDIF
            ILC = INDXBC(CHIN,' ')
            IF(ILC.LE.0)ILC=12
            NC=ILC-IFC+1
            IF(NC.GT.9)THEN
               WRITE(CHFORM,'(A,I2,A)') '('//CHF1,NC,')'
            ELSE
               WRITE(CHFORM,'(A,I1,A)') '('//CHF1,NC,')'
            ENDIF
            READ(CHIN(IFC:ILC),CHFORM,ERR=999)IVAL
      ENDIF
  999 END
