*
* $Id: dzdpln.F,v 1.1.1.1 1996/03/04 16:13:18 mclareni Exp $
*
* $Log: dzdpln.F,v $
* Revision 1.1.1.1  1996/03/04 16:13:18  mclareni
* Dzdoc/Zebpack
*
*
#include "dzdoc/pilot.h"
      SUBROUTINE DZDPLN(LUN,CVAR,NEWFLG)
*     put CVAR into output line
*     NEWFLG:  1: start new or continue FORTRAN
*              2: force new line (FORTRAN)
*             99: output last line (FORTRAN)
*              0: start new or continue (col 1, no blank suppress)
*             -1: dito but suppress all but 1 blank
*             -2: force new line (keep blanks)
*             -3: dito suppress blanks
*            -99: last line
*
      CHARACTER*(*) CVAR
      PARAMETER (NCL=80)
      CHARACTER*(NCL) CL
      SAVE CL
      SAVE IP
      DATA IP/1/
*
      IF(NEWFLG.GT.0)THEN
         ICOL1=7
         NCLINE=72
      ELSE
         ICOL1=1
         NCLINE=80
      ENDIF
      IF(LUN.LE.0)THEN
         IP=1
         GOTO 999
      ENDIF
*     force new line
      IF(ABS(NEWFLG).GT.1)THEN
          IF(IP.GT.ICOL1 .OR.
     &    (IP.EQ.ICOL1.AND.CL(ICOL1:ICOL1).NE.' '))THEN
             IP=IP-1
             IF(CL(IP:IP).EQ.' ')IP=IP-1
             IF(IP.GT.NCL)IP=NCL
             WRITE(LUN,'(A)')CL(1:IP)
          ENDIF
          CL=' '
          IP=ICOL1
          IF(ABS(NEWFLG).EQ.99)GOTO 999
      ENDIF
      NC=LEN(CVAR)
      IF(NC.LE.0)GOTO 999
*     remove trailing blanks
      ILC = INDXBC(CVAR(1:NC),' ')
      IF(ILC.GT.0)NC=ILC
      IF(NEWFLG.EQ.-1 .OR. NEWFLG.EQ.-3)THEN
         ILC=NC
         IFC=INDEXC(CVAR(1:ILC),' ')
         IF(IFC.EQ.0)GOTO 999
*        find # of char, keep 1 blank
         IBL=0
         NC=0
         DO 10 I=IFC,ILC
         IF(CVAR(I:I).NE.' ')THEN
            NC=NC+1
            IBL=0
         ELSE
            IF(IBL.EQ.0)NC=NC+1
            IBL=IBL+1
         ENDIF
10       CONTINUE
      ENDIF
*     insert one blank if not FORTRAN
      IF(NEWFLG.LE.0)THEN
         IF(IP.GT.1 .AND. IP.LE.NCLINE)THEN
            CL(IP:IP)=' '
            IP=IP+1
         ENDIF
      ENDIF
*     does it fit on line
 
      IF(IP+NC-1.GT.NCLINE)THEN
          IP=IP-1
          IF(CL(IP:IP).EQ.' ')IP=IP-1
          IF(IP.GT.NCL)IP=NCL
          WRITE(LUN,'(A)')CL(1:IP)
          CL=' '
          IF(NEWFLG.GT.0)CL(6:6)='+'
          IP=ICOL1
      ENDIF
      IF(NEWFLG.EQ.-1 .OR. NEWFLG.EQ.-3)THEN
         DO 20 I=IFC,ILC
         IF(CVAR(I:I).NE.' ')THEN
            CL(IP:IP)=CVAR(I:I)
            IP=IP+1
            IBL=0
         ELSE
            IF(IBL.EQ.0)THEN
               CL(IP:IP)=' '
               IP=IP+1
            ENDIF
            IBL=IBL+1
         ENDIF
20       CONTINUE
*         add a blank
*         IF(IP.LE.NCLINE)THEN
*            CL(IP:IP)=' '
*            IP=IP+1
*         ENDIF
      ELSE
         IPL = IP+NC-1
         CL(IP:IPL)=CVAR(1:NC)
         IP = IPL+1
      ENDIF
999   END
 
************************************************************************
