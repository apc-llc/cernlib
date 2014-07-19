*
* $Id: sigsee.F,v 1.1.1.1 1995/12/12 14:36:17 mclareni Exp $
*
* $Log: sigsee.F,v $
* Revision 1.1.1.1  1995/12/12 14:36:17  mclareni
* Imported sources
*
*
#include "sigma/pilot.h"
*CMZ :  1.06/00 14/08/89  15.15.54  by  Carlo E. Vandoni
*-- Author :
      SUBROUTINE SIGSEE
C
C    ....................................................
C         PURPOSE
C            RETURNS  KIT=POINTER IF NAME IS NOT IN IETA(FIRST FREE LOC.)
C                     KIT=-POINTER IF NAME IS IN IETA (LOCATION OF NAME)
C
C         USAGE
C            CALL SEE
C
C         REMARKS
C
C
C...  PAW VERSION  ...  MAY 1988
C
C    ......................................................
#include "sigma/sicsig.inc"
#include "sigma/sigc.inc"
#include "sigma/pawc.inc"
C
      CALL SITRAC (' SIGSEE     ')
C
****************************************
      CALL KUVECT(CNAME,LLOW,LHI)
C
      KIT=-(LLOW-4)
      IF(LLOW.EQ.0)KIT=123
      MODE=IQUEST(14)
      IF(MODE.EQ.3)MODE=2
      IF(KIT.GE.0)MODE=1
         IF(IQUEST(14).EQ.2)THEN
           MODE=1
*
*   THIS (MODE=1) HAS TO BE CHANGED LATER WHEN
* DECIDED HOW TO HANDLE THE INTEGER VECTORS
*
           IF(KIT.LT.0)THEN
              PRINT *,' TRYING TO USE WITHIN SIGMA AN INTEGER VECTOR'
              PRINT *,' ALREADY DEFINED'
           ENDIF
         ENDIF
*      PRINT *,CNAME,'IQUEST (14) in sigsee =',IQUEST(14)
*      print *,'kit in sigsee = ',kit
*      PRINT *,'MODE IN SIGSEE',MODE
****************************************
 999  END
