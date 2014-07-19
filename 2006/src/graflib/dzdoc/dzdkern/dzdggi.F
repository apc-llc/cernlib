*
* $Id: dzdggi.F,v 1.3 2003/02/03 14:54:49 mclareni Exp $
*
* $Log: dzdggi.F,v $
* Revision 1.3  2003/02/03 14:54:49  mclareni
* Recent f90 compilers prefer * for assumed size arrays
*
* Revision 1.2  1997/03/14 14:20:28  mclareni
* WNT mods
*
* Revision 1.1.1.1.2.1  1997/01/21 11:24:55  mclareni
* All mods for Winnt 96a on winnt branch
*
* Revision 1.1.1.1  1996/03/04 16:12:57  mclareni
* Dzdoc/Zebpack
*
*
#include "dzdoc/pilot.h"
      SUBROUTINE DZDGGI(INFVEC,CTAGRQ,CLINK,COUT,IFC,ILC)
      INTEGER INFVEC(*)
      CHARACTER*2 CTAGRQ, CTAGIN
      CHARACTER*4 CLINK,CLINKI
      CHARACTER*8 CINT
      CHARACTER*(*) COUT
      INTEGER LENINF
#include "dzdoc/dzdocobk.inc"
*--     Text and order for general information
*--     Additional offset for *REP card on output
#include "dzdoc/docparq.inc"
#include "dzdoc/bknuparq.inc"
#include "zebra/bkfoparq.inc"
#include "dzdoc/bkstparq.inc"
#include "dzdoc/bktgparq.inc"
#include "dzdoc/bktgdatq.inc"
 
      IFC=0
      ILC=0
      COUT = ' '
      LCOUT=LEN(COUT)
      IF(CTAGRQ.NE.'LI' .AND. CTAGRQ.NE.'DA')THEN
         II=INFVEC(MBHEAQ)+1
         LENINF=INFVEC(MBGENQ)+II
      ELSE IF(CTAGRQ.EQ.'LI')THEN
         II=INFVEC(MBGENQ)+INFVEC(MBHEAQ)+1
         LENINF=INFVEC(MBLINQ)+II
      ENDIF
      IF(LENINF.LE.1)GOTO 999
*     loop through general info and find tag
 
1     CONTINUE
 
      IF(II.GE.LENINF)GOTO 999
      NW = INFVEC(II)/16
      IT = MOD(INFVEC(II),16)
*     WRITE(*,*)'New section,IT,NW',IT,NW
*     if not Integer section continue
      IF(IT.NE.2)THEN
          IF(IT.EQ.5)THEN
*              WRITE(*,*)' Hollerith section:'
*              WRITE(*,'(80A4)')(INFVEC(II+K),K=1,NW)
          ENDIF
          II = II + NW + 1
          GOTO 1
      ENDIF
*      WRITE(*,'(A,Z9)')' Tag word(hex)',INFVEC(II+1)
*     decode word containing tag ..
      LOWBIT=IAND(INFVEC(II+1),1023)
*      WRITE(*,*)' bits 0-9 ',LOWBIT
      INTAG=IBITS(INFVEC(II+1),ICHIDQ,NCHIDQ)
*     the comment on *B..BANK comment is special(ID=0)
      IF(INTAG.EQ.0)THEN
          CTAGIN='CL'
      ELSE
          CTAGIN=CBKTAC(INTAG)
      ENDIF
*     find position of tag
3     CONTINUE
*      WRITE(*,'(A,I5,X,A)')'INTAG ',INTAG,CBKTAC(INTAG)
*     WRITE(*,*)' REST OF INTEGER WORDS',(INFVEC(II+K),K=2,NW)
      IF(CTAGIN.EQ.CTAGRQ .OR. CTAGRQ.EQ.'LI')THEN
          IF(LOWBIT.EQ.0)THEN
*             info given as Integer in following word
              IF(INFVEC(II+2).LT.0)THEN
                  CINT='  NNNNNN'
                  IFC = 3
              ELSE
                  WRITE(CINT,'(I8)')INFVEC(II+2)
                  IFC=INDEXN(CINT)
              ENDIF
              NC1 = MIN(LCOUT,LEN(CINT)-IFC+1)
              COUT(1:NC1)=CINT(IFC:IFC+NC1-1)
          ENDIF
*         is there any holl text left
          II = II+NW+1
          NW = INFVEC(II)/16
          IF(NW.LE.0)GOTO 4
          IT = MOD(INFVEC(II),16)
          IF(IT.EQ.5)THEN
*            if its link look if it matches 
             IF(CTAGRQ.EQ.'LI')THEN
                CALL UHTOC(INFVEC(II+1),4,CLINKI,4)
                IF(CLINKI.NE.CLINK)THEN
                   GOTO 4
                ELSE
                   II=II+2
                   NW=NW-2
                ENDIF
             ENDIF
             NC = MIN(LCOUT,NW*4)
             CALL UHTOC(INFVEC(II+1),4,COUT,NC)
          ENDIF
          IFC=INDEXC(COUT,' ')
          ILC=INDXBC(COUT,' ')
          GOTO 999
      ENDIF
4     II= II+NW+1
      GOTO 1
999   CONTINUE
      END
************************************************************************
