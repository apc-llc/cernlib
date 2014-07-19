*
* $Id: hreset.F,v 1.1.1.1 1996/01/16 17:07:47 mclareni Exp $
*
* $Log: hreset.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:47  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.22/08 04/07/94  08.53.18  by  Rene Brun
*-- Author :
      SUBROUTINE HRESET(IDD,CHTITL)
*.==========>
*.           RESETS TO 0. CONTENT OF IDD
*.           OPTIONNALY CHANGE TITLE
*..=========> ( R.Brun )
#include "hbook/hcbits.inc"
#include "hbook/hcbook.inc"
      CHARACTER*(*) CHTITL
      CHARACTER*80   CHRZ
*.___________________________________________
      NARG=2
      CALL NOARG(NARG)
      IRET=3
*
  10  CALL HLOOP(IDD,'HRESET',IRET)
      IF(IRET.EQ.0)GO TO 99
      CALL HDCOFL
      CALL SBIT0(IQ(LCID),5)
      IF(I4.NE.0)THEN
         IF(IDD.NE.0)THEN
            NDIM=IQ(LCID+2)
            NCHRZ=IQ(LCID+11)
            IF (IQ(LCID-2) .NE. 2) THEN
               CALL HNTRSET(IDD, CHTITL)
               GO TO 90
            ENDIF
*
            LLIMS=LQ(LCID-2)
            DO 15 I=1,NDIM
               Q(LLIMS+2*I-1)=1.E31
               Q(LLIMS+2*I  )=-1.E31
  15        CONTINUE
            CALL SBIT1(IQ(LCONT),1)
            IQ(LCID+3)=0
            IQ(LCID+5)=IDD
            IQ(LCID+7)=1
            IQ(LCID+11)=0
            CALL HCDIR(CHRZ, 'R')
            IF (CHRZ(1:6) .EQ. '//PAWC') THEN
               IQ(LCID+11) = 0
               CHRZ = ' '
            ELSE
               IQ(LCID+11) = LENOCC(CHRZ)
            ENDIF
            CALL UCTOH(CHRZ,IQ(LCID+12),4,80)
*
            CALL HBTIT(CHTITL,NWTITL,NCHT)
            IF (NWTITL .NE. 0) THEN
               NMORE = NWTITL - IQ(LCID+8)
               IF(NMORE.NE.0)THEN
                  CALL MZPUSH(IHDIV,LCID,0,NMORE,' ')
               ENDIF
               ITIT1 = IQ(LCID+9)
               CALL UCTOH(CHTITL,IQ(LCID+ITIT1),4,NCHT)
            ENDIF
            IQ(LCID+8) = NWTITL
*      drop linear structure for memory resident ntuples
            IF(NCHRZ.EQ.0)THEN
               LR2=LQ(LCID-1)
               IF(LQ(LR2).NE.0)THEN
                  CALL MZDROP(IHDIV,LQ(LR2),' ')
               ENDIF
            ENDIF
         ENDIF
         GO TO 90
      ENDIF
*
*             CHANGE TITLE
*
      IF(NARG.EQ.2.AND.I123.NE.0)THEN
         CALL HBTIT(CHTITL,NWTITL,NCHT)
         IF(NWTITL.NE.0)THEN
            IF(I1.NE.0)THEN
               NWOLD=IQ(LCID-1)-KTIT1+1
               J1=KTIT1
            ELSEIF(I230.NE.0)THEN
               NWOLD=IQ(LCID-1)-KTIT2+1
               J1=KTIT2
            ENDIF
            NMORE=NWTITL-NWOLD
            IF(NMORE.NE.0)THEN
               CALL MZPUSH(IHDIV,LCID,0,NMORE,' ')
               IQ(LCID+KNTOT)=IQ(LCID+KNTOT)+NMORE
            ENDIF
            IF(NWTITL.NE.0)THEN
               CALL UCTOH(CHTITL,IQ(LCID+J1),4,NCHT)
            ENDIF
         ENDIF
      ENDIF
*
*             Reset contents
*
*======>     Array
*
      IF(I123.EQ.0)THEN
         CALL VZERO(IQ(LCID+1),IQ(LCID-1))
         GO TO 90
      ENDIF
*
*======>     1-DIM histogram
*
      IF(I1.NE.0)THEN
         LCONT=LQ(LCID-1)
         CALL HSUPIS(SQRT,0,0,0)
         CALL HREZ0(LQ(LCID-1))
         GO TO 90
      ENDIF
*
*=====>     2-DIM histogram
*
      IF(I230.NE.0)THEN
         LSCAT=LQ(LCID-1)
         CALL VZERO(IQ(LSCAT+KNOENT),IQ(LSCAT-1)-1)
*
*           PROX
*
         CALL HREZ0(LQ(LCID-2))
*
*           PROY
*
         CALL HREZ0(LQ(LCID-3))
*
*           SLIX
*
         LSLIX=LQ(LCID-4)
         IF(LSLIX.NE.0)THEN
            DO 40 I=1,IQ(LSLIX-2)
               CALL HREZ0(LQ(LSLIX-I))
  40        CONTINUE
         ENDIF
*
*           SLIY
*
         LSLIY=LQ(LCID-5)
         IF(LSLIY.NE.0)THEN
            DO 50 I=1,IQ(LSLIY-2)
               CALL HREZ0(LQ(LSLIY-I))
  50        CONTINUE
         ENDIF
*
*           BANX
*
         LBANX=LQ(LCID-6)
  60     IF(LBANX.NE.0)THEN
            CALL HREZ0(LQ(LBANX-1))
            LBANX=LQ(LBANX)
            IF(LBANX.NE.0)GO TO 60
         ENDIF
*
*           BANY
*
         LBANY=LQ(LCID-7)
  70     IF(LBANY.NE.0)THEN
            CALL HREZ0(LQ(LBANY-1))
            LBANY=LQ(LBANY)
            IF(LBANY.NE.0)GO TO 70
         ENDIF
      ENDIF
*
  90  IRET=2
      GO TO 10
*
  99  RETURN
      END
