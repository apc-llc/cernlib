*
* $Id: hfunc.F,v 1.1.1.1 1996/01/16 17:07:38 mclareni Exp $
*
* $Log: hfunc.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:38  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.19/00 26/04/93  11.57.51  by  Rene Brun
*-- Author :
      SUBROUTINE HFUNC (ID2,FUNC)
*.==========>
*.           Store value of function FUNC in LFUNC data structure
*..=========> ( R.Brun )
#include "hbook/hcbook.inc"
#include "hbook/hcflag.inc"
#include "hbook/hcbits.inc"
#include "hbook/hcform.inc"
      REAL V(2)
      EQUIVALENCE (X, V(1)), (Y, V(2))
      EXTERNAL FUNC
*.___________________________________________
      IRET=3
   10 CALL HLOOP(ID2,'HFUNC ',IRET)
      IF(IRET.EQ.0)GO TO 60
      CALL HDCOFL
      IF(I123.EQ.0)THEN
         CALL HBUG('Not a histogram','HFUNC',ID)
         IRET=2
         GO TO 10
      ENDIF
*
      LCONT=LQ(LCID-1)
      IF(IQ(LCONT-2).EQ.0)THEN
         NNOW=IQ(LCONT-1)+IQ(LCONT-3)+10
         NMORE=2
         NNEW=NNOW+NMORE
         CALL HSPACE(NNOW+NNEW,'HFUNC ',ID)
         IF(IERR.NE.0)GO TO 50
         CALL MZPUSH(IHDIV,LCONT,NMORE,0,' ')
         IQ(LCID+KNTOT)=IQ(LCID+KNTOT)+NMORE
      ENDIF
      IF(LQ(LCONT-1).EQ.0)THEN
         IF(I1.NE.0)THEN
            NCX=IQ(LCID+KNCX)
            NTOT=NCX+12
            CALL HSPACE(NTOT,'HFUNC ',ID2)
            IF(IERR.NE.0)GO TO 50
            CALL HSIFLA(12,1)
            IQ(LCID+KNTOT)=IQ(LCID+KNTOT)+NTOT
            CALL MZBOOK(IHDIV,LFUNC,LCONT,-1,'HFUN',1,1,NCX+2,
     +      IOCF2,0)
         ELSE
            NCX=IQ(LCID+KNCX)
            NCY=IQ(LCID+KNCY)
            IF(LQ(LCONT-1).EQ.0)THEN
               NTOT=NCX*NCY+14
               CALL HSPACE(NTOT,'HFUNC ',ID2)
               IF(IERR.NE.0)GO TO 50
               CALL HSIFLA(12,1)
               IQ(LCID+KNTOT)=IQ(LCID+KNTOT)+NTOT
               CALL MZBOOK(IHDIV,LFUNC,LCONT,-1,'HFUN',1,1,NCX*NCY+4,
     +         IOCF4,0)
            ENDIF
         ENDIF
      ELSE
         LFUNC=LQ(LCONT-1)
         IF(I1.NE.0)THEN
            NCX=IQ(LCID+KNCX)
            NMORE=NCX+2-IQ(LFUNC-1)
            IF(NMORE.NE.0)THEN
               LR1=LFUNC
               CALL MZPUSH(IHDIV,LR1,0,NMORE,' ')
               IQ(LCID+KNTOT)=IQ(LCID+KNTOT)+NMORE
            ENDIF
         ENDIF
      ENDIF
      LFUNC =LQ(LCONT-1)
      LHFIT =LQ(LFUNC-1)
      IF(IQ(LFUNC-2).EQ.0)LHFIT=0
      IF(LHFIT.NE.0)IQ(LHFIT+1)=0
      IQ(LFUNC+1)=1
      IQ(LFUNC+2)=NCX
      IF(I1.EQ.0)THEN
         IQ(LFUNC+3)=1
         IQ(LFUNC+4)=NCY
      ENDIF
      IF(NV.NE.1)THEN
         IF(I1.NE.0)THEN
            DX=(Q(LCID+KXMAX)-Q(LCID+KXMIN))/FLOAT(IQ(LCID+KNCX))
            DO 20 I=1,NCX
               IF(I6.EQ.0)THEN
                  X=Q(LCID+KXMIN) +DX*(FLOAT(I)-0.5)
               ELSE
                  LBINS=LQ(LCID-2)
                  X=0.5*(Q(LBINS+I)+Q(LBINS+I+1))
               ENDIF
               Q(LFUNC+I+2)=FUNC(V)
   20       CONTINUE
         ELSE
* MZPUSH HFUN bank if necessary.
            IF(IQ(LFUNC-1).EQ.4)THEN
               CALL MZPUSH(IHDIV,LFUNC,0,NCX*NCY,' ')
               IQ(LCID+KNTOT)=IQ(LCID+KNTOT)+NCX*NCY
            ENDIF
            DX=(Q(LCID+KXMAX)-Q(LCID+KXMIN))/FLOAT(IQ(LCID+KNCX))
            DY=(Q(LCID+KYMAX)-Q(LCID+KYMIN))/FLOAT(IQ(LCID+KNCY))
            DO 40 IX=1,NCX
               X=Q(LCID+KXMIN)+(IX-0.5)*DX
               DO 30 IY=1,NCY
                  Y=Q(LCID+KYMIN)+(IY-0.5)*DY
                  I=(IY-1)*NCX+IX
                  Q(LFUNC+I+4)=FUNC(V)
   30          CONTINUE
   40       CONTINUE
         ENDIF
      ENDIF
*
   50 IRET=2
      GO TO 10
   60 NV=2
      END
