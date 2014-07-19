*
* $Id: hfn.F,v 1.2 1996/02/29 11:20:10 cernlib Exp $
*
* $Log: hfn.F,v $
* Revision 1.2  1996/02/29 11:20:10  cernlib
* Replace a tab by spaces
*
* Revision 1.1.1.1  1996/01/16 17:07:37  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :          04/08/95  12.08.44  by  Julian Bunn
*-- Author :
      SUBROUTINE HFN(ID1,X)
*.==========>
*.           FILLING OF A N-TUPLE
*.
*..=========> ( R.Brun )
#include "hbook/hcflag.inc"
#include "hbook/hcbook.inc"
#include "hbook/hcbits.inc"
#include "hbook/hcdire.inc"
      COMMON/QUEST/IQUEST(100)
      CHARACTER*64 CHWOLD,CHDIR,CWDRZ
      DIMENSION X(*)
      INTEGER   IDRZ(2)
      CHARACTER*80 MSG
*.___________________________________________
*
 
 
      IF(ID1.NE.IDLAST)THEN
         ID=ID1
         IDPOS=LOCATI(IQ(LTAB+1),IQ(LCDIR+KNRH),ID)
         IF(IDPOS.LE.0)RETURN
         IDLAST=ID1
         LCID=LQ(LTAB-IDPOS)
         I4=JBIT(IQ(LCID+KBITS),4)
         IF(I4.EQ.0)RETURN
         IF (IQ(LCID-2) .NE. 2) THEN
            CALL HBUG('New N-tuple, fill it with HFNT',
     +                'HFN',ID)
            RETURN
         ENDIF
      ENDIF
*
*             Memory bank full ?
*             Allocate a new block if memory only
*             Write current block with RZ and start filling same block
*             if RZ option
*
      LCONT=LQ(LCID-1)
      LR2=LQ(LCID-3)
      NDIM=IQ(LCID+2)
      IFIRST=IQ(LCID+7)
   10 IF(IFIRST+NDIM-1.GT.IQ(LCONT-1))THEN
         NCHRZ=IQ(LCID+11)
         NWP=IQ(LCONT-1)
         IF(NCHRZ.EQ.0)THEN
            CALL HSPACE(NWP+10,'HFN   ',ID1)
            IF(IERR.NE.0)GO TO 30
            CALL MZBOOK(IHDIV,LR2,LR2,0,'HCON',0,0,NWP,3,0)
            LQ(LCID-3)=LR2
            IQ(LCID+5)=IQ(LCID+5)+1
         ELSE
            CALL SBIT0(IQ(LCONT),1)
            IQ(LCID+6)=IQ(LCID+6)+1
            CALL RZCDIR(CWDRZ,'R')
            CALL HCDIR(CHWOLD,'R')
            CHDIR=' '
            CALL UHTOC(IQ(LCID+12),4,CHDIR,NCHRZ)
            CALL HCDIR(CHDIR,' ')
            IF(IQUEST(1).NE.0)THEN
               IQ(LCID+11)=0
               CALL HBUG('+Switch to memory mode','HFN',ID1)
               GO TO 10
            ENDIF
            IF (ICHTYP(ICDIR) .EQ. 1) THEN
               IDRZ(1) = 10000*IQ(LCID+6)+ID1
            ELSE
               IDRZ(1) = ID1
               IDRZ(2) = IQ(LCID+6)
            ENDIF
            CALL HRZOUT(IHDIV,LCONT,IDRZ,ICYCLE,'A')
            IF(IQ(LCID+6).EQ.1)THEN
               IDRZ(2)=0
               CALL HRZOUT(IHDIV,LCID,IDRZ,ICYCLE,'S')
            ENDIF
            CALL RZSAVE
            CALL HCDIR(CHWOLD,' ')
            IF(CHWOLD.NE.CWDRZ)THEN
               CALL RZCDIR(CWDRZ,' ')
            ENDIF
            CALL SBIT1(IQ(LCONT),1)
         ENDIF
         IFIRST=1
      ENDIF
*
      LLIMS=LQ(LCID-2)
      IQ(LCID+3)=IQ(LCID+3)+1
      DO 20 I=1,NDIM
 
#if defined(CERNLIB_SUN)||defined(CERNLIB_SOLARIS)||defined(CERNLIB_SGI)||defined(CERNLIB_DECS)||defined(CERNLIB_ALPHA)||defined(CERNLIB_HPUX)||defined(CERNLIB_IBMRT)
* 	test the floating point
         JX=IFPS(X(I))
         IF(JX .EQ. 0) THEN
           WRITE(MSG,'(A,I10)') 'HFN bad float in column',I
           CALL HFPBUG(X(I),MSG,ID1)
         ENDIF
*
#endif
 
         IF(X(I).LT.Q(LLIMS+2*I-1))Q(LLIMS+2*I-1)=X(I)
         IF(X(I).GT.Q(LLIMS+2*I  ))Q(LLIMS+2*I  )=X(I)
         Q(LR2+IFIRST+I-1)=X(I)
   20 CONTINUE
      IQ(LCID+7)=IFIRST+NDIM
*
   30 RETURN
      END
 
 
 
 
 
 
 
 
 
 
 
 
 
 
