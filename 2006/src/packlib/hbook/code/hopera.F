*
* $Id: hopera.F,v 1.2 1996/08/28 09:48:46 couet Exp $
*
* $Log: hopera.F,v $
* Revision 1.2  1996/08/28 09:48:46  couet
* *** empty log message ***
*
* Revision 1.1.1.1  1996/01/16 17:07:44  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.23/01 20/02/95  09.49.13  by  Julian Bunn
*-- Author :
      SUBROUTINE HOPERA(ID1,CHOP,ID2,ID3,CC1,CC2)
*.==========>
*.           Operations between histograms
*.             CHOP='+'   ID3=C1*ID1+C2*ID2
*.             CHOP='-'   ID3=C1*ID1-C2*ID2
*.             CHOP='*'   ID3=C1*ID1*C2*ID2
*.             CHOP='/'   ID3=C1*ID1/(C2*ID2)
*.             If option 'E' errors will be computed on ID3
*.             If option '/B' is given , binomial errors are computed.
*..=========> ( R.Brun )
#include "hbook/hcflag.inc"
#include "hbook/hcbook.inc"
#include "hbook/hcprin.inc"
#include "hbook/hcbits.inc"
      CHARACTER*(*) CHOP
      LOGICAL HLABEQ,HL1,HL2
*.___________________________________________
      C1=1.
      C2=1.
      N=6
      CALL NOARG(N)
      IF(N.GE.5)C1=FLOARG(CC1)
      IF(N.GE.6)C2=FLOARG(CC2)
*
      IBE=0
      IF(INDEX(CHOP,'+').NE.0)THEN
         KOP=1
      ELSEIF(INDEX(CHOP,'-').NE.0)THEN
         KOP=2
      ELSEIF(INDEX(CHOP,'*').NE.0)THEN
         KOP=3
      ELSEIF(INDEX(CHOP,'/').NE.0)THEN
         KOP=4
         IF(INDEX(CHOP,'B').NE.0)IBE=1
         IF(INDEX(CHOP,'b').NE.0)IBE=1
      ELSE
         GO TO 90
      ENDIF
      IOPTE=0
      IF(INDEX(CHOP,'E').NE.0)IOPTE=1
      IF(INDEX(CHOP,'e').NE.0)IOPTE=1
*
*             Find Address of ID1
*
      IERR=0
      CALL HFIND(ID1,'HOPERA')
      IF(LCID.EQ.0)GO TO 99
      HL1=HLABEQ(ID1,'X')
      LR1=LCID
      NWTOT=IQ(LR1+KNTOT)
      CALL HNOENT(ID1,N1)
*
*             Find Address of ID2
*
      CALL HFIND(ID2,'HOPERA')
      IF(LCID.EQ.0)GO TO 99
      HL2=HLABEQ(ID2,'X')
      LR2=LCID
      CALL HNOENT(ID2,N2)
*
*             Find Address of ID3 or create it
*
      ID=ID3
      IDPOS=LOCATI(IQ(LTAB+1),IQ(LCDIR+KNRH),ID3)
      IF(IDPOS.LE.0)THEN
         CALL HSPACE(NWTOT+100,'HOPERA',ID1)
         IF(IERR.NE.0)GO TO 99
         CALL HCOPY(ID1,ID3,' ')
         CALL HFIND(ID3,'HOPERA')
      ELSE
         LCID=LQ(LTAB-IDPOS)
         CALL SBIT0(IQ(LCID),5)
      ENDIF
      CALL HDCOFL
      IF(I1.NE.0.AND.I9.EQ.0.AND.IOPTE.NE.0)THEN
         CALL HBARX(ID3)
      ENDIF
      IF(I230.NE.0.AND.IOPTE.NE.0)THEN
         CALL HBAR2(ID3)
         CALL HBARX(ID3)
         CALL HBARY(ID3)
      ENDIF
*
*======>     Array
*
      IF(I123.EQ.0)THEN
         IF(I4.NE.0)GO TO 90
         IF(IQ(LR1-1).NE.IQ(LR2-1).OR.IQ(LR1-1).NE.IQ(LCID-1))GO TO 90
         DO 10 I=1,IQ(LCID-1)
            IF(KOP.EQ.1)THEN
               Q(LCID+I)=C1*Q(LR1+I)+C2*Q(LR2+I)
            ELSEIF(KOP.EQ.2)THEN
               Q(LCID+I)=C1*Q(LR1+I)-C2*Q(LR2+I)
            ELSEIF(KOP.EQ.3)THEN
               Q(LCID+I)=C1*Q(LR1+I)*C2*Q(LR2+I)
            ELSEIF(KOP.EQ.4)THEN
               DIV=C2*Q(LR2+I)
               IF(DIV.NE.0.)THEN
                  Q(LCID+I)=C1*Q(LR1+I)/DIV
               ELSE
                  Q(LCID+I)=0.
               ENDIF
            ENDIF
  10     CONTINUE
         GO TO 99
      ENDIF
*
*======>     1-DIM histogram
*
      IF(I1.NE.0)THEN
         NCX1=IQ(LR1+KNCX)
         NCX2=IQ(LR2+KNCX)
         NCX3=IQ(LCID+KNCX)
*
*     Histogram with Alphanumeric labels: 4 possible cases: (only with '+')
*       1- ID1 ID1 ID1  : process like normal case
*       2- ID1 ID2 ID1  : loop on ID2 and fill ID1
*       3- ID1 ID2 ID2  : loop on ID1 and fill ID2
*       4- ID1 ID2 ID3  : loop on ID1 and fill ID3, loop on ID2 and fill ID3
         IF(KOP.EQ.1.AND.(HL1.OR.HL2))THEN
            IF(ID1.EQ.ID2.AND.ID2.EQ.ID3)GO TO 12
            IF(ID3.EQ.ID1)THEN
               IF(N2.NE.0)CALL HOPER1A(NCX2,ID2,ID3,C2)
            ELSEIF(ID3.EQ.ID2)THEN
               IF(N1.NE.0)CALL HOPER1A(NCX1,ID1,ID3,C1)
            ELSE
               CALL HRESET(ID3,' ')
               IF(N1.NE.0)CALL HOPER1A(NCX1,ID1,ID3,C1)
               IF(N2.NE.0)CALL HOPER1A(NCX2,ID2,ID3,C2)
            ENDIF
            CALL HFIND(ID3,'HOPERA')
            LC3=LQ(LCID-1)
            IQ(LC3+KNOENT)=N1+N2
            GO TO 99
         ENDIF
  12     IF(NCX1.NE.NCX2.OR.NCX1.NE.NCX3)GO TO 90
         LPRX=LCID+KNCX
         CALL HOPER1(KOP,NCX1,LQ(LR1-1),LQ(LR2-1),LQ(LCID-1),C1,C2,IBE)
         GO TO 99
      ENDIF
*
*=====>     2-DIM histogram
*
      IF(I230.NE.0)THEN
         NCX1=IQ(LR1+KNCX)
         NCY1=IQ(LR1+KNCY)
         NCX2=IQ(LR2+KNCX)
         NCY2=IQ(LR2+KNCY)
         NCX3=IQ(LCID+KNCX)
         NCY3=IQ(LCID+KNCY)
         IF(NCX1.NE.NCX2.OR.NCX1.NE.NCX3)GO TO 90
         IF(NCY1.NE.NCY2.OR.NCY1.NE.NCY3)GO TO 90
         LC1=LQ(LR1-1)
         LC2=LQ(LR2-1)
         LC3=LQ(LCID-1)
         LW1=LQ(LC1)
         LW2=LQ(LC2)
         LW3=LQ(LC3)
         KN=IQ(LC1+KNOENT)+IQ(LC2+KNOENT)
         LCONT=LC3
         D1=C1*C1
         D2=C2*C2
         DO 30 I=0,NCX1+1
            DO 20 J=0,NCY1+1
               IF(I.GT.0.AND.I.LE.NCX1.AND.J.GT.0.AND.J.LE.NCY1)THEN
                  KW=NCX1*(J-1)+I
                  IF(LW3.EQ.0)KW=0
               ELSE
                  KW=0
               ENDIF
               LSCAT=LC1
               NB=IQ(LSCAT+KNBIT)
               R1=HCXY(I,J,1)
               S1=ABS(R1)
               IF(LW1.NE.0.AND.KW.NE.0)THEN
                  S1=Q(LW1+KW)
               ENDIF
               LSCAT=LC2
               NB=IQ(LSCAT+KNBIT)
               R2=HCXY(I,J,1)
               S2=ABS(R2)
               IF(LW2.NE.0.AND.KW.NE.0)THEN
                  S2=Q(LW2+KW)
               ENDIF
               LSCAT=LC3
               NB=IQ(LSCAT+KNBIT)
               IF(KOP.EQ.1)THEN
                  R3=C1*R1+C2*R2
                  IF(KW.NE.0)THEN
                     S3=D1*S1+D2*S2
                  ENDIF
               ELSEIF(KOP.EQ.2)THEN
                  R3=C1*R1-C2*R2
                  IF(KW.NE.0)THEN
                     S3=D1*S1+D2*S2
                  ENDIF
               ELSEIF(KOP.EQ.3)THEN
                  R3=C1*R1*C2*R2
                  IF(KW.NE.0)THEN
                     S3=D1*D2*(S1*R2*R2+S2*R1*R1)
                  ENDIF
               ELSEIF(KOP.EQ.4)THEN
                  DIV=C2*R2
                  IF(DIV.NE.0.)THEN
                     R3=C1*R1/DIV
                     IF(KW.NE.0)THEN
                        IF(IBE.EQ.0)THEN
                           S3=D1*D2*(S1*R2*R2+S2*R1*R1)/DIV**4
                        ELSE
                           S3=ABS(R3*(1.-R3)/DIV)
                        ENDIF
                     ENDIF
                  ELSE
                     R3=0.
                     S3=0.
                  ENDIF
               ENDIF
               CALL HFCXY(I,J,R3)
               IF(KW.NE.0)THEN
                  Q(LW3+KW)=S3
               ENDIF
  20        CONTINUE
  30     CONTINUE
         IQ(LC3+KNOENT)=KN
*
*           PROX
*
         LPRX=LCID+KNCX
         CALL HOPER1(KOP,NCX1,LQ(LR1-2),LQ(LR2-2),LQ(LCID-2),C1,C2,IBE)
*
*           PROY
*
         LPRX=LCID+KNCY
         CALL HOPER1(KOP,NCY1,LQ(LR1-3),LQ(LR2-3),LQ(LCID-3),C1,C2,IBE)
*
*           SLIX
*
         LSLIX=LQ(LCID-4)
         LC1=LQ(LR1-4)
         LC2=LQ(LR2-4)
         LPRX=LCID+KNCX
         IF(LSLIX.NE.0)THEN
            DO 40 I=1,IQ(LSLIX-2)
               CALL HOPER1(KOP,NCX1,LQ(LC1-I),LQ(LC2-I),LQ(LSLIX-I)
     +                     ,C1,C2,IBE)
  40        CONTINUE
         ENDIF
*
*           SLIY
*
         LSLIY=LQ(LCID-5)
         LC1=LQ(LR1-5)
         LC2=LQ(LR2-5)
         LPRX=LCID+KNCY
         IF(LSLIY.NE.0)THEN
            DO 50 I=1,IQ(LSLIY-2)
               CALL HOPER1(KOP,NCY1,LQ(LC1-I),LQ(LC2-I),LQ(LSLIY-I)
     +                     ,C1,C2,IBE)
  50        CONTINUE
         ENDIF
*
*           BANX
*
         LBANX=LQ(LCID-6)
         LC1=LQ(LR1-6)
         LC2=LQ(LR2-6)
         LPRX=LCID+KNCX
  60     IF(LBANX.NE.0)THEN
            CALL HOPER1(KOP,NCX1,LQ(LC1-1),LQ(LC2-1),LQ(LBANX-1)
     +                    ,C1,C2,IBE)
            LBANX=LQ(LBANX)
            IF(LBANX.NE.0)THEN
               LC1=LQ(LC1)
               IF(LC1.NE.0)THEN
                  LC2=LQ(LC2)
                  IF(LC2.NE.0)GO TO 60
               ENDIF
            ENDIF
         ENDIF
*
*           BANY
*
         LBANY=LQ(LCID-7)
         LC1=LQ(LR1-7)
         LC2=LQ(LR2-7)
         LPRX=LCID+KNCY
  70     IF(LBANY.NE.0)THEN
            CALL HOPER1(KOP,NCY1,LQ(LC1-1),LQ(LC2-1),LQ(LBANY-1)
     +                    ,C1,C2,IBE)
            LBANY=LQ(LBANY)
            IF(LBANY.NE.0)THEN
               LC1=LQ(LC1)
               IF(LC1.NE.0)THEN
                  LC2=LQ(LC2)
                  IF(LC2.NE.0)GO TO 70
               ENDIF
            ENDIF
         ENDIF
      ENDIF
      GO TO 99
*
  90  CALL HBUG('Histograms with different specifications','HOPERA',ID1)
*
  99  RETURN
      END
