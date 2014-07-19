*
* $Id: hlabel.F,v 1.2 2001/10/10 08:12:07 couet Exp $
*
* $Log: hlabel.F,v $
* Revision 1.2  2001/10/10 08:12:07  couet
* - Do not call MZFORM any more. MZFORM for alphanumeric labels date banks
*   is now called  at initialisation time (HLIMIT or HLIMAP).
*
* Revision 1.1.1.1  1996/01/16 17:07:41  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.23/01 20/02/95  10.12.54  by  Julian Bunn
*-- Author :    P.Aubert   18/11/92
      SUBROUTINE HLABEL(IDD,NLABEL,CHLAB,CHOPT)
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C HLABEL :
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C ID          HISTOGRAM ID
C
C GENERAL REMARK
C --------------
C
C 1/ by default axis equals 'X', you could specifie 'X' or 'Y' in CHOPT.
C 2/ for a 2-D histogram, you must specifie 'X' or 'Y'
C 3/ option 'N' 'T' 'R' 'S' are mutally exclusive
C
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C CHOPT = 'N'
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C ADD LABEL TO AN HISTOGRAM WITHOUT LABEL
C
C IN
C --
C ID          HISTOGRAN ID
C NLABEL      NUMBER OF LABEL TO ADD TO AXIS 'X' or 'Y'
C CHLAB       ARRAY OF LABEL CHARACTER*(*) CHLAB*
C CHOPT       'N'='NX','NY'
C
C EXEMPLE :
C   CALL HLABEL(ID,10,CHLAB,'NX')
C
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C CHOPT = 'T'
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C CHANGE LABEL TO AN HISTOGRAM WICH ALREADY HAVE LABEL
C
C IN
C --
C ID          HISTOGRAN ID
C NLABEL      NUMBER OF LABEL TO CHANGE TO AXIS 'X' or 'Y'
C CHLAB       ARRAY OF LABEL CHARACTER*(*) CHLAB*
C CHOPT       'T'='TX','TY'
C
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C CHOPT = 'R'
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C GET BACK AN ARRAY 'CHLAB' CONTAINING THE ARRAY OF LABEL FOR AXIS 'X'
C OR 'Y'
C
C IN
C --
C ID          HISTOGRAN ID
C NLABEL      NUMBER OF LABEL TO STORE FROM AXIS 'X' or 'Y' TO CHLAB
C CHOPT       'R'='RX','RY'
C
C OUT
C ---
C CHLAB       ARRAY OF LABEL CHARACTER*(*) CHLAB*
C
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C CHOPT = 'S'
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C SORT OPTION
C
C IN
C --
C ID          HISTOGRAN ID
C CHOPT       S X Y A E V R
C
C EXAMPLE
C -------
C
C TO SORT X AXIS ALPHABETICALLY
C       CALL HLABEL(ID,0,' ','SAX')
C TO SORT Y AXIS ALPHABETICALLY
C       CALL HLABEL(ID,0,' ','SAY')
C
C THIS WORKS ONLY FOR 1-D HISTOGRAM
C ---------------------------------
C TO SORT X AXIS BY INCREASING CHANNEL CONTENTS
C       CALL HLABEL(ID,0,' ','SXD')
C TO SORT X AXIS BY DECREASING CHANNEL CONTENTS
C       CALL HLABEL(ID,0,' ','SXV')
C
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C ERROR
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C 'Unknowm histogram type'
C      histogram ID is unknown
C
C 'OPTION N and S are mutually exclusive'
C      you could add label and sort them at the same time
C      use 2 call to hlabel first with chopt 'N' and then 'S'
C
C 'FOR A 2-D HISTOGRAM CHOPT MUST CONTAIN X or Y'
C      you MUST have 'X' or 'Y' in chopt for a 2D histgram
C
C 'Y-AXIS DO NOT HAVE LABEL '
C      use hlabel with option 'N' before
C
C 'X-AXIS DO NOT HAVE LABEL '
C      use hlabel with option 'N' before
C
C 'NLABEL <= 0 '
C      nlabel must be > 0
C
C 'No data sorting for a 2D histo'
C      there is no sense to sort an 2-D histogram according to the
C      value of row or column
C
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#include "hbook/hcbook.inc"
#include "hbook/hcbits.inc"
#include "hbook/hcflag.inc"
#include "hbook/hcprin.inc"
#include "hbook/hcform.inc"
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C PARAMETER
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      INTEGER KLGRCX,KBLAB,KNCHX
      PARAMETER(KLGRCX = 16,KBLAB = 7,KNCHX = 2)
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C COMMON
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      INTEGER LCHX,LCHY,LCH,NCHX,NCHY,LINDEX,LTEMP,LGR
      EQUIVALENCE(LCHX   ,LHDUM(1))
      EQUIVALENCE(LCHY   ,LHDUM(2))
      EQUIVALENCE(LCH    ,LHDUM(3))
      EQUIVALENCE(LINDEX ,LHDUM(4))
      EQUIVALENCE(LTEMP  ,LHDUM(5))
      LOGICAL HLABEQ
      REAL HCX,HCXY
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C VARIABLE FOR QUICKSORT
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      INTEGER QUICKM
      PARAMETER (QUICKM = 20)
      INTEGER LEFT,RIGHT,LEVEL
      REAL X,W
      INTEGER STACKLEFT(QUICKM),STACKRIGHT(QUICKM)
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C VARIABLE INPUT OUTPUT
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      INTEGER IDD,NLABEL,JBIT
      CHARACTER*(*) CHOPT
      CHARACTER*(*) CHLAB(*)
      CHARACTER*8 CHOPTU
      CHARACTER*16 CHTMP
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C LOCAL VARIABLE
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      INTEGER I,J,K,L,IVALUE,LW,IOCC,DIMHISTO,IKNOENT
      INTEGER NBLOC
      REAL ERR
      CHARACTER CHAXE
      LOGICAL BUILD
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      NBLOC = NLABEL
C OPTIONS ?
      CHOPTU=CHOPT
      CALL CLTOU(CHOPTU)
      IOPTN=INDEX(CHOPTU,'N')
      IOPTS=INDEX(CHOPTU,'S')
      IOPTA=INDEX(CHOPTU,'A')
      IOPTE=INDEX(CHOPTU,'E')
      IOPTD=INDEX(CHOPTU,'D')
      IOPTV=INDEX(CHOPTU,'V')
      IOPTT=INDEX(CHOPTU,'T')
      IOPTX=INDEX(CHOPTU,'X')
      IOPTY=INDEX(CHOPTU,'Y')
      IOPTZ=INDEX(CHOPTU,'Z')
      IOPTR=INDEX(CHOPTU,'R')
C -- INIT VALUE
      CALL HFIND(IDD,'HLABEL')
      IF(LCID.EQ.0)GO TO 999
      CALL HDCOFL
      IF(I123.EQ.0)THEN
         CALL HBUG('Unknowm histogram type','HLABEL',IDD)
         GO TO 999
      ENDIF
C -- some test to detect inconsistant option and type of the histogram
C 1/ TOO MANY OPTIONS
      IF((IOPTN.NE.0).AND.(IOPTS.NE.0))THEN
         CALL HBUG('OPTION N and S are mutually exclusive','HLABEL',IDD)
         GO TO 999
      ENDIF
C 2/
      IF((I2.NE.0).AND.((IOPTX.NE.0).AND.(IOPTY.NE.0)))THEN
         CALL HBUG('FOR A 2-D HISTOGRAM CHOPT MUST CONTAIN X or Y',
     +   'HLABEL',IDD)
      ENDIF
      CALL SBIT0(IQ(LCID),5)
C -- We store the value because of side effect in the routine hfc1
      IKNOENT = IQ(LCONT + KNOENT)
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C ADD BANK IF THE LABEL'S BANK DON'T EXIST
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      NL=IQ(LCID-2)
C 1D FIRST
      IF(I1.NE.0)THEN
         DIMHISTO = 1
         IF(.NOT.HLABEQ(ID,'X'))THEN
            CALL MZPUSH(IHDIV,LCID,8-NL,0,' ')
            CALL MZBOOK(IHDIV,LCHX,LCID, -8,'HLCX',0,0,KBLAB,IOCC,0)
            IQ(LCHX + 1) = KLGRCX
            IQ(LCHX + KNCHX) = 0
            CALL UCTOH('UNDERFLOW       ',IQ(LCHX + 3),4,KLGRCX)
            CALL SBIT0(IQ(LCHX),17)
            CALL SBIT1(IQ(LCHX),18)
         ENDIF
C 2D
      ELSE
         DIMHISTO = 2
C
C -- s'il n'y a pas de label sur X ni sur Y, c'est clair qu'il faut les
C -- mettre
C -- par contre si nous n'avons des labels que sur
C -X
C    si l'action porte sur l'axe X et qu'elle ne concerne pas 'N'
C       pas de problemes
C    si l'action porte sur l'axe Y et qu'elle ne concerne pas 'N'
C       -> HBUG
C -Y
C    inverse evidement
C -- Y-a-t-il d'autres problemes des cas de bloquages ?
C
         BUILD = .FALSE.
         IF(HLABEQ(ID,'X'))THEN
            IF(.NOT.HLABEQ(ID,'Y'))THEN
               IF((IOPTY.NE.0).AND.(IOPTN.EQ.0))THEN
                  CALL HBUG('No labels on Y-AXIS ','HLABEL',ID)
                  RETURN
               ENDIF
            ENDIF
         ELSE
            IF(HLABEQ(ID,'Y'))THEN
               IF((IOPTX.NE.0).AND.(IOPTN.EQ.0))THEN
                  CALL HBUG('No labels on X-AXIS ','HLABEL',ID)
                  RETURN
               ENDIF
            ELSE
               BUILD = .TRUE.
            ENDIF
         ENDIF
C
         IF(BUILD)THEN
            CALL MZPUSH(IHDIV,LCID,9-NL,0,' ')
            CALL MZBOOK(IHDIV,LCHX,LCID, -8,'HLCX',0,0,KBLAB,IOCC,0)
            IQ(LCHX + 1) = KLGRCX
            IQ(LCHX + KNCHX) = 0
            CALL UCTOH('UNDERFLOW       ',IQ(LCHX + 3),4,KLGRCX)
            CALL SBIT0(IQ(LCHX),17)
            CALL SBIT1(IQ(LCHX),18)
            CALL MZBOOK(IHDIV,LCHY,LCID, -9,'HLCY',0,0,KBLAB,IOCC,0)
            IQ(LCHY + 1) = KLGRCX
            IQ(LCHY + KNCHX) = 0
            CALL UCTOH('UNDERFLOW       ',IQ(LCHY + 3),4,KLGRCX)
            CALL SBIT0(IQ(LCHY),17)
            CALL SBIT1(IQ(LCHY),18)
         ENDIF
      ENDIF
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C Axis Selection
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      IF(DIMHISTO.EQ.1)THEN
         LCH = LQ(LCID - 8)
         NCH = IQ(LCH + KNCHX)
         CHAXE = 'X'
      ELSE
C        -- Y
         IF (IOPTY.NE.0)THEN
            LCH = LQ(LCID - 9)
            NCH = IQ(LCH + KNCHX)
            CHAXE = 'Y'
C        -- X or default
         ELSE
            LCH = LQ(LCID - 8)
            NCH = IQ(LCH + KNCHX)
            CHAXE = 'X'
         ENDIF
      ENDIF
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C IF CHOPT = T
C CHANGE LABEL
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      IF(IOPTT.NE.0)THEN
         IF(NBLOC.GT.NCH)THEN
            NBLOC = NCH
         ENDIF
         IF(NBLOC.LE.0)THEN
            CALL HBUG('NLABEL <= 0 ','HLABEL',ID)
         ENDIF
         DO 10 I = 1,NBLOC
            CHTMP = '                '
            LGR   = LENOCC(CHLAB(I))
            IF(LGR.GT.KLGRCX)THEN
               LGR = KLGRCX
            ENDIF
            CHTMP(1:LGR) = CHLAB(I)(1:LGR)
            CALL UCTOH(CHTMP,IQ(LCH + KBLAB + 4*(I - 1)),4,KLGRCX)
   10    CONTINUE
         RETURN
      ENDIF
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C IF CHOPT = R
C RETURN ARRAY OF LABEL
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      IF(IOPTR.NE.0)THEN
         IF(NBLOC.GT.NCH)THEN
            NBLOC = NCH
         ENDIF
         IF(NBLOC.LE.0)THEN
            CALL HBUG('NLABEL <= 0 ','HLABEL',ID)
         ENDIF
         DO 20 I = 1,NBLOC
            CALL HLGNXT(ID,I,CHLAB(I),CHAXE)
   20    CONTINUE
         RETURN
      ENDIF
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C IF CHOPT = N
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      IF(IOPTN.NE.0)THEN
C req : il faut peut-etre mettre a jour des bits genre auto binning ...
         NBLOC = NBLOC + 2
         CALL HSPACE(NBLOC*4,'HLABEL',ID)
C -- if not enough memory
         IF(IERR.NE.0) GO TO 999
C -- else
         CALL MZPUSH(IHDIV,LCH,0,4*NBLOC,' ')
C -- we don''t count UNDERFLOW et OVERFLOW
         IQ(LCH + KNCHX) = NBLOC - 2
         CALL UCTOH('UNDERFLOW       ',IQ(LCH + KBLAB - 4),4,KLGRCX)
         DO 30  I = 1,NBLOC - 2
            CHTMP = '                '
            LGR   = LENOCC(CHLAB(I))
            IF(LGR.GT.KLGRCX)THEN
               LGR = KLGRCX
            ENDIF
            CHTMP(1:LGR) = CHLAB(I)(1:LGR)
            CALL UCTOH(CHTMP,IQ(LCH + KBLAB + 4*(I - 1)),4,KLGRCX)
   30    CONTINUE
         CALL UCTOH('OVERFLOW        ',IQ(LCH + KBLAB + 4*(NBLOC - 2)),
     +        4,KLGRCX)
C
         IF(DIMHISTO.EQ.2)THEN
            IF(CHAXE.EQ.'X')THEN
               Q(LCID+KXMIN) = 1.0
               Q(LCID+KXMAX) = FLOAT(IQ(LCID+KNCX))+1.0
            ELSE
               Q(LCID+KYMIN) = 1.0
               Q(LCID+KYMAX) = FLOAT(IQ(LCID+KNCY))+1.0
            ENDIF
         ENDIF
         RETURN
      ENDIF
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C IF CHOPT = S
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      IF(IOPTS.NE.0)THEN
C -- INIT VALUE
C -- we need an auxillary array of integer length = nch
         CALL HSPACE(NCH,'HLABEL',ID)
         IF(IERR.NE.0) GO TO 999
         CALL MZPUSH(IHDIV,LCHX,1,0,' ')
         CALL MZBOOK(IHDIV,LINDEX,LCHX,-1,'LIDX',0,0,NCH+2,2,0)
C -- HACK for hcx.f
         NB = IQ(LCONT + KNBIT)
C -- if error exist
         IF(LQ(LCONT).NE.0)THEN
            LW = LQ(LCONT)
         ENDIF
C -- call sort routine
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C IF CHOPT = A
C ALPHANUMERIC SORT
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C -- SI ON TRIE PAR ORDRE ALPHABETIQUE OK
C -- AUSSI SI ON VEUX TRIER PAR ORDRE INVERSE ALPHABETIQUE
         IF((IOPTA.NE.0).OR.(IOPTE.NE.0))THEN
C -- axe X pour l instant
C IF NOT AUTOMATICALLY SORT
            IF(JBIT(IQ(LCH),1).EQ.0)THEN
C              -- status bit =1 now
               CALL SBIT1(IQ(LCH),1)
C              -- sort label
               CALL HLSORT(IQ(LCH + KBLAB),IQ(LINDEX+1),NCH,KLGRCX)
C
               IF(DIMHISTO.EQ.1)THEN
C                -- add memory
                  CALL HSPACE(NCH,'HLABEL',ID)
                  IF(IERR.NE.0) GO TO 999
                  CALL MZPUSH(IHDIV,LINDEX,0,NCH+2,' ')
C                 -- sort value
                  DO 40 I = 1,NCH
                     Q(LINDEX + NCH + I) = HCX(IQ(LINDEX + I),1)
   40             CONTINUE
                  DO 50 I = 1,NCH
                     CALL HFCX(I,Q(LINDEX + NCH + I))
   50             CONTINUE
C                 -- sort ERROR
                  IF(LQ(LCONT).NE.0)THEN
                     DO 60 I = 1,NCH
                        Q(LINDEX + NCH + I) = Q(LW + IQ(LINDEX + I))
   60                CONTINUE
                     DO 70 I = 1,NCH
                        Q(LW + I) = Q(LINDEX + NCH + I)
   70                CONTINUE
                  ENDIF
               ELSE IF(DIMHISTO.EQ.2)THEN
                  IF(HLABEQ(ID,'X'))THEN
                     LCHX = LQ(LCID-8)
                     NCHX = IQ(LCHX+KNCHX)
                  ELSE
                     NCHX = IQ(LCID+KNCX)
                  ENDIF
                  IF(HLABEQ(ID,'Y'))THEN
                     LCHY = LQ(LCID-9)
                     NCHY = IQ(LCHY+KNCHX)
                  ELSE
                     NCHY = IQ(LCID+KNCY)
                  ENDIF
                  NB   = IQ(LCONT+KNBIT)
C                 -- we create a temporary buffer
                  CALL HSPACE(NCHX*NCHY,'HLABEL',ID)
C                 -- if not enough memory
                  IF(IERR.NE.0) GO TO 999
C                 -- else
                  CALL MZPUSH(IHDIV,LCID,1,0,' ')
                  CALL MZBOOK(IHDIV,LTEMP,LCID,-10,'TEMP',
     +              0,0,NCHX*NCHY,3,0)
C                 -- est-ce utile ? Cf hcxy ou c'est pas declare'
                  LSCAT  = LQ(LCID-1)
C                 -- store value in TEMP
                  DO 90 I=1,NCHX
                     DO 80 J=1,NCHY
                        Q(LTEMP+(J-1)*NCHX+I) = HCXY(I,J,1)
   80                CONTINUE
   90             CONTINUE
C                 -- sort value
                  IF(CHAXE.EQ.'X')THEN
                     DO 110 I=1,NCHX
                        DO 100 J=1,NCHY
                           CALL HFCXY(I,J,
     +                       Q(LTEMP+(J-1)*NCHX+IQ(LINDEX+I)))
  100                   CONTINUE
  110                CONTINUE
                  ELSE
                     DO 130 I=1,NCHX
                        DO 120 J=1,NCHY
                           CALL HFCXY(I,J,
     +                       Q(LTEMP+(IQ(LINDEX+J)-1)*NCHX+I))
  120                   CONTINUE
  130                CONTINUE
                  ENDIF
C ??? faut-il permuter les erreurs ???
C                 -- destroy temp bank
                  CALL MZDROP(IHDIV,LTEMP,' ')
                  CALL MZPUSH(IHDIV,LCID,-1,0,' ')
               ENDIF
            ENDIF
         ENDIF
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C IF CHOPT = D
C SORT BY DATA
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C -- on trie sur les donnees : dans les 2 cas ordre direct et inverse
C -- on rentre
         IF((IOPTD.NE.0).OR.(IOPTV.NE.0))THEN
C
C -- WARNING : we are not sort alphabetically now
            CALL SBIT0(IQ(LCH),1)
C
            IF(DIMHISTO.EQ.2)THEN
               CALL HBUG('No data sorting for a 2D histo','HLABEL',ID)
               RETURN
            ENDIF
C superbe trie quicksort : on devrait faire un test sur la taille de la
C pile
C
            LEVEL = 1
            STACKLEFT(1) = 1
            STACKRIGHT(1) = NCH
C
C -- REPEAT
  140       CONTINUE
            LEFT = STACKLEFT(LEVEL)
            RIGHT = STACKRIGHT(LEVEL)
            LEVEL = LEVEL - 1
C--   REPEAT
  150       CONTINUE
            I = LEFT
            J = RIGHT
            X = HCX((LEFT + RIGHT)/2,1)
C -- REPEAT
  160       CONTINUE
C -- WHILE
  170       CONTINUE
            IF(HCX(I,1).LT.X)THEN
               I = I + 1
               GO TO 170
            ENDIF
C -- END WHILE
C -- WHILE
  180       CONTINUE
            IF(X.LT.HCX(J,1))THEN
               J = J - 1
               GO TO 180
            ENDIF
C -- END WHILE
            IF(I.LE.J)THEN
               W = HCX(I,1)
               CALL HFCX(I,HCX(J,1))
               CALL HFCX(J,W)
C -- swap label i,j
               DO 190 L = 0,3
                  IVALUE = IQ(LCH + KBLAB + 4*(I - 1) + L)
                  IQ(LCH + KBLAB + 4*(I - 1) + L) = IQ(LCH + KBLAB + 4*
     +            (J - 1) + L)
                  IQ(LCH + KBLAB + 4*(J - 1) + L) = IVALUE
  190          CONTINUE
C -- swap error i,f
               IF(LQ(LCONT).NE.0)THEN
                  ERR = Q(LW + I)
                  Q(LW + I) = Q(LW + J)
                  Q(LW + J) = ERR
               ENDIF
C -- compute new left an dright
               I = I + 1
               J = J - 1
            ENDIF
            IF(I.LE.J) GO TO 160
C -- END REPEAT
            IF((J - LEFT).LT.(RIGHT - I))THEN
               IF(I.LT.RIGHT)THEN
                  LEVEL = LEVEL + 1
                  STACKLEFT(LEVEL) = I
                  STACKRIGHT(LEVEL) = RIGHT
               ENDIF
               RIGHT = J
            ELSE
               IF(LEFT.LT.J)THEN
                  LEVEL = LEVEL + 1
                  STACKLEFT(LEVEL) = LEFT
                  STACKRIGHT(LEVEL) = J
               ENDIF
               LEFT = I
            ENDIF
            IF(LEFT.LT.RIGHT) GO TO 150
C -- END REPEAT
            IF(LEVEL.NE.0)GO TO 140
C -- END REPEAT
C
         ENDIF
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C IF CHOPT = R ou V
C REVERSE ALPHANUMERIC SORT
C THIS PART OF HLABEL IS THE SAME IF YOU SORT REVERSE WITH RESPECT TO
C THE DATA VALUE OR THE LABEL
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
         IF((IOPTE.NE.0).OR.(IOPTV.NE.0))THEN
C -- WARNING : we are not sort alphabetically now
            CALL SBIT0(IQ(LCH),1)
C -- swap label
            DO 210 I = 1,NCH/2
               DO 200 J = 0,3
                  K = IQ(LCH + KBLAB + 4*(I - 1) + J)
                  IQ(LCH + KBLAB + 4*(I - 1) + J) =
     +                 IQ(LCH + KBLAB + 4*(NCH - I) + J)
                  IQ(LCH + KBLAB + 4*(NCH - I) + J) = K
  200          CONTINUE
  210       CONTINUE
C -- 1 D
            IF(DIMHISTO.EQ.1)THEN
C               -- swap value
               DO 220 I=1,NCH/2
                  X = HCX(I,1)
                  CALL HFCX(I,HCX(NCH - I + 1,1))
                  CALL HFCX(NCH - I + 1,X)
  220          CONTINUE
C               -- swap error
               IF(LQ(LCONT).NE.0)THEN
                  DO 230 I = 1,NCH/2
                     ERR = Q(LW + I)
                     Q(LW + I) = Q(LW + NCH + 1 - I)
                     Q(LW + NCH + 1 - I) = ERR
  230             CONTINUE
               ENDIF
            ELSE
C -- 2D
               IF(HLABEQ(ID,'X'))THEN
                  LCHX = LQ(LCID-8)
                  NCHX = IQ(LCHX+KNCHX)
               ELSE
                  NCHX = IQ(LCID+KNCX)
               ENDIF
               IF(HLABEQ(ID,'Y'))THEN
                  LCHY = LQ(LCID-9)
                  NCHY = IQ(LCHY+KNCHX)
               ELSE
                  NCHY = IQ(LCID+KNCY)
               ENDIF
C               -- swap value
               IF(CHAXE.EQ.'X')THEN
                  DO 250 I=1,NCHX/2
                     DO 240 J=1,NCHY
                        X = HCXY(I,J,1)
                        CALL HFCXY(I,J,HCXY(NCHX-I+1,J,1))
                        CALL HFCXY(NCHX-I+1,J,X)
  240                CONTINUE
  250             CONTINUE
               ELSE
                  DO 270 I=1,NCHY/2
                     DO 260 J=1,NCHX
                        X = HCXY(I,J,1)
                        CALL HFCXY(I,J,HCXY(I,NCHY-J+1,1))
                        CALL HFCXY(I,NCHY-J+1,X)
  260                CONTINUE
  270             CONTINUE
 
               ENDIF
C ??? swap error ???
            ENDIF
         ENDIF
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
         CALL MZDROP(IHDIV,LINDEX,' ')
      ENDIF
C -- END OPTION 'S'
C -- catch the old value ( should be modified by hfcx )
      IQ(LCONT + KNOENT) = IKNOENT
  999 END
