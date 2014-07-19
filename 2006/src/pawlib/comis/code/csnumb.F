*
* $Id: csnumb.F,v 1.5 1998/02/27 09:26:30 couet Exp $
*
* $Log: csnumb.F,v $
* Revision 1.5  1998/02/27 09:26:30  couet
* - The problem with the E notation is now fixed (also on HP)
*
* Revision 1.4  1998/02/16 09:38:28  couet
* - Back to the previous version of this routine. The last fix done make the
*   follwoing COMIS routine crashes on HPUX:
*
*       subroutine e
*       err=2.E-5
*       print*, err
*       end
*
* Revision 1.3  1998/01/16 11:28:23  couet
* - 1.e-2 was not understood
*
* Revision 1.2  1996/12/03 16:36:09  berejnoi
* Mods in csnumb:more then 30 digits in a number
* Mods in cscrexec: for AIX -berok added to solve problem
* with incremental dyn. loading
*
* Revision 1.1.1.1  1996/02/26 17:16:22  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.18/05 27/09/93  15.12.28  by  Vladimir Berezhnoi
*-- Author :
      INTEGER FUNCTION CSNUMB(J,N,I,R,D)
***--------------------------------------------
*       parse f77 numbers
***--------------------------------------------
      COMMON/CSCONT/LCONT
*OUTPUT     ERROR  NOT NUMBER  INT   REAL   DP
*   CSNUMB=  -15        0       1     2     5
*                               I     R     D
      DOUBLE PRECISION D,D1
      INTEGER MD(40)
      PARAMETER (MAXMD=40,  MAXIN=10  ,MAXEXP=38)
      INTEGER GSNLAB
      COMMON /CSGSCM/IGSST,JGSST,NGSST,NGSPAR,JGSSB,GSNLAB
****
*     SB   -> LDIG             ^SIP
*             '.'  ASAVE LDIG  ^SFRACR
*             ANOT
*     SIP  -> LDIG              ^SIP
*             BLNK  AINT
*             '.'   ASAVE        ^SFRAC
*             ALE   AR1          ^EXP
*             ALD   AD1          ^EXP
*             AINT
*     SFRAC -> LDIG              ^SFRACR
*              'EQ'   AREST       AINT
*              ALE    AR1          ^EXP
*              ALD    AD1          ^EXP
*           'NE'/'LT'/'LE'/'GE'/'GT'/'AND'/'OR' AREST AINT
*              ARFN
*     SFRACR -> LDIG                  ^SFRACR
*              ALE    ARF             ^EXP
*              ALD    ADF             ^EXP
*              ARFY
*     EXP  -> '+'                ^EXP1
*            '-'       AEM       ^EXP1
*                                ^EXP1
*     EXP1 -> LDIG     AEXP      ^EXP2
*             E(15)
*     EXP2 -> LDIG    AEXP      ^EXP2
*             ARDE
****
      INTEGER GSCMST(194),GSSTRC(5),GSSTRP(23)
      DATA GSCMST/
     *7,6,2,8,21,0,17,4,1,6,5,6,2,8,127,0,0,6,15,0,27,6,2,8,21,0,33,6,
     *18,6,11,0,41,4,1,6,5,8,61,0,49,6,16,6,3,8,153,0,57,6,17,6,4,8,153,
     *0,0,6,11,0,67,6,2,8,127,0,75,4,3,6,6,6,11,0,83,6,16,6,3,8,153,0,
     *91,6,17,6,4,8,153,0,123,4,5,5,118,4,7,5,118,4,9,5,118,4,11,5,118,
     *4,13,5,118,4,15,5,118,4,17,6,6,6,11,0,0,6,12,0,133,6,2,8,127,0,
     *141,6,16,6,7,8,153,0,149,6,17,6,8,8,153,0,0,6,13,0,159,4,19,8,171,
     *0,167,4,21,6,9,8,171,0,0,8,171,0,179,6,2,6,10,8,183,0,0,3,15,0,
     *191,6,2,6,10,8,183,0,0,6,14,0/
      DATA GSSTRC/
     *4H.EQN,4HELTL,4HEGEG,4HTAND,4HOR+-/
      DATA GSSTRP/
     *0,1,1,2,3,2,5,2,7,2,9,2,11,2,13,3,16,2,18,1,19,1,-1/
****
****    Romove the following statements, and replace them
****     with explicit calls to MJSCHA when the varaiables
****     JSCE and JSCD are needed.
****
*     DATA NENTRY/0/
*     IF(NENTRY.EQ.0)THEN
*       NENTRY=1
*       JSCE=MJSCHA('E')
*       JSCD=MJSCHA('D')
*     ENDIF
      LCONT=1
      NI=0
      LAST=0
      IEXP=0
      ISIGN=1
      JGSST=J
      NGSST=N
      JSV=J
      NSV=N
    1 CALL CSGSCL(GSCMST(1),GSSTRC(1),GSSTRP(1),II)
      GO TO (2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19),GSNLAB
    2 CONTINUE
      IF(IGSST.EQ.10)IGSST=0
      CSNUMB=IGSST
      J=JGSST
      N=NGSST
      LCONT=0
      RETURN
*   LDIG
    3 CONTINUE
      IGSST=0
      IF(NGSST.LE.0)GO TO 1
      ISYM=MKCHAR(JGSST)
      IF(MLDIGI(ISYM).LT.1) GO TO 1
      LAST=LAST+1
      IF(LAST.GT.MAXMD)GO TO 715
      MD(LAST)=MDIGIT(ISYM)
      IGSST=1
      JGSST=JGSST+1
      NGSST=NGSST-1
      GO TO 1
*   AR1
    4 CONTINUE
      NI=LAST
      ITYP=2
      GO TO 1
*   AD1
    5 CONTINUE
      NI=LAST
      ITYP=5
      GO TO 1
*   ASAVE
    6 CONTINUE
      JSV=JGSST-1
      NSV=NGSST+1
      NI=LAST
      GO TO 1
*   AREST
    7 CONTINUE
      JGSST=JSV
      NGSST=NSV
      GO TO 1
*   ARF
    8 CONTINUE
      ITYP=2
      GO TO 1
*   ADF
    9 CONTINUE
      ITYP=5
      GO TO 1
*   AEM
   10 CONTINUE
      ISIGN=-1
      GO TO 1
*   AEXP
   11 CONTINUE
      IEXP=IEXP*10+MD(LAST)
      LAST=LAST-1
      GO TO 1
*   AINT
   12 CONTINUE
      IF(LAST.GT.MAXIN)GO TO 715
      I=0
      DO 101 K=1,LAST
      I=I*10+MD(K)
 101  CONTINUE
      GO TO 1
*   ARFN
   13 CONTINUE
      R=0.
      DO 102 K=1,LAST
 102  R=R*10.+MD(K)
      IGSST=2
      GO TO 1
*   ARFY
   14 CONTINUE
      R=0.
      IEXP=NI-LAST
      DO 103 K=1,NI
 103  R=R*10.+MD(K)
      FR=0.
      DO 104 K=NI+1,LAST
 104  FR=FR*10.+MD(K)
      R=R+FR*10.**IEXP
      IGSST=2
      GO TO 1
*   ARDE
   15 CONTINUE
      D=0.D0
      D1=D
      IEXP1=0
      IEXP=IEXP*ISIGN
      DO 105 K=1,NI
      IF(MD(K).NE.0)GO TO 106
 105  IEXP1=IEXP1+1
 106  IEXP1=NI-IEXP1
      IF(IABS(IEXP+IEXP1).GT.MAXEXP) GO TO 715
      DO 107 K=1,NI
 107  D=D*10.D0+MD(K)
      DO 108 K=NI+1,LAST
 108  D1=D1*10.D0+MD(K)
      D=(D+D1*10.D0**(NI-LAST))*10.D0**IEXP
      R=D
      IGSST=ITYP
      GO TO 1
*   ANOT
   16 CONTINUE
      JGSST=JSV
      NGSST=NSV
      IGSST=10
      GO TO 1
*   ALE
   17 CONTINUE
****
****  the original statement here was JCH=JSCE
      ICHB=ICHAR('E')
      ICHs=ICHAR('e')
****
  109 IGSST=0
      IF(NGSST.GT.0)THEN
        ISYM=MKCHAR(JGSST)
        IF(ISYM.EQ.ICHB .or.ISYM.eq.ICHs)THEN
           JGSST=JGSST+1
           NGSST=NGSST-1
           IGSST=1
         ENDIF
      ENDIF
      GO TO 1
*   ALD
   18 CONTINUE
****
**** the original statement here was JCH=JSCD
      ICHB=ICHAR('D')
      ICHs=ICHAR('d')
****
      GO TO 109
*   BLNK
   19 CONTINUE
      IGSST=0
      ISYM=MKCHAR(JGSST)
      IF(ISYM.EQ.ICHAR(' '))IGSST=1
      GO TO 1
 715  IGSST=-15
      GO TO 1
      END
