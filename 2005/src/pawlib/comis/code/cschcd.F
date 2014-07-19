*
* $Id: cschcd.F,v 1.1.1.1 1996/02/26 17:16:28 mclareni Exp $
*
* $Log: cschcd.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:28  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.16/05 16/08/93  16.39.44  by  Vladimir Berezhnoi
*-- Author : V.Berezhnoi
      SUBROUTINE CSCHCD(KOD,N)
      INTEGER KOD(N)
      PARAMETER ( NBYTPW=4)
      PARAMETER ( KFARGL=104,KFCALL=105)
      PARAMETER ( KLBAK=145, KLBA2K=146, KLBAL=192,
     + KLBAG=193, KLBAKA=202, KLBAA=203, KLBACK=204, KLBACL=205,
     + KLBACG=206, KBACA=207, KLBAHK=208    )
      PARAMETER ( KLAK=46,  KLACK=48,  KDCV=49,   KLAL=51, KLAG=53,
     +            KLAKA=57, KLACL=59,  KLACG=61,  KAA=86,  KACA=89,
     +            KLAHK=77, KCONT=109, KLADK=138, KLADL=140,KLADG=142,
     +            KLACXK=185, KLACXL=187, KLACXG=189 )
      I=1
 1    IF(N.EQ.0)RETURN
      IF(KOD(I).EQ.KLAK)THEN
         N=N-2
         KOD(I)=KLBAK
         I=I+2
      ELSEIF(KOD(I).EQ.KLACK)THEN
         KOD(I)=KLBACK
         LN=(KOD(I+1)-1)/NBYTPW+3
         I=I+LN
         N=N-LN
      ELSEIF(KOD(I).EQ.KDCV)THEN
         N=N-2
         I=I+2
      ELSEIF(KOD(I).EQ.KLAL)THEN
         N=N-2
         KOD(I)=KLBAL
         I=I+2
      ELSEIF(KOD(I).EQ.KLAG)THEN
         N=N-3
         KOD(I)=KLBAG
         I=I+3
      ELSEIF(KOD(I).EQ.KLAKA)THEN
         N=N-3
         KOD(I)=KLBAKA
         I=I+3
      ELSEIF(KOD(I).EQ.KLACL)THEN
         N=N-3
         KOD(I)=KLBACL
         I=I+3
      ELSEIF(KOD(I).EQ.KLACG)THEN
         N=N-4
         KOD(I)=KLBACG
         I=I+4
      ELSEIF(KOD(I).EQ.KAA)THEN
         N=N-2
         KOD(I)=KLBAA
         I=I+2
      ELSEIF(KOD(I).EQ.KACA)THEN
         N=N-2
         KOD(I)=KBACA
         I=I+2
      ELSEIF(KOD(I).EQ.KLAHK)THEN
         LN=KOD(I+1)+2
         N=N-LN
         KOD(I)=KLBAHK
         I=I+LN
      ELSEIF(KOD(I).EQ.KLADK)THEN
         N=N-3
         KOD(I)=KLBA2K
         I=I+3
      ELSEIF(KOD(I).EQ.KLADL)THEN
         N=N-2
         KOD(I)=KLBAL
         I=I+2
      ELSEIF(KOD(I).EQ.KLADG)THEN
         N=N-3
         KOD(I)=KLBAG
         I=I+3
      ELSEIF(KOD(I).EQ.KLACXK)THEN
         N=N-3
         KOD(I)=KLBA2K
         I=I+3
      ELSEIF(KOD(I).EQ.KLACXL)THEN
         N=N-2
         KOD(I)=KLBAL
         I=I+2
      ELSEIF(KOD(I).EQ.KLACXG)THEN
         N=N-3
         KOD(I)=KLBAG
         I=I+3
      ELSEIF(KOD(I).EQ.KCONT)THEN
         N=N-1
         I=I+1
      ELSE
      print *,' error in internal codes during optimization',KOD(I)
         RETURN
      ENDIF
      GO TO 1
      END
