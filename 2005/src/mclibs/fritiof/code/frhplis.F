*
* $Id: frhplis.F,v 1.1.1.1 1996/01/11 14:05:21 mclareni Exp $
*
* $Log: frhplis.F,v $
* Revision 1.1.1.1  1996/01/11 14:05:21  mclareni
* Fritiof
*
*
C********************************* END FREDIPY ***************************
 
C********************************* FRHPLIS *******************************
 
      SUBROUTINE FRHPLIS
 
C.........TO LIST THE HARD PARTONS EXTRACTED FROM PYTHIA EVENT RECORD
C.........in case one wants to examing it                       ......
 
      COMMON/LUDAT1/MSTU(200),PARU(200),MSTJ(200),PARJ(200)
      COMMON/PYPARS/MSTP(200),PARP(200),MSTI(200),PARI(200)
      COMMON/FRCNUT/NR,KR(10,5),PR(10,5),NR0
      COMMON/FRJETS/NJ,KJ(100,5),PJ(100,5)
      COMMON/FRPICKJ/NH,KP(100,5),PP(100,5)
      SAVE /LUDAT1/,/PYPARS/,/FRCNUT/,/FRJETS/,/FRPICKJ/
 
      WRITE(MSTU(11),*) '===================================='
      WRITE(MSTU(11),10) MSTI(5)
      WRITE(MSTU(11),15) MSTI(11),MSTI(12)
      WRITE(MSTU(11),20) MSTI(1), MSTI(2)
      WRITE(MSTU(11),30) MSTI(3),MSTI(31)
      WRITE(MSTU(11),50) MSTI(7),MSTI(8)
      WRITE(MSTU(11),60) MSTI(13),MSTI(14)
      WRITE(MSTU(11),70) MSTI(15),MSTI(16),(MSTI(L),l=21,24)
      WRITE(MSTU(11),80) PARI(33), PARI(34)
 
      WRITE(MSTU(11),*) ' NH=',NH, '    -- FRPICKJ '
      IF(NH.GT.0) THEN
      DO 301 J=1, NH
301   WRITE(MSTU(11),3401) J, (KP(J,L),L=1,5),(PP(J,L),L=1,5)
      ENDIF
      WRITE(MSTU(11),*) ' NJ=',NJ, '    -- FRJETS '
      IF(NJ.GT.0) THEN
      DO 303 J=1, NJ
303   WRITE(MSTU(11),3401) J, (KJ(J,L),L=1,5),(PJ(J,L),L=1,5)
      ENDIF
 
      WRITE(MSTU(11),*) ' NR=',NR, '    -- FRCNUT '
      IF(NR.GT.0) THEN
      DO 305 J=1, NR
305   WRITE(MSTU(11),3401) J, (KR(J,L),L=1,5),(PR(J,L),L=1,5)
      ENDIF
 
10    FORMAT( /' PARTON LIST AT ',I6,2X,'-th CALL TO PYTHIA',/)
15    FORMAT( ' Collision between ',I5,' & ',I5,/)
20    FORMAT( ' Subprocess type - MSTI(1,2): ',2I6 )
30    FORMAT( ' No. of partons produced: ',I6, 2X,
     >          ' No. of interactions: ', I3 )
50    FORMAT( ' Parton line number - MSTI(7,8): ',2I6 )
60    FORMAT( ' Initial shower initiaters - MSTI(13,14): ',2I5 )
70    FORMAT( ' The process is ',I4,' +',I4,' ->',I4,' +',I4,
     >     I4,' +',I4 )
80    FORMAT( ' X_1, X_2 = ',2G13.6, / )
 
3401  FORMAT(1X,I3,';',2x,5I6,2X,5F10.4, ' -- K, P ' )
      RETURN
      END
