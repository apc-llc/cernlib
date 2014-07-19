*
* $Id: h301m.F,v 1.1.1.1 1996/04/01 15:01:30 mclareni Exp $
*
* $Log: h301m.F,v $
* Revision 1.1.1.1  1996/04/01 15:01:30  mclareni
* Mathlib gen
*
*
#include "gen/pilot.h"
      SUBROUTINE H301M
C     H301   ASSNDX

#include "iorc.inc"

      LOGICAL LOK

      PARAMETER (IDQ = 10, IDW = 11)

      DIMENSION Q(IDQ,IDQ),Q1(IDQ,IDQ),Q2(IDQ,IDQ),IP(IDQ),IQ(IDQ)
      DIMENSION IW(6*IDW)
      DIMENSION IQ1(10,10),IQ2(10,10)
      DIMENSION JRC(10,2,2),JCR(10,2,2),JRCS(2,2),JCRS(2,2)

      DATA ((IQ1(I,J),J=1,10),I=1,10)
     1/ 622, 955, 808, 368, 508, 106,-348,-375,-720, 444,
     2  220,-849, 625,-189, 349,-658, 175, 991, 981, 724,
     3  941, 291,-218, 876, 837,-587, 117,-383,-789, 947,
     4  360,-626, 913,-898,-547,-457,-885, 938, 636,-289,
     5 -899, 518, 665,-502,-512,-311, 787, 632,-499, 546,
     6  930,-289,-849, 237, 862, 710, 934, 425, 328,-353,
     7 -440,-184, 381,-699, 685, 559, 265, 448, 205,-867,
     8  739,-750,-677, 114,-164, 959, 481,-449, 644,-907,
     9 -169,-618,-917, 615,-210, 806,-630, 351, 507, 310,
     A -661,-824, 224,-565,-670, 766, 732, 556,-774,-606/

      DATA ((IQ2(I,J),J=1,10),I=1,10)
     1/ 377, 537,-888,-821, 138,-908,-869, 869, 720,-551,
     2  281, 266,-368, 872, 209, 297,-832,-958,-582, 619,
     3  757, 426, 680, 555, 898,-446,-788,-893, 216,-167,
     4  788, 925,-788,-556,-569,-957,-962,-926, 776, 390,
     5  823,-920, 853,-415, 821, 513, 589,-881, 408, 365,
     6 -730, 143, 270,-160,-177, 766, 577,-543, 196, 435,
     7 -133, 267, 928,-725,-661,-951,-960,-637,-571, 403,
     8 -314, 630,-665, 473, 501,-704, 272, 643, 570,-645,
     9  343, 865, 233, 554,-375,-441,-263,-337, 834,-371,
     A -983,-103, 363,-147,-296, 572,-194, 583, 631,-773/

      DATA (JRC(I,1,1),I=1, 5),JRCS(1,1) /105,202,303,404,501, -2356/
      DATA (JCR(I,1,1),I=1, 5),JCRS(1,1) /301,502,203,104,405, -1921/

      DATA (JRC(I,1,2),I=1, 5),JRCS(1,2) /109,202,306,404,501, -3953/
      DATA (JCR(I,1,2),I=1,10),JCRS(1,2) /001,502,103,004,005,
     1                                    406,307,208,009,010, -4511/

      DATA (JRC(I,2,1),I=1,10),JRCS(2,1) /100,202,300,404,501,
     1                                    600,700,800,903,1005,-4233/
      DATA (JCR(I,2,1),I=1, 5),JCRS(2,1)/1001,502,403,104,705, -4173/

      DATA (JRC(I,2,2),I=1,10),JRCS(2,2) /109,202,306,404,501,
     1                                    603,710,808,907,1005,-7418/
      DATA (JCR(I,2,2),I=1,10),JCRS(2,2) /601,502,803,104,905,
     1                                    406,707,308,209,1010, -7676/

      CALL HEADER('H301',0)

      DO 8 I = 1,10
      DO 8 J = 1,10
      Q1(I,J)=IQ1(I,J)
    8 Q2(I,J)=IQ2(I,J)

      LOK=.TRUE.
      DO 1 M = 5,10,5
      DO 1 N = 5,10,5
C     WRITE(LOUT,'(A1,7X,''M ='',I3,5X,''N ='',I3/)') '1',M,N
      DO 1 MODE = 1,2
      IF(MODE .EQ. 1) THEN
       CALL RMCPY(10,10,Q1(1,1),Q1(1,2),Q1(2,1),Q(1,1),Q(1,2),Q(2,1))
C      IF(M .EQ.  5) WRITE(LOUT,'( 5(5X,F6.0)/)') ((Q(I,J),J=1, 5),I=1,N)
C      IF(M .EQ. 10) WRITE(LOUT,'(10(5X,F6.0)/)') ((Q(I,J),J=1,10),I=1,N)
       CALL ASSNDX(1,Q,N,M,IDQ,IP,SUM,IW,IDW)
C      WRITE(LOUT,'(//7X,''MODE ='',I3,5X,
C    1           ''ROW TO COLUMN;   SUM = '',F8.0 /)') MODE,SUM
C      WRITE(LOUT,'(7X,10(4X,2I3))') (I,IP(I),I=1,N)

       DO 11 I = 1,N
   11  LOK=LOK .AND. JRC(I,N/5,M/5) .EQ. 100*I+IP(I)
       LOK=LOK .AND. -JRCS(N/5,M/5) .EQ. NINT(-SUM)

C      WRITE(LOUT,'(1X)')
C      WRITE(LOUT,'(1X)')
      ELSE
       CALL RMCPY(10,10,Q2(1,1),Q2(1,2),Q2(2,1),Q(1,1),Q(1,2),Q(2,1))
C      IF(M .EQ.  5) WRITE(LOUT,'( 5(5X,F6.0)/)') ((Q(I,J),J=1, 5),I=1,N)
C      IF(M .EQ. 10) WRITE(LOUT,'(10(5X,F6.0)/)') ((Q(I,J),J=1,10),I=1,N)
       CALL ASSNDX(2,Q,N,M,IDQ,IQ,SUM,IW,IDW)
C      WRITE(LOUT,'(//7X,''MODE ='',I3,5X,
C    1           ''COLUMN TO ROW;   SUM = '',F8.0 /)') MODE,SUM
C      WRITE(LOUT,'(7X,10(4X,2I3))') (IQ(I),I,I=1,M)

       DO 12 I = 1,M
   12  LOK=LOK .AND. JCR(I,N/5,M/5) .EQ. 100*IQ(I)+I
       LOK=LOK .AND. -JCRS(N/5,M/5) .EQ. NINT(-SUM)

C      WRITE(LOUT,'(1X)')
C      WRITE(LOUT,'(1X)')
      ENDIF
    1 CONTINUE

      IF(LOK) WRITE(LOUT,'(7X,''H301 ASSNDX  ** TEST SUCCESSFUL **'')')
      IF(.NOT.LOK) WRITE(LOUT,'(7X,''H301 ASSNDX  ** TEST FAILED **'')')

      WRITE(LOUT,'(1X)')

      IRC= ITEST('H301', LOK)

      CALL ASSNDX(1,Q,0,1,IDQ,IP,SUM,IW,IDW)
      CALL ASSNDX(1,Q,1,0,IDQ,IP,SUM,IW,IDW)

      CALL PAGEND('H301')
      END
