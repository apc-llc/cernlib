*
* $Id: test.F,v 1.2 1997/02/04 17:36:53 mclareni Exp $
*
* $Log: test.F,v $
* Revision 1.2  1997/02/04 17:36:53  mclareni
* Merge Winnt and 97a versions
*
* Revision 1.1.1.1.2.1  1997/01/21 11:32:13  mclareni
* All mods for Winnt 96a on winnt branch
*
* Revision 1.1.1.1  1996/02/15 17:48:36  mclareni
* Kernlib
*
*
#include "kernnumt/pilot.h"
          PROGRAM CHECK
#include "kernnumt/sysdat.inc"
          PARAMETER(LW  =  27 000)
          COMMON              W(LW)
          CHARACTER*6         ERCODE
          CHARACTER*8         CDATE, CTIME, CSYST
          LOGICAL             OK,  OKC
          DOUBLE PRECISION    D, D1, DR
          DATA                ER,         ME
#if defined(CERNLIB_NUME38)
     +                     /  1.70E38,    126  /
#endif
#if defined(CERNLIB_NUME75)
     +                     /  7.23E75,    251  /
#endif
#if defined(CERNLIB_NUME293)
     +                     /  3.19E293,   975  /
#endif
#if defined(CERNLIB_NUME2465)
     +                     /  2.72E2465, 8190  /
#endif
          DATA                DR,         MD
#if defined(CERNLIB_NUMD38)
     +                     /  1.70D38,    126  /
#endif
#if defined(CERNLIB_NUMD75)
     +                     /  7.23D75,    251  /
#endif
#if defined(CERNLIB_NUMD279)
     +                     /  1.13D279,   927  /
#endif
#if defined(CERNLIB_NUMD2465)
     +                     /  2.72D2465, 8190  /
#endif
          DATA                TL
#if defined(CERNLIB_NUMAP)||defined(CERNLIB_NUMCD)||defined(CERNLIB_NUMCR)||defined(CERNLIB_NUMCV)||defined(CERNLIB_NUMDE)||defined(CERNLIB_NUMIB)||defined(CERNLIB_NUMLN)||defined(CERNLIB_NUMMS)||defined(CERNLIB_NUMND)||defined(CERNLIB_NUMUC)||defined(CERNLIB_NUMNT)
     +                     /  0.005  /
#endif
          ERANGE     =  ER
          DRANGE     =  DR
          MXBINE     =  ME
          MXBIND     =  MD
          R   =  1.
  10      R   =  0.5*R
          CALL INCR1(R1,R)
          IF(R1 .NE. 1.)  GOTO 10
          RELPRT(1)  =  2.*R
          D   =  1.
  12      D   =  0.5*D
          D1  =  D + 1
	  CALL INCD1(D1,D)
          IF(D1 .NE. 1.D0)  GOTO 12
#if defined(CERNLIB_HPUX)
          CALL ALLOW_UNALIGNED_DATA_ACCESS
#endif
          RELPRT(2)  =  2.*D
          CALL STAMP(CDATE,CTIME,CSYST)
#if defined(CERNLIB_NEXT)
C-- The Absoft compiler allows using the 96-bit precision of
C   the floating point coprocessor. Therefore the computation
C   of RELPRT with the above method is not correct. While a
C   better method to compute the relative precision of REAL
C   and DOUBLE PRECISION stored data is found, we set it by hand.
C--
          RELPRT(1)=8.5E-8
          RELPRT(2)=2.5D-12
#endif
#if (defined(CERNLIB_NUMAP))&&(!defined(CERNLIB_NUMDPRNT))&&(!defined(CERNLIB_UNIX))
          OPEN(UNIT=3,FILE='-STDOUT')
*.
#endif
#if (defined(CERNLIB_NUMAP))&&(!defined(CERNLIB_NUMDPRNT))&&(defined(CERNLIB_UNIX))&&(!defined(CERNLIB_MSDOS))
          OPEN(UNIT=3,FILE='kernnumt.ft03',STATUS='UNKNOWN')
*.
#endif
#if (defined(CERNLIB_NUMAP))&&(!defined(CERNLIB_NUMDPRNT))&&(defined(CERNLIB_MSDOS))
          OPEN(UNIT=3,FILE='kernnumt.ft3',STATUS='UNKNOWN')
#endif
#if defined(CERNLIB_IBMRT)
          RELPRT(1)  = 0.953674316E-6
#endif
#if defined(CERNLIB_NUMIB2)
          CALL ERRSET(187,999,-1,1,1)
#endif
#if (defined(CERNLIB_NUMND))&&(!defined(CERNLIB_NUMDPRNT))
          OPEN(UNIT=3,FILE='KERNNUMT:FT03',STATUS='UNKNOWN')
#endif
#if defined(CERNLIB_NUMDPRNT)
          IOUNIT  =  6
#endif
#if !defined(CERNLIB_NUMDPRNT)
          IOUNIT  =  3
#endif
          LGFILE  =  0
#if defined(CERNLIB_NUMEPRNT)
          ERPRNT  =  .TRUE.
          KNTM    =  100
#endif
#if !defined(CERNLIB_NUMEPRNT)
          ERPRNT  =  .FALSE.
          KNTM    =  0
#endif
#if defined(CERNLIB_NUMESTOP)
          ERSTOP  =  .TRUE.
          KNTR    =  0
#endif
#if !defined(CERNLIB_NUMESTOP)
          ERSTOP  =  .FALSE.
          KNTR    =  100
#endif
          ERCODE  =  ' '
          CALL RANGET(SEEDIN)
          NP  =  0
          OK  =  .TRUE.
          DMY = 1000.
          CALL TIMEST( DMY )
          CALL TIMEX( CHKTIM(0) )
          KNTIME = 0
          DO 21     J  =  1, 4
#if !defined(CERNLIB_NUMCHK1)
             IF(J .EQ. 1)  GOTO 20
#endif
#if !defined(CERNLIB_NUMCHK2)
             IF(J .EQ. 2)  GOTO 20
#endif
#if !defined(CERNLIB_NUMCHK3)
             IF(J .EQ. 3)  GOTO 20
#endif
#if !defined(CERNLIB_NUMCHK4)
             IF(J .EQ. 4)  GOTO 20
#endif
             WRITE(*,1000) J, CSYST, CDATE, CTIME, RELPRT
             CALL KERSET(ERCODE,LGFILE,KNTM,KNTR)
             IF(ERPRNT) THEN
                WRITE(*,1001)
             ELSE
                WRITE(*,1002)
             ENDIF
             IF(ERSTOP) THEN
                WRITE(*,1003)
             ELSE
                WRITE(*,1004)
             ENDIF
             NPC  =  0
             OKC  =  .FALSE.
#if defined(CERNLIB_NUMCHK1)
             IF(J .EQ. 1)
     +          CALL CHECK1(LW,W,NPC,OKC,PKNAME(NP+1),PKTIME(NP+1))
#endif
#if defined(CERNLIB_NUMCHK2)
             IF(J .EQ. 2)
     +          CALL CHECK2(LW,W,NPC,OKC,PKNAME(NP+1),PKTIME(NP+1))
#endif
#if defined(CERNLIB_NUMCHK3)
             IF(J .EQ. 3)
     +          CALL CHECK3(LW,W,NPC,OKC,PKNAME(NP+1),PKTIME(NP+1))
#endif
#if defined(CERNLIB_NUMCHK4)
             IF(J .EQ. 4)
     +          CALL CHECK4(LW,W,NPC,OKC,PKNAME(NP+1),PKTIME(NP+1))
#endif
             IF(.NOT. OKC)  WRITE(*,1007) J
             IF(      OKC)  WRITE(*,1008) J
             NP  =  NP + NPC
             OK  =  OK .AND. OKC
  20         CALL TIMEX( CHKTIM(J) )
  21         CONTINUE
          IF(.NOT. OK)  WRITE(*,1011) NP
          IF(      OK)  WRITE(*,1012) NP
#if defined(CERNLIB_NUMTIME)
          T  =  CHKTIM(4) - CHKTIM(0)
          WRITE(*,1021) T
          DO 30  J = 1, 4
             T  =  CHKTIM(J) - CHKTIM(J-1)
             WRITE(*,1022) J, T
  30      CONTINUE
          WRITE(*,1023) TL
          DO 31  I = 1, NP
             IF(I .EQ. 1)  RT = PKTIME(1) - CHKTIM(0)
             IF(I .GT. 1)  RT = PKTIME(I) - PKTIME(I-1)
             IF(RT .GT. TL)  WRITE(*,1024) PKNAME(I), RT
  31         CONTINUE
          DO 32  K = 1, KNTIME
             WRITE(*,'(1X,A30)') SPTIME(K)
  32         CONTINUE
#endif
 
          STOP
1000      FORMAT(6H1CHECK,I2,11H OF KERNNUM,21X,A8,20X,A8,
     +              / 27H0VERSION 3.05   (17.3.1990)  ,41X,A8,
     +              / 26H0RELATIVE SINGLE PRECISION, 1P, E12.2,
     +              / 26H0RELATIVE DOUBLE PRECISION, 1P, E12.2)
1001      FORMAT(//20X,'LIBRARY PROGRAMS MAY PRINT ERROR MESSAGES')
1002      FORMAT(//20X,
     +      'LIBRARY PROGRAMS SHALL NOT PRINT ERROR MESSAGES')
1003      FORMAT(//20X,'LIBRARY PROGRAMS MAY ABEND')
1004      FORMAT(//20X,'LIBRARY PROGRAMS SHALL NOT ABEND')
1007      FORMAT(// 12H ????? CHECK,I2,
     +       29H OF KERNNUM HAS FAILED  ?????  )
1008      FORMAT(//  6H CHECK,I2, 22H OF KERNNUM SUCCESSFUL )
1011      FORMAT(// I10, ' PACKAGES CHECKED.'
     +     // 20X, 44H ?????  CHECK OF KERNNUM HAS FAILED.  ????? )
1012      FORMAT(// I10, ' PACKAGES CHECKED.',
     +           //8X,'VERIFY BY INSPECTION OF THIS OUTPUT WHETHER',
     +                ' ERROR AND ABEND REPORTS',
     +            /8X,'FROM LIBRARY PROGRAMS AGREE WITH THE',
     +                ' PREDICTIONS PRINTED BY THE'
     +            /8X,'TEST PROGRAM.    -   OTHERWISE...'
     +           // 20X, 'KERNNUM CHECK HAS BEEN SUCCESSFUL.')
1021    FORMAT(18H1TOTAL RUNTIME ..., F17.3, 9H  SECONDS)
1022    FORMAT(9H0   CHECK,I3,13H  RUNTIME ..., F10.3, 9H  SECONDS)
1023    FORMAT(34H0RUNTIMES OF PACKAGES WHICH EXCEED,F6.3,9H  SECONDS)
1024    FORMAT(8X, A4, 13H  RUNTIME ..., F10.3, 9H  SECONDS)
        END
	SUBROUTINE INCR1(R1,R)
	R1=R+1
	RETURN
	END
	SUBROUTINE INCD1(R1,R)
	DOUBLE PRECISION R1,R
	R1=R+1
	RETURN
	END
