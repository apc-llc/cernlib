*
* $Id: sideli.F,v 1.2 1996/04/02 22:17:36 thakulin Exp $
*
* $Log: sideli.F,v $
* Revision 1.2  1996/04/02 22:17:36  thakulin
* Support for EPC Fortran:  remove char-int equivalences and use F90
* transfer facility.
*
* Revision 1.1.1.1  1995/12/12 14:36:16  mclareni
* Imported sources
*
*
#include "sigma/pilot.h"
*CMZ :  1.09/04 26/10/93  09.49.18  by  Carlo E. Vandoni
*-- Author :
      SUBROUTINE SIDELI(CN)
C
C    ....................................................
C
C
C         PURPOSE
C            IDENTIFY DELIMITER AND OUTPUT IT
C
C         USAGE
C            CALL DELIM(CN)  IN SIINPS
C
C         DESCRIPTION OF PARAMETERS
C            CN  DELIMITER
C
C
C         SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED
C   ITILDE/
C
C...  PAW VERSION  ...  MAY 1988
C
C    ......................................................
C
#include "sigma/sigkq.inc"
#include "sigma/sigc.inc"
#include "sigma/pawc.inc"
      LOGICAL SICOMP,IND
#if !defined(CERNLIB_CRAY)
      CHARACTER CN4*4,IVAL(16)*1,CN*1,N*1
#endif
#if defined(CERNLIB_CRAY)
      CHARACTER CN4*8,IVAL(16)*1,CN*1,N*1
#endif
      EQUIVALENCE (N,CN4)
#if !defined(CERNLIB_F90) && !defined(CERNLIB_QFEPC)
      EQUIVALENCE (N,MEQUI)
#endif
C     ALLOWED DELIM VALUES  +  -  /  (  )  =  ,  !  '  _  @  ^  :  "
      SAVE IVAL
      DATA IVAL( 1)/'+'/
      DATA IVAL( 2)/'-'/
      DATA IVAL( 3)/'/'/
      DATA IVAL( 4)/'('/
      DATA IVAL( 5)/')'/
      DATA IVAL( 6)/'='/
      DATA IVAL( 7)/','/
      DATA IVAL( 8)/'!'/
      DATA IVAL( 9)/'#'/
      DATA IVAL(10)/'%'/
      DATA IVAL(11)/'&'/
      DATA IVAL(12)/'"'/
      DATA IVAL(13)/':'/
      DATA IVAL(14)/''''/
      DATA IVAL(15)/'['/
      DATA IVAL(16)/']'/
*
      CALL SITRAX(' SIDELI   ')
#if !defined(CERNLIB_CRAY)
      CN4='    '
#endif
#if defined(CERNLIB_CRAY)
      CN4='        '
#endif
      L=16
      IND = .FALSE.
 3    CONTINUE
      IF(SICOMP(KQBLAN)) GO TO 3
C     LOOP IF BLANKS
C
C     DOUBLE OPERATOR SIGN BUSSINESS BEGINS HERE.
C     ** IS THE POWER.  ASTERISK HAS VALUE 39.
      IF(SICOMP(KQSTAR)) GO TO 101
C     BRANCH IF ASTERISK
C
      DO 1 I=1,L
      IF(SICOMP(IVAL(I))) GO TO 2
    1 CONTINUE
      RETURN
   2  CN=IVAL(I)
#if !defined(CERNLIB_CRAY)
      CN4=CN//'    '
#endif
#if defined(CERNLIB_CRAY)
      CN4=CN//'        '
#endif

      CN4=CN//'   '
100     CONTINUE
       CALL SIOUT (4)
#if defined(CERNLIB_F90) || defined(CERNLIB_QFEPC)
      CALL SIOUT (transfer(N,MEQUI))
#else
      CALL SIOUT (MEQUI)
#endif
      IND=.TRUE.
      RETURN
C
 101  CONTINUE
C---  THE RETRIEVING OF ** HAS TO BE MODIFIED ACCORDINGLY
      N='^'
      IF(SICOMP(KQSTAR)) GO TO 100
C     BRANCH IF **
      N='*'
      GO TO 100
C
      END
