*
* $Id: main.F,v 1.2 2004/07/29 13:53:57 mclareni Exp $
*
* $Log: main.F,v $
* Revision 1.2  2004/07/29 13:53:57  mclareni
* For 64-bit pointer systems put local variables referenced by LOCF
* in a dummy named common block to keep addresses in the program region.
* Changed added by cpp define CERNLIB_QMLXIA64 but it should work in all
* systems.
*
* Revision 1.1.1.1  1996/03/08 11:50:41  mclareni
* Ffread
*
*
#include "ffread/pilot.h"
#if defined(CERNLIB_CDC)
      PROGRAM FFTEST(INPUT,OUTPUT)
#endif
#if !defined(CERNLIB_CDC)
      PROGRAM FFTEST
#endif
#if defined(CERNLIB_QMLXIA64)
      COMMON /TEST64/X,Y,I,J,A,B,TEXT
#endif
 
      INTEGER I, J(10)
      REAL    X, Y(10)
      LOGICAL A, B(10)
      CHARACTER*4 TEXT
      EXTERNAL FUREAD, FUSTAR
C
C----------------- Beginning of executable statements -------------------------
C
      I = 0
      X = 0.
      A = .FALSE.
 
      DO 10 K = 1, 10
          J(K) = 0
          Y(K) = 0.
          B(K) = .FALSE.
  10  CONTINUE
 
      CALL FFINIT (0)
 
      CALL FFSET ('SIZE', 6)
 
      CALL FFRSET( 10, FUREAD)
 
      CALL FFKEY ('INTVAR',   I,  1, 'INTEGER')
      CALL FFKEY ('INTARRAY', J, 10, 'INTEGER')
 
      CALL FFKEY ('REALVAR',   X,  1, 'REAL')
      CALL FFKEY ('REALARRAY', Y, 10, 'REAL')
 
      CALL FFKEY ('LOGVAR',   A,  1, 'LOGICAL')
      CALL FFKEY ('LOGARRAY', B, 10, 'LOGICAL')
 
      CALL FFGO
 
      WRITE (*, *)
 
      WRITE (*, *) ' INTEGER VARIABLE:', I
      WRITE (*, *) ' INTEGER ARRAY:   ', (J(K), K = 1, 5)
      WRITE (*, *) '                  ', (J(K), K = 6,10)
      WRITE (*, *)
 
      WRITE (*, *) ' LOGICAL VARIABLE:', A
      WRITE (*, *) ' LOGICAL ARRAY:   ', (B(K), K = 1, 5)
      WRITE (*, *) '                  ', (B(K), K = 6,10)
      WRITE (*, *)
 
      CALL UHTOC (X, 4, TEXT, 4)
      WRITE (*, *) ' TEXT (REALVAR):  ''', TEXT, ''''
      WRITE (*, *) ' REAL    ARRAY:   ', (Y(K), K = 1, 4)
      WRITE (*, *) '                  ', (Y(K), K = 5, 8)
      WRITE (*, *) '                  ', (Y(K), K = 9,10)
 
      END
