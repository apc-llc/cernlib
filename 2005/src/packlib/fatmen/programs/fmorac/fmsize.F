*
* $Id: fmsize.F,v 1.1.1.1 1996/03/07 15:17:47 mclareni Exp $
*
* $Log: fmsize.F,v $
* Revision 1.1.1.1  1996/03/07 15:17:47  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMSIZE(EXPER, NTOTAL, NACTIVE, NDELETE)
*
*
* Action   : Get the number of files per experiment and print the
*            total number active and number deleted.
*
*
* Receives : EXPER  =======> Experiment name.
*
*
* Returns  : NTOTAL   =====> Total number of files per experiment.
*            NACTIVE  =====> Total number of active files.
*            NDELETE  =====> Total number of deleted files.
*
*
*...General variables
*
      CHARACTER*20 EXPER
*
* ...Variables for Oracle.
*
      EXEC SQL BEGIN DECLARE SECTION
      CHARACTER*100 STMT1
      CHARACTER*200 STMT2
      CHARACTER*200 STMT3
      INTEGER       NTOTAL
      INTEGER       NACTIVE
      INTEGER       NDELETE
 
      EXEC SQL END DECLARE SECTION
*
* ... Sql common area used.
      EXEC SQL INCLUDE SQLCA
* ... If exist problems with sqlca
      EXEC SQL WHENEVER SQLERROR GOTO 300
*
* ... SQL dynamic sentences (Method 3).
*
      STMT1 = 'SELECT COUNT(GN#) FROM FILES_' // EXPER
      STMT2 = STMT1 // ' WHERE (ACTIVE = ''Y'')'
      STMT3 = STMT1 // ' WHERE (ACTIVE = ''N'')'
*
* ... For obtain NTOTAL
      EXEC SQL PREPARE S1 FROM :STMT1
      EXEC SQL DECLARE C1 CURSOR FOR S1
      EXEC SQL OPEN C1
      EXEC SQL FETCH C1 INTO :NTOTAL
      EXEC SQL WHENEVER SQLERROR GOTO 300
*
* ... For obtain NACTIVE
      EXEC SQL PREPARE S2 FROM :STMT2
      EXEC SQL DECLARE C2 CURSOR FOR S2
      EXEC SQL OPEN C2
      EXEC SQL FETCH C2 INTO :NACTIVE
      EXEC SQL WHENEVER SQLERROR GOTO 300
*
* ... For obtain NDELETE
      EXEC SQL PREPARE S3 FROM :STMT3
      EXEC SQL DECLARE C3 CURSOR FOR S3
      EXEC SQL OPEN C3
      EXEC SQL FETCH C3 INTO :NDELETE
      EXEC SQL WHENEVER SQLERROR GOTO 300
*
* ... Results.
*     IRC = IOSCLR()
      WRITE(6, *)
      WRITE(6, 50) EXPER
50    FORMAT (/,'           EXPERIMENT  : ', A20)
      WRITE(6,*)
      WRITE(6,*)
      WRITE(6, 100) NTOTAL
100   FORMAT (/, '     Total number of files         : ', I10)
      WRITE(6, *)
      WRITE(6,150) NACTIVE
150   FORMAT (/, '     Total number of active files  : ', I10)
      WRITE(6, *)
      WRITE(6,250) NDELETE
250   FORMAT (/, '     Total number of deleted files : ', I10)
      WRITE(6,*)
      WRITE(6,*)
      RETURN
*
* ... Causes contained into SQLEMC(70).
300   CONTINUE
      WRITE(6,350) (SQLEMC(I),I=1,70)
350   FORMAT(/, 1X,70A1,/)
      EXEC SQL WHENEVER SQLERROR CONTINUE
      EXEC SQL ROLLBACK WORK RELEASE
      RETURN
      END
      END
