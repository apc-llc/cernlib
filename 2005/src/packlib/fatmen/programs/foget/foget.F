*
* $Id: foget.F,v 1.1.1.1 1996/03/07 15:17:48 mclareni Exp $
*
* $Log: foget.F,v $
* Revision 1.1.1.1  1996/03/07 15:17:48  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
 
#if defined(CERNLIB_IBMRT)
      SUBROUTINE FOGET(NAME, NDONE, IRETC)
#endif
#if !defined(CERNLIB_IBMRT)
      SUBROUTINE FOGET_(NAME, NDONE, IRETC)
#endif
      CHARACTER*(*) NAME
      INTEGER NDONE, IRETC
 
*   Action  :  Looks if //Database/Experiment are OK. If yes, then
*              searches in the FATMEN file database on Oracle/SQL
*              for generic name (more than 1 if directory-like)
*              derived from NAME and fills common block FOFILE with
*              information found in tables GNAMES_, FILES_, FXV_
*              and VOLUMES_
*
*   Receives:  NAME => complete or partial (one * means any value in
*                      that position) generic name to be searched
*
*              NDONE => zero indicates is the first time routine is
*                       being called. Any other value indicates that is
*                       a continuation of previous call (ATTENTION !!)
*
*   Returns :  NDONE =>  number of entries filled in the common blocks
*                        for this call
*
*              IRETC =>  <0 : errors (-9999 => no information retrieved)
*                                    (-9998 => SQL warning. See FOR006 )
*                                    (-9997 => Database and Experiment
*                                              wrongly specified       )
*                                    (-0001 to
*                                     -6079 => SQL errors, IRETC=SQLCDE)
*                        =0 : no more information (normal end)
*                        =1 : to be continued (incomplete end)
*
*   Interface : common block /FOFILE/, containing 10-elements arrays
*               which will contain at the end (if no errors) NDONE
*               logical tuples made of concatenation of elements in
*               the same position of the arrays. Information about
*               each table is repeated when necessary (same generic
*               name/file attributes when multi-volume files, whose
*               are indicated by volume sequence number (VLSQNO)
*
*   Responsible : Luigino Palermo (PALERMO@VXCERN,CERNVM)
*
*   Changes history:
*
*      DATE                    MODIFICATIONS
*--------------------------------------------------------------------
*
*  28/06/89        Use of Oracle CURSOR instead of array fetch (due
*                  to unknown number of retrieved rows)
*  01/07/89        Generic name begining with //CERN/exp-name/...
*                  and case-insensitive
*  04/04/89        Call CLTOU instead of TRE0EU (lower to upper case)
*  07/07/89        Lock of tables in SHARE mode (no parallel updating
*                  allowed) before read them
*  13/07/89        Inclusion of creator account (CREACC) and 10 system
*                  words (SYSWDx); use of Patchy to manage code
*  26/09/89        Recodification, using dynamic-defined statements.
*                  Method 3 (PREPARE, DECLARE, OPEN & FETCH) for SELECT
*                  and method 1 (EXECUTE IMMEDIATE) for LOCK
*
 
*...  General variables
 
      INTEGER I1, I2, I3, J, K
      INTEGER NTOTAL, LEXP
      EXTERNAL  CLTOU
      CHARACTER*1100 SELECT
      CHARACTER*80 FROM, ORDER
      CHARACTER*6 CMD
#if !defined(CERNLIB_IBMRT)
#include "fatmen/fofile.inc"
#endif
#if defined(CERNLIB_IBMRT)
#include "fatmen/fofilex.inc"
#endif
 
*...  Variables for Oracle
 
      EXEC SQL BEGIN DECLARE SECTION
 
      CHARACTER*8 DBASE, EXPER
      CHARACTER*100 STMT1
      CHARACTER*240 GFN
      CHARACTER*200 WHERE
      CHARACTER*2000 STMT2
#if defined(CERNLIB_SQLDS)
      INTEGER*2      LSTMT2
      CHARACTER*200  STMT02
      COMMON/STMT02/ LSTMT2,STMT2
      EQUIVALENCE   (LSTMT2,STMT02)
#endif
#include "fatmen/fovars.inc"
 
      EXEC SQL END DECLARE SECTION
 
*...  SQL common area
 
      EXEC SQL INCLUDE SQLCA.FOR
 
      EXEC SQL WHENEVER SQLERROR   GOTO 1200
 
*...  If NDONE is zero, this is the first call. Otherwise, continuation
*     of previous call
 
      IF (NDONE) 90, 10, 90
 
10    CONTINUE
 
      NTOTAL = 0
 
*...  Translate to upper case
 
      CALL CLTOU (NAME)
 
*...  I1, I2 and I3 mark the limits of first 2 parts of generic name
 
      I1 = INDEX (NAME, '//')
      I2 = INDEX (NAME(I1+2:), '/')
      I3 = INDEX (NAME(I1+I2+2:), '/')
      IF ((I1.LE.0).OR.(I2.LE.1).OR.(I3.LE.1)) GOTO 1300
 
*...  Parse the generic NAME and search for *, preparing fetch
 
      DBASE = NAME (I1+2:I1+I2)
      EXPER = NAME (I1+I2+2:I1+I2+I3)
      LEXP = INDEX (EXPER,' ') - 1
      NAME = NAME (I1+I2+I3+2:)
      K = INDEX (NAME, '*')
      IF (K.NE.0) THEN
*...  '%' in SQL means any string of zero or more chars
        NAME(K:K) = '%'
      ENDIF
 
      GFN = NAME
 
      EXEC SQL WHENEVER NOT FOUND GOTO 1300
 
*...  Looks for Database and Experiment provided
 
      EXEC SQL SELECT *  INTO :DBASE, :EXPER  FROM FATMEN
     2         WHERE DATABASE=:DBASE AND EXPERIMENT=:EXPER
 
*...  Prepare Oracle cursor to fetch of tuples containing files
 
      SELECT = 'SELECT GNAME,NVL(COPYLEVEL,-1),NVL(LOCATION,-1),'//
     1  'NVL(HOSTNAME,'' ''),NVL(FULLNAME,'' ''),'//
     2  'NVL(HOSTTYPE,'' ''),NVL(OPERSYS,'' ''),NVL(FILEFORMAT,'' ''),'
     3  //'NVL(USERFORMAT,'' ''),NVL(STARTREC#,-1),NVL(ENDREC#,-1),'//
     4  'NVL(STARTBLK#,-1),NVL(ENDBLK#,-1),NVL(RECFORMAT,'' ''),'//
     5  'NVL(RECLENGTH,-1),NVL(BLKLENGTH,-1),NVL(ACTIVE,'' ''),'//
     6  'NVL(TO_CHAR(CREATION,''DD-MON-YY HH24:MM''),'' ''),'//
     7  'NVL(TO_CHAR(CATALOGATION,''DD-MON-YY HH24:MM''),'' ''),'//
     8  'NVL(TO_CHAR(LASTACCESS,''DD-MON-YY HH24:MM''),'' ''),'//
     9  'NVL(CREATORNAME,'' ''),NVL(CREATORACCOUNT,'' ''),'//
     A  'NVL(CREATORNODE,'' ''),NVL(CREATORJOB,'' ''),'//
     B  'NVL(PROTECTION,-1),NVL(USERWORD0,-1),NVL(USERWORD1,-1),'//
     C  'NVL(USERWORD2,-1),NVL(USERWORD3,-1),NVL(USERWORD4,-1),'//
     D  'NVL(USERWORD5,-1),NVL(USERWORD6,-1),NVL(USERWORD7,-1),'//
     E  'NVL(USERWORD8,-1),NVL(USERWORD9,-1),NVL(SYSWORD0,-1),'//
     F  'NVL(SYSWORD1,-1),NVL(SYSWORD2,-1),NVL(SYSWORD3,-1),'//
     G  'NVL(SYSWORD4,-1),NVL(SYSWORD5,-1),NVL(SYSWORD6,-1),'//
     H  'NVL(SYSWORD7,-1),NVL(SYSWORD8,-1),NVL(SYSWORD9,-1),'//
     I  'NVL(COMMENTS, '' ''),NVL(MEDIATYPE,'' ''),NVL(FILESEQ#,-1),'//
     J  'NVL(VOLSEQ#,-1),NVL(VID,'' ''),NVL(VSN,'' ''),'//
     K  'NVL(VIDPREFIX,-1),NVL(DENSITY,-1) '
      FROM = ' FROM VOLUMES_' // EXPER(1:LEXP) // ', FXV_' //
     1       EXPER(1:LEXP)// ',FILES_' // EXPER(1:LEXP) //
     2       ', GNAMES_' // EXPER(1:LEXP)
      WHERE = ' WHERE GNAMES_' // EXPER(1:LEXP) // '.GN# = FILES_'
     1        // EXPER(1:LEXP) // '.GN# (+) AND FILES_' //
     2        EXPER(1:LEXP) // '.FILE# = FXV_' //
     3        EXPER(1:LEXP) // '.FILE# (+) AND FXV_' //
     4        EXPER(1:LEXP) // '.VOL# = VOLUMES_' //
     5        EXPER(1:LEXP) // '.VOL# (+) AND ' //
     6        'ACTIVE = ''Y'' AND GNAME LIKE :GFN'
      ORDER = ' ORDER BY GNAMES_' // EXPER(1:LEXP) //
     1        '.GN#, FILES_' // EXPER(1:LEXP) // '.FILE#, VOLSEQ#'
 
      STMT2 = SELECT//FROM//WHERE//ORDER
#if defined(CERNLIB_ORACLE)
      EXEC SQL PREPARE S1 FROM :STMT2
#endif
#if defined(CERNLIB_SQLDS)
*     Warning: must use this trick for SQL/DS pre-compiler
*     LONG VARCHAR variables are not allowed, and we don't have
*     structures in FORTRAN so...
*     Change reference 452 to 448 before compiling
      EXEC SQL PREPARE S1 FROM :STMT02
#endif
      EXEC SQL DECLARE C1 CURSOR FOR S1
 
*...  Lock all tables of that experiment for read access
 
      STMT1 = 'LOCK TABLE GNAMES_' // EXPER(1:LEXP) //
     1        ',FILES_' // EXPER(1:LEXP) // ',FXV_' //
     2        EXPER(1:LEXP) // ',VOLUMES_' // EXPER(1:LEXP)
     3        // ' IN SHARE MODE'
 
      CMD = 'STMT1'
      EXEC SQL EXECUTE IMMEDIATE :STMT1
 
      EXEC SQL OPEN C1 USING :GFN
 
90    CONTINUE
 
      EXEC SQL WHENEVER NOT FOUND GOTO 1000
      EXEC SQL WHENEVER SQLWARNING GOTO 1100
 
      CMD = 'STMT2'
      EXEC SQL FETCH C1 INTO
     1    :GENAME, :CPLVL,  :LOCAT,  :HNAME,  :FNAME,  :HTYPE,
     2    :OPSYS,  :FFORMT, :USRFMT, :SRTREC, :ENDREC, :SRTBLK,
     3    :ENDBLK, :RECFMT, :RECLGH, :BLKLGH, :ACTIVE,
     4    :CREDAT, :CATDAT, :ACSDAT,
     5    :CRENAM, :CREACC, :CRENOD, :CREJOB, :PROTEC,
     6    :USRWD0, :USRWD1, :USRWD2, :USRWD3, :USRWD4, :USRWD5,
     7    :USRWD6, :USRWD7, :USRWD8, :USRWD9,
     6    :SYSWD0, :SYSWD1, :SYSWD2, :SYSWD3, :SYSWD4, :SYSWD5,
     6    :SYSWD6, :SYSWD7, :SYSWD8, :SYSWD9,
     8    :COMMTS, :MEDIA,  :FLSQNO, :VLSQNO,
     9    :VID,    :VSN,    :PREF,   :DENS
 
*...   NDONE holds the number of tuples for this fetch, NTOTAL the total
*      of tuples up to last fetch
 
       NDONE = SQLERD(3) - NTOTAL
       NTOTAL = SQLERD(3)
       IRETC = 1
       RETURN
 
1000  CONTINUE
      NDONE = SQLERD(3) - NTOTAL
      NTOTAL = SQLERD(3)
      IF ((NDONE.EQ.0).AND.(NTOTAL.EQ.0)) THEN
*...no information could be retrieved (wrong generic name?)
         IRETC = -9999
      ELSE
         IRETC = 0
      ENDIF
 
      EXEC SQL CLOSE C1
      EXEC SQL COMMIT WORK
      RETURN
 
1100  CONTINUE
      WRITE (6,1110) CMD, SQLWN1, SQLWN2, SQLWN3, SQLWN4, SQLWN5,
     2               SQLWN6, SQLWN7
      IRETC = -9998
      EXEC SQL ROLLBACK WORK
      RETURN
 
1200  CONTINUE
      WRITE (6,1210) CMD, SQLCDE, (SQLEMC(J), J=1,70)
      IRETC = SQLCDE
      EXEC SQL WHENEVER SQLERROR CONTINUE
      EXEC SQL ROLLBACK WORK
      RETURN
 
1300  CONTINUE
      WRITE (6,1310) DBASE,EXPER
      IRETC = -9997
      EXEC SQL ROLLBACK WORK
      RETURN
 
1110  FORMAT (/, ' SQL WARNING ON ', A6, '. SQLWN_ INDICATORS: ',
     1        7('"',A1,'"',2X))
1210  FORMAT (/, ' SQL ERROR ON ', A6, ': ', I5, ' ', 70A1)
1310  FORMAT (/, ' ERROR: UNKNOWN DATABASE AND EXPERIMENT',A,1X,A)
 
      END
