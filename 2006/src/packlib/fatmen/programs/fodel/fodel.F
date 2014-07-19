*
* $Id: fodel.F,v 1.1.1.1 1996/03/07 15:17:48 mclareni Exp $
*
* $Log: fodel.F,v $
* Revision 1.1.1.1  1996/03/07 15:17:48  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
 
#if !defined(CERNLIB_IBMRT)
      SUBROUTINE FODEL(IRETC)
#endif
#if defined(CERNLIB_IBMRT)
      SUBROUTINE FODEL_(IRETC)
#endif
      INTEGER IRETC
 
*   Action  :  Excludes from the FATMEN file database the information
*              received in the first tuple of common block FOFILE, if
*              file information exists and user account provided is
*              the same as of the creator.
*              File information is only logically deleted (ACTIVE is
*              set to "N") in FILES_ table, but all related rows in
*              FXV_ and VOLUMES_ tables are removed (from VOLUMES_
*              only if last file on that volume)
*
*   Returns : IRETC => return code (  0 => normal end. First tuple in
*                                          FOFILE removed from the
*                                          database
*                                  -9998=> SQL warning. See FOR006
*                                  -9997=> //database/experiment does
*                                          not exist
*                                  -9996=> generic name does not exist
*                                  -9995=> file entry does exist (check
*                                          file and volume fields)
*                                  -9994=> user account not authorized
*                                          to remove the file entry
*                                  -6079 to
*                                  -0001=> SQL errors, IRETC=SQLCDE    )
*
*   Note:  Although the multi-volume case is not yet considered, the
*          DELETE operation (including removal of the volumes) can be
*          issued by a sequence of calls to this routine, using same
*          values each time and only changing the VID and VSN values
*
*   Responsible : Luigino Palermo (PALERMO@VXCERN,CERNVM)
*
*   Changes history:
*
*      DATE                    MODIFICATIONS
*-----------------------------------------------------------------------
*
*   15/09/89        Correction of bug when selecting VOL# in FXV_ table
*                   before deleting tape from VOLUMES_ table (problem
*                   only for multi-file volume). Now uses COUNT function
*   26/09/89        Recodification, using dynamic-defined statements.
*                   Method 3 (PREPARE, DECLARE, OPEN & FETCH) for SELECT
*                   method 1 (EXECUTE IMMEDIATE) for LOCK and method 2
*                   otherwise.
 
#if defined(CERNLIB_IBMRT)
#include "fatmen/fofilex.inc"
#endif
#if !defined(CERNLIB_IBMRT)
#include "fatmen/fofile.inc"
#endif
#include "fatmen/fovars.inc"
 
*...General variables
 
      INTEGER I1, I2, I3, LEXP
      EXTERNAL CLTOU
      CHARACTER*6 CMD
*
*...Variables for Oracle/SQL...........................................
*
      EXEC SQL BEGIN DECLARE SECTION
 
      CHARACTER*8 DBASE, EXPER
      CHARACTER*100 STMT1, STMT2, STMT5, STMT6, STMT7, STMT8
      CHARACTER*200 STMT3
      CHARACTER*400 STMT4
#if defined(CERNLIB_SQLDS)
      INTEGER*2      LSTMT4
      CHARACTER*200  STMT04
      COMMON/STMT02/ LSTMT4,STMT4
      EQUIVALENCE   (LSTMT4,STMT04)
#endif
      INTEGER LINES
 
*...Auxiliary host variables to hold internal identifiers of rows
 
      INTEGER GNID, FILEID, VOLID
 
*...Auxiliary host vars for GNAMES_ table
 
      CHARACTER*240 GENAM2
 
*...Auxiliary host vars for FILES_ table
 
      INTEGER LOCA2, FLSQN2, VLSQN2
      CHARACTER HNAM2*8, FNAM2*240, CREAC2*8, MEDI2*1
 
*...Auxiliary host vars for VOLUMES_ table
 
      INTEGER PRE2, DEN2
      CHARACTER VI2*6, VS2*6
 
*...Indicator variables
 
      INTEGER*2 CREACI
 
      EXEC SQL END DECLARE SECTION
*......................................................................
 
 
*
*...SQL common area
*
      EXEC SQL INCLUDE SQLCA.FOR
 
*...  Converts lower to upper case in the generic name
 
      GENAM2 = GENAME(1)
      CALL CLTOU (GENAM2)
 
*...  I1, I2, and I3 mark the limits of first 2 parts of generic name
 
      I1 = INDEX (GENAM2, '//')
      I2 = INDEX (GENAM2(I1+2:), '/')
      I3 = INDEX (GENAM2(I1+I2+2:), '/')
      IF ((I1.LE.0).OR.(I2.LE.1).OR.(I3.LE.1)) GOTO 1300
 
*...  Parse generic name and exclude //database/experiment/ prologue
*     (in the tables of an experiment do not contain the database and
*     experiment names)
 
      DBASE = GENAM2(I1+2:I1+I2)
      EXPER = GENAM2(I1+I2+2:I1+I2+I3)
      LEXP  = INDEX (EXPER, ' ') - 1
      GENAM2 = GENAM2(I1+I2+I3+2:)
 
      EXEC SQL WHENEVER SQLWARNING GOTO 1100
      EXEC SQL WHENEVER SQLERROR   GOTO 1200
 
*...Locks FATMEN table to avoid parallel writing while reading it
 
      EXEC SQL LOCK TABLE FATMEN IN SHARE MODE
 
*...Looks for database and experiment in FATMEN table (if doesn't exist,
*...returns error -9997)
 
      EXEC SQL WHENEVER NOT FOUND GOTO 1300
 
      EXEC SQL SELECT * INTO :DBASE, :EXPER  FROM FATMEN
     2         WHERE DATABASE =:DBASE AND EXPERIMENT =:EXPER
 
*...Locks all tables to be modified for exclusive access
 
      STMT1 = 'LOCK TABLE GNAMES_' // EXPER(1:LEXP) // ', FILES_' //
     1         EXPER(1:LEXP) // ', FXV_' // EXPER(1:LEXP) //
     2        ',VOLUMES_' // EXPER(1:LEXP) // ' IN EXCLUSIVE MODE'
 
      CMD = 'STMT1'
      EXEC SQL EXECUTE IMMEDIATE :STMT1
 
*...Looks for generic name in GNAMES_ table (error if it does not exist)
 
      EXEC SQL WHENEVER NOT FOUND GOTO 1400
 
      STMT2 = 'SELECT GN# FROM GNAMES_' // EXPER(1:LEXP) //
     1        ' WHERE GNAME = :GENAM2'
 
      CMD = 'STMT2'
      EXEC SQL PREPARE S2 FROM :STMT2
      EXEC SQL DECLARE C2 CURSOR FOR S2
      EXEC SQL OPEN C2 USING :GENAM2
      EXEC SQL FETCH C2 INTO :GNID
 
   30 CONTINUE
 
*...NB: Stores every array bind variable to be used in SQL statement in
*...    a temporary variable with same name less last char, plus "2"
*...    (PCC precompiler DOES NOT accept arrays elements !)
 
      MEDI2 = MEDIA(1)
      CALL CLTOU (MEDI2)
 
      EXEC SQL WHENEVER NOT FOUND GOTO 1500
 
      IF (MEDI2.EQ.'D') THEN
 
*...     Assigns values to temporary variables to be used in SELECT
*        of FILES_ table (disk files)
 
            HNAM2 = HNAME(1)
            CALL CLTOU (HNAM2)
 
            FNAM2 = FNAME(1)
            CALL CLTOU (FNAM2)
 
 
            STMT3 = 'SELECT FILE#, CREATORACCOUNT FROM FILES_' //
     1              EXPER(1:LEXP) // ' WHERE HOSTNAME = :HNAM2' //
     2             ' AND FULLNAME = :FNAM2 AND MEDIATYPE = :MEDI2' //
     3             ' AND ACTIVE = ''Y'' AND GN# = :GNID'
 
            CMD = 'STMT3'
            EXEC SQL PREPARE S3 FROM :STMT3
            EXEC SQL DECLARE C3 CURSOR FOR S3
            EXEC SQL OPEN C3 USING :HNAM2, :FNAM2, :MEDI2, :GNID
            EXEC SQL FETCH C3 INTO :FILEID, :CREAC2:CREACI
 
         ELSE
 
*...        Assigns values to temporary variables to be used in SELECT
*           (tape files, columns to identify it uniquely come from
*           FILES_, FXV_ and VOLUMES_ tables)
 
            LOCA2  = LOCAT(1)
 
            VI2 = VID(1)
            CALL CLTOU (VI2)
 
            VS2 = VSN(1)
            CALL CLTOU (VS2)
 
            FLSQN2 = FLSQNO(1)
 
            STMT4 = 'SELECT FILES_' // EXPER(1:LEXP) //
     1              '.FILE#, VOLUMES_' // EXPER(1:LEXP) //
     2              '.VOL#, CREATORACCOUNT FROM VOLUMES_' //
     3              EXPER(1:LEXP) // ', FXV_'// EXPER(1:LEXP) //
     4              ', FILES_' // EXPER(1:LEXP) // ' WHERE FILES_' //
     5              EXPER(1:LEXP) //'.FILE# = FXV_' // EXPER(1:LEXP) //
     6              '.FILE# AND FXV_' // EXPER(1:LEXP) //
     7              '.VOL# = VOLUMES_' // EXPER(1:LEXP) //
     8              '.VOL# AND VID = :VI2 AND VSN = :VS2' //
     9              ' AND FILESEQ# = :FLSQN2 AND LOCATION = :LOCA2' //
     A              ' AND MEDIATYPE = :MEDI2 AND ACTIVE = ''Y''' //
     B              ' AND GN# = :GNID'
 
            CMD = 'STMT4'
#if defined(CERNLIB_ORACLE)
            EXEC SQL PREPARE S4 FROM :STMT4
#endif
#if defined(CERNLIB_SQLDS)
            EXEC SQL PREPARE S4 FROM :STMT04
#endif
            EXEC SQL DECLARE C4 CURSOR FOR S4
            EXEC SQL OPEN C4 USING :VI2, :VS2, :FLSQN2, :LOCA2,
     1                             :MEDI2, :GNID
            EXEC SQL FETCH C4 INTO :FILEID, :VOLID, :CREAC2:CREACI
 
         ENDIF
 
*...     Checks if the creator account is the same as the provided in
*        the common area (if NULL, goes on deletion)
 
         IF ((CREAC2.NE.CREACC(1)).AND.(CREACI.NE.-1)) GOTO 1600
 
*...     Marks the file entry as a "deleted" one, but keeps row with
*        ACTIVE = No
 
         STMT6 = 'UPDATE FILES_' // EXPER(1:LEXP) //
     1           ' SET ACTIVE = ''N'' WHERE FILE# = :FILEID'
 
         CMD = 'STMT6'
         EXEC SQL PREPARE S6 FROM :STMT6
         EXEC SQL EXECUTE S6 USING :FILEID
 
*...     Verifies if file was the only on that tape volume, and if
*        positive, deletes respective rows in VOLUMES_, besides FXV_
 
         IF (MEDI2.NE.'D') THEN
 
             STMT7 = 'DELETE FROM FXV_' // EXPER(1:LEXP) //
     1               ' WHERE FILESEQ# = :FLSQN2' //
     2               ' AND VOL# = :VOLID AND FILE# = :FILEID'
 
             CMD = 'STMT7'
             EXEC SQL PREPARE S7 FROM :STMT7
             EXEC SQL EXECUTE S7 USING :FLSQN2, :VOLID, :FILEID
 
             EXEC SQL WHENEVER NOT FOUND CONTINUE
 
             LINES = 0
 
             STMT5 = 'SELECT COUNT(VOL#) FROM FXV_' // EXPER(1:LEXP)
     1               //  ' WHERE VOL# = :VOLID'
 
             CMD = 'STMT5'
             EXEC SQL PREPARE S5 FROM :STMT5
             EXEC SQL DECLARE C5 CURSOR FOR S5
             EXEC SQL OPEN C5 USING :VOLID
             EXEC SQL FETCH C5 INTO :LINES
 
             IF (LINES.EQ.0) THEN
 
                STMT8 = 'DELETE FROM VOLUMES_' // EXPER(1:LEXP) //
     1                  ' WHERE VOL# = :VOLID'
 
                CMD = 'STMT8'
                EXEC SQL PREPARE S8 FROM :STMT8
                EXEC SQL EXECUTE S8 USING :VOLID
 
             ENDIF
         ENDIF
 
      EXEC SQL COMMIT WORK
 
      RETURN
 
 1100 CONTINUE
      WRITE (6,1110) CMD, SQLWN1, SQLWN2, SQLWN3, SQLWN4, SQLWN5,
     2               SQLWN6, SQLWN7
      IRETC = -9998
      EXEC SQL WHENEVER SQLWARNING CONTINUE
      EXEC SQL ROLLBACK WORK
      RETURN
 
 1200 CONTINUE
      WRITE (6,1210) CMD, SQLCDE, (SQLEMC(J), J=1,70)
      IRETC = SQLCDE
      EXEC SQL WHENEVER SQLERROR CONTINUE
      EXEC SQL ROLLBACK WORK
      RETURN
 
 1300 CONTINUE
      WRITE (6,1310) DBASE,EXPER
      IRETC = -9997
      EXEC SQL ROLLBACK WORK
      RETURN
 
 1400 CONTINUE
      WRITE (6,1410)
      IRETC = -9996
      EXEC SQL ROLLBACK WORK
      RETURN
 
 1500 CONTINUE
      WRITE (6,1510)
      IRETC = -9995
      EXEC SQL ROLLBACK WORK
      RETURN
 
 1600 CONTINUE
      WRITE (6,1610)
      IRETC = -9994
      EXEC SQL ROLLBACK WORK
      RETURN
 
 1110 FORMAT (/, ' SQL WARNING ON ', A6, '. SQLWN_ INDICATORS: ',
     1        7('"',A1,'"',2X))
 1210 FORMAT (/, ' SQL ERROR ON ', A6, ': ', I5, ' ', 70A1)
 1310 FORMAT (/, ' ERROR: UNKNOWN DATABASE AND EXPERIMENT,A,1X,A')
 1410 FORMAT (/, ' ERROR: GENERIC NAME DOES NOT EXIST')
 1510 FORMAT (/, ' ERROR: DISK OR TAPE FILE ENTRY DOES NOT EXIST')
 1610 FORMAT (/, ' ERROR: PROVIDED ACCOUNT DIFFERS FROM CREATOR''S ONE')
      END
 
