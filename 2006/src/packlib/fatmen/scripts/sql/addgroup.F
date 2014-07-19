*
* $Id: addgroup.F,v 1.1.1.1 1996/03/07 15:17:51 mclareni Exp $
*
* $Log: addgroup.F,v $
* Revision 1.1.1.1  1996/03/07 15:17:51  mclareni
* Fatmen
*
*
#include "sys/CERNLIB_machine.h"
#include "_fatmen/pilot.h"
 
#if defined(CERNLIB_ORACLE)
INSERT INTO Fatmen VALUES ('CERN', 'THRONG')
/
 
REM ***   SPECIFIC TABLES FOR THRONG
REM ***   Note: 1) CHAR(240) => Oracle V5.1
REM ***         2) TMS later substitutes Volumes_
 
 
CREATE TABLE GNames_THRONG ( GName CHAR(240) NOT NULL,
                             GN# NUMBER NOT NULL)
/
CREATE TABLE Files_THRONG ( File# NUMBER NOT NULL,
                            GN# NUMBER NOT NULL,
                            Copylevel NUMBER(2) NOT NULL,
                            Location NUMBER NOT NULL,
                            Hostname CHAR(8) NOT NULL,
                            Fullname CHAR(240) NOT NULL,
                            Hosttype CHAR(16),
                            Opersys CHAR(12),
                            Fileformat CHAR(4) NOT NULL,
                            Userformat CHAR(4),
                            Startrec# NUMBER,
                            Endrec# NUMBER,
                            Startblk# NUMBER,
                            Endblk# NUMBER,
                            Recformat CHAR(4),
                            Reclength NUMBER,
                            Blklength NUMBER,
                            Creation DATE,
                            Catalogation DATE,
                            Lastaccess DATE,
                            Active CHAR(1) NOT NULL,
                            Creatorname CHAR(8),
                            Creatoraccount CHAR(8),
                            Creatornode CHAR(8),
                            Creatorjob CHAR(8),
                            Protection NUMBER(2),
                            Userword0 NUMBER,
                            Userword1 NUMBER,
                            Userword2 NUMBER,
                            Userword3 NUMBER,
                            Userword4 NUMBER,
                            Userword5 NUMBER,
                            Userword6 NUMBER,
                            Userword7 NUMBER,
                            Userword8 NUMBER,
                            Userword9 NUMBER,
                            Sysword0 NUMBER,
                            Sysword1 NUMBER,
                            Sysword2 NUMBER,
                            Sysword3 NUMBER,
                            Sysword4 NUMBER,
                            Sysword5 NUMBER,
                            Sysword6 NUMBER,
                            Sysword7 NUMBER,
                            Sysword8 NUMBER,
                            Sysword9 NUMBER,
                            Comments CHAR(80),
                            Mediatype CHAR(1))
/
CREATE TABLE FXV_THRONG ( File# NUMBER NOT NULL,
                          Fileseq# NUMBER NOT NULL,
                          Vol# NUMBER NOT NULL,
                          Volseq# NUMBER)
/
CREATE TABLE Volumes_THRONG ( Vol# NUMBER NOT NULL,
                              VSN CHAR(6) NOT NULL,
                              VID CHAR(6) NOT NULL,
                              VIDprefix NUMBER,
                              Density NUMBER)
/
 
REM ***  ALLOWING ACCESS TO EVERYBODY
REM ***  Note: Only Fatmen table was protected
REM ***        against modifications
 
 
GRANT SELECT ON Gnames_THRONG TO PUBLIC
/
GRANT SELECT ON Files_THRONG TO PUBLIC
/
GRANT SELECT ON FXV_THRONG TO PUBLIC
/
GRANT SELECT ON Volumes_THRONG TO PUBLIC
/
 
#endif
#if defined(CERNLIB_SQLDS)
INSERT INTO Fatmen VALUES ('CERN', 'throng')
 
CREATE TABLE GNames_throng ( GName CHAR(240) NOT NULL,     -
                             GN# INTEGER NOT NULL)
CREATE TABLE Files_throng ( File# INTEGER NOT NULL,        -
                            GN# INTEGER NOT NULL,          -
                            Copylevel INTEGER NOT NULL,    -
                            Location INTEGER NOT NULL,     -
                            Hostname CHAR(8) NOT NULL,     -
                            Fullname CHAR(240) NOT NULL,   -
                            Hosttype CHAR(16),             -
                            Opersys CHAR(12),              -
                            Fileformat CHAR(4) NOT NULL,   -
                            Userformat CHAR(4),            -
                            Startrec# INTEGER,             -
                            Endrec# INTEGER,               -
                            Startblk# INTEGER,             -
                            Endblk# INTEGER,               -
                            Recformat CHAR(4),             -
                            Reclength INTEGER,             -
                            Blklength INTEGER,             -
                            Creation DATE,                 -
                            Catalogation DATE,             -
                            Lastaccess DATE,               -
                            Active CHAR(1) NOT NULL        -
                            Creatorname CHAR(8),           -
                            Creatoraccount CHAR(8),        -
                            Creatornode CHAR(8),           -
                            Creatorjob CHAR(8),            -
                            Protection INTEGER(2))
\end{XMP}
\end{minipage}\hfill
\begin{minipage}[t]{.494\linewidth}
\tiny\begin{XMP}
Alter table files_throng add (Userword0 INTEGER,           -
                -
                              Userword1 INTEGER,           -
                              Userword2 INTEGER,           -
                              Userword3 INTEGER,           -
                              Userword4 INTEGER,           -
                              Userword5 INTEGER,           -
                              Userword6 INTEGER,           -
                              Userword7 INTEGER,           -
                              Userword8 INTEGER,           -
                              Userword9 INTEGER,           -
                              Sysword0 INTEGER,            -
                              Sysword1 INTEGER,            -
                              Sysword2 INTEGER,            -
                              Sysword3 INTEGER,            -
                              Sysword4 INTEGER,            -
                              Sysword5 INTEGER,            -
                              Sysword6 INTEGER,            -
                              Sysword7 INTEGER,            -
                              Sysword8 INTEGER,            -
                              Sysword9 INTEGER,            -
                              Comments CHAR(80),           -
                              Mediatype CHAR(1))
CREATE TABLE FXV_throng ( File# INTEGER NOT NULL,          -
                          Fileseq# INTEGER NOT NULL,       -
                          Vol# INTEGER NOT NULL,           -
                          Volseq# INTEGER)
CREATE TABLE Volumes_throng ( Vol# INTEGER NOT NULL,       -
                              VSN CHAR(6) NOT NULL,        -
                              VID CHAR(6) NOT NULL,        -
                              VIDprefix INTEGER,           -
                              Density INTEGER)
 
 
#endif
