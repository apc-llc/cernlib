*
* $Id: fatstruct.inc,v 1.1.1.1 1996/03/07 15:17:52 mclareni Exp $
*
* $Log: fatstruct.inc,v $
* Revision 1.1.1.1  1996/03/07 15:17:52  mclareni
* Fatmen
*
*
#ifndef CERNLIB_FATMEN_FATSTRUCT_INC
#define CERNLIB_FATMEN_FATSTRUCT_INC
*
*
* fatstruct.inc
*
#if defined(CERNLIB_F90)
*
*     Draft F90 derived data type for mapping to FATMEN bank
*
      TYPE FATBANK
*
*     Generic file description
*
         CHARACTER*256 FILE_NAME
         CHARACTER*8   HOST_NAME
         INTEGER       COPY_LEVEL
         INTEGER       MEDIA_TYPE
         INTEGER       LOCATION_CODE
*
*     Disk description
*
         CHARACTER*16  HOST_TYPE
         CHARACTER*12  HOST_SYSTEM
*
*     Tape description
*
         CHARACTER*8   VSN
         CHARACTER*8   VID
         INTEGER       VID_PREFIX
         INTEGER       DENSITY
         INTEGER       VOLUME_SEQUENCE_NUMBER
         INTEGER       FILE_SEQUENCE_NUMBER
*
*     File description
*
         INTEGER       START_RECORD
         INTEGER       END_RECORD
         INTEGER       START_BLOCK
         INTEGER       END_BLOCK
*
*     Physical description
*
         CHARACTER*4   RECORD_FORMAT
         INTEGER       RECORD_LENGTH
         INTEGER       BLOCK_LENGTH
         INTEGER       FILE_SIZE
         INTEGER       USE_COUNT
*
*     Logical description
*
         CHARACTER*4   LOGICAL_FORMAT
         CHARACTER*4   USER_FORMAT
*
*     Dates and times
*
         INTEGER       DATE_CREATED
         INTEGER       DATE_CATALOGUED
         INTEGER       DATE_ACCESSED
*
*     Creation information
*
         CHARACTER*8   CREATED_BY_OWNER
         CHARACTER*8   CREATED_BY_ACCOUNT
         CHARACTER*8   CREATED_ON_NODE
         CHARACTER*8   CREATED_BY_JOB
*
*     File protection
*
         INTEGER       FILE_PROTECTION_MASK
*
*     User words
*
         INTEGER       USER_WORDS(10)
*
*     System words
*
         INTEGER       SYSTEM_WORDS(10)
      END TYPE FATBANK
#endif
 

#endif
