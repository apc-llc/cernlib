/*
 * $Id: setuid.c,v 1.1.1.1 1996/03/08 15:44:28 mclareni Exp $
 *
 * $Log: setuid.c,v $
 * Revision 1.1.1.1  1996/03/08 15:44:28  mclareni
 * Cspack
 *
 */
#include "cspack/pilot.h"
/*
 * Copyright (C) 1989 by Frederic Hemmer
 * All rights reserved
 */
 
#include <errno.h>
 
int
setuic( uic)
int uic;
{
 int rc;
 
 if ((rc = pcb$setuic(&uic)) < 0) {
  errno = EVMSERR;
  vaxc$errno = rc;
  return(-1);
 }
}
 
/* Removed because VAXCRTL defines it as a null function
int
setuid( uid)
int uid;
{
 int rc;
 
 if ((rc = pcb$setmbr(&uid)) < 0) {
  errno = EVMSERR;
  vaxc$errno = rc;
  return(-1);
 }
}
*/
 
int
seteuid( uid)
int uid;
{
 int rc;
 
 if ((rc = pcb$setmbr(&uid)) < 0) {
  errno = EVMSERR;
  vaxc$errno = rc;
  return(-1);
 }
}
 
int
setruid( uid)
int uid;
{
 int rc;
 
 if ((rc = pcb$setmbr(&uid)) < 0) {
  errno = EVMSERR;
  vaxc$errno = rc;
  return(-1);
 }
}
 
/* Removed because VAXCRTL defines it as a null function
int
setgid( gid)
int gid;
{
 int rc;
 
 if ((rc = pcb$setgrp(&gid)) < 0) {
  errno = EVMSERR;
  vaxc$errno = rc;
  return(-1);
 }
}
*/
 
int
setegid( gid)
int gid;
{
 int rc;
 
 if ((rc = pcb$setgrp(&gid)) < 0) {
  errno = EVMSERR;
  vaxc$errno = rc;
  return(-1);
 }
}
 
int
setrgid( gid)
int gid;
{
 int rc;
 
 if ((rc = pcb$setgrp(&gid)) < 0) {
  errno = EVMSERR;
  vaxc$errno = rc;
  return(-1);
 }
}
