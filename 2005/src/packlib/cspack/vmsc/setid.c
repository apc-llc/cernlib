/*
 * $Id: setid.c,v 1.1.1.1 1996/03/08 15:44:28 mclareni Exp $
 *
 * $Log: setid.c,v $
 * Revision 1.1.1.1  1996/03/08 15:44:28  mclareni
 * Cspack
 *
 */
#include "cspack/pilot.h"
/*
 * Copyright (C) 1989,1990 by Frederic Hemmer           CERN-DD/CN
 * All rights reserved
 */
 
#ifndef lint
static char     *ident="%W% %G% Frederic Hemmer CERN-DD/CN";
#endif /* lint */
 
#ifdef DEBUG
#include <stdio.h>
#include "sysreq/pwd_vms.h"
#endif /* DEBUG */
#include <errno.h>
#include <ssdef.h>
 
int
setid(uic)      /* Set VMS identifiers as registered for this uic       */
unsigned int uic;
{
 
        int             status; /* system service return status         */
        struct {
  unsigned int    uic;
  unsigned int    reserved;
        } holder;               /* Holder (UIC) to find rights for      */
 struct  {
  unsigned int    id;
  unsigned int    attrib;
 } id;
        unsigned int    context; /* context long word                   */
 unsigned int    pidadr;  /* process id                          */
 unsigned int    prvatr;  /* previous attribute                  */
 
        context=0;              /* Initialize context                   */
 holder.uic = uic;
 holder.reserved = 0;
 
again:
 
 status = sys$find_held(&holder, &id.id, &id.attrib, &context);
 if ((status != SS$_NORMAL) && (status != SS$_NOSUCHID))      {
  (void) sys$finish_rdb(context);
 }
 if (status == SS$_NORMAL)       {
  pidadr = 0;
  prvatr = 0;
  status = sys$grantid(&pidadr, 0, &id, 0, &prvatr);
  if ((status != SS$_NORMAL) && (status != SS$_ACCVIO) &&
      (status != SS$_WASSET)) {
   (void) sys$finish_rdb(context);
   errno = EVMSERR;
   vaxc$errno = status;
   return(-1);
  }
  else    {
   goto again;
  }
 }
 if (status == SS$_NOSUCHID) return(0);
 else {
  errno = EVMSERR;
  vaxc$errno = status;
  return(-1);
 }
}
 
