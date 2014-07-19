/*
 * $Id: osdep.h,v 1.1.1.1 1996/03/08 15:44:26 mclareni Exp $
 *
 * $Log: osdep.h,v $
 * Revision 1.1.1.1  1996/03/08 15:44:26  mclareni
 * Cspack
 *
 */
/*
 * Copyright (C) 1990-1992 by CERN/CN/SW/DC
 * All rights reserved
 */
 
/*
 * @(#)osdep.h  1.4 10/14/92 CERN CN-SW/DC Frederic Hemmer
 */
 
/* osdep.h      Operating system dependencies                           */
 
#ifndef _OSDEP_H_INCLUDED_
#define  _OSDEP_H_INCLUDED_
 
/*
 * Data representation
 */
 
#define BYTESIZE        1
#define WORDSIZE        2
#define LONGSIZE        4
#define QUADSIZE        8
 
#if defined(unix) || defined(_AIX)
#ifndef CRAY
typedef unsigned char   U_BYTE;
typedef unsigned short  U_WORD;
typedef unsigned int    U_LONG;
typedef struct  {
        U_LONG      lslw;
        U_LONG      mslw;
} U_QUAD;
typedef          char   BYTE;
typedef          short  WORD;
typedef          int    LONG;
typedef struct  {
        U_LONG    lslw;
        LONG      mslw;
} QUAD;
#else /* CRAY */
typedef unsigned char   U_BYTE;
typedef unsigned short  U_WORD;
typedef          int    U_LONG;
typedef unsigned long   U_QUAD;
 
typedef          char   BYTE;
typedef          short  WORD;
typedef          int    LONG;
typedef          long   QUAD;
 
#endif /* CRAY */
#endif /* unix */
 
#ifdef VM
typedef unsigned char   U_BYTE;
typedef unsigned short  U_WORD;
typedef unsigned int    U_LONG;
typedef struct  {
        U_LONG      lslw;
        U_LONG      mslw;
} U_QUAD;
typedef          char   BYTE;
typedef          short  WORD;
typedef          int    LONG;
typedef struct  {
        LONG      lslw;
        LONG      mslw;
} QUAD;
 
#endif /* VM */
 
#ifdef vms
typedef unsigned char   U_BYTE;
typedef unsigned short  U_WORD;
typedef unsigned int    U_LONG;
typedef struct  {
        U_LONG      lslw;
        U_LONG      mslw;
} U_QUAD;
typedef          char   BYTE;
typedef          short  WORD;
typedef          int    LONG;
typedef struct  {
        LONG      lslw;
        LONG      mslw;
} QUAD;
 
#endif /* vms */
 
#define BYTEADDR(x)     (((char *)&(x))+sizeof(BYTE)-BYTESIZE)
#define WORDADDR(x)     (((char *)&(x))+sizeof(WORD)-WORDSIZE)
#define LONGADDR(x)     (((char *)&(x))+sizeof(LONG)-LONGSIZE)
#define QUADADDR(x)     (((char *)&(x))+sizeof(QUAD)-QUADSIZE)
 
/*
 * Byte swapping
 */
 
/*
 * Exit codes
 */
 
#ifdef unix
#define BADEXIT         1
#define GOODEXIT        0
#endif /* unix */
#ifdef VM
#define BADEXIT         -1
#define GOODEXIT        0
#endif /* VM */
#ifdef vms
#define BADEXIT         0
#define GOODEXIT        1
#endif /* vms */
 
/*
 * Error reporting
 */
 
#ifdef VM
#define NETERROR  tcperror
#define OSERROR   perror
#else
#define NETERROR  perror
#define OSERROR   perror
#endif /* VM */
 
#endif /* _OSDEP_H_INCLUDED_ */
