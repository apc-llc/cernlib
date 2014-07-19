/*
 * $Id: converter.h,v 1.1.1.1 1996/03/01 11:39:31 mclareni Exp $
 *
 * $Log: converter.h,v $
 * Revision 1.1.1.1  1996/03/01 11:39:31  mclareni
 * Paw
 *
 */
/*CMZ :  2.04/15 14/03/94  01.27.08  by  Fons Rademakers*/
/*-- Author :    Fons Rademakers   23/10/92*/
#ifndef _CONVERTER_H_
#define _CONVERTER_H_

typedef int XtGravity;

#define XtRGravity "Gravity"
#define XtEForget "forget"
#define XtENorthWest "northwest"
#define XtENorth "north"
#define XtENorthEast "northeast"
#define XtEWest "west"
#define XtECenter "center"
#define XtEEast "east"
#define XtESouthWest "southwest"
#define XtESouth "south"
#define XtESouthEast "southeast"
#define XtEStatic "static"
#define XtEUnmap "unmap"
#define XtEleft "left"
#define XtEright "right"
#define XtEtop "top"
#define XtEbottom "bottom"


extern void XrdmCvtStringToGravity (
#if NeedFunctionPrototypes
    XrmValue*           /* args */,
    Cardinal*           /* num_args */,
    XrmValuePtr         /* fromVal */,
    XrmValuePtr         /* toVal */
#endif
);

#endif /* _CONVERTER_H_ */
