/*
 * $Id: ntmgt.c,v 1.2 1997/03/14 14:22:28 mclareni Exp $
 *
 * $Log: ntmgt.c,v $
 * Revision 1.2  1997/03/14 14:22:28  mclareni
 * WNT mods
 *
 * Revision 1.1.1.1.2.1  1997/01/21 11:25:22  mclareni
 * All mods for Winnt 96a on winnt branch
 *
 * Revision 1.1.1.1  1996/02/14 13:10:26  mclareni
 * Higz
 *
 */
#include "higz/pilot.h"
#if defined(CERNLIB_NTC)
/*CMZ :  1.23/06 22/11/95  10.36.05  by  O.Couet*/
/*-- Author : O.Couet*/
/******************************************************************************
 *                                                                            *
 * Dynamic Normalisation Transformation management package.                   *
 *                                                                            *
 * ignint: Initiate the Normalisation Transformation management               *
 * igncre: Create a new Normalisation Transformation                          *
 * ignscu: Set the Current Normalisation Transformation                       *
 * igndel: Delete the Current Normalisation Transformation                    *
 * igngar: Delete the NT with a priority less than Priority                   *
 * ignspr: Set the Priority of the Current Normalisation Transformation       *
 * ignswn: Set the Window of the Current Normalisation Transformation         *
 * ignsvp: Set the Viewport of the Current Normalisation Transformation       *
 * ignnxt: Set the NEXT Normalisation Transformation as the curreny one       *
 * ignget: Get the Current Normalisation Transformation parameters            *
 * igns3w: Set the 3D Window of the Current Normalisation Transformation      *
 * igns3v: Set the 3D Viewport of the Current Normalisation Transformation    *
 * igng3d: Get the Current 3D Normalisation Transformation parameters         *
 *                                                                            *
 ******************************************************************************/

#include <stdlib.h>
#include <stdio.h>

#if defined(CERNLIB_QX_SC)
#define ignint ignint_
#define igncre igncre_
#define igndel igndel_
#define igngar igngar_
#define ignspr ignspr_
#define ignswn ignswn_
#define ignsvp ignsvp_
#define ignget ignget_
#define igns3w igns3w_
#define igns3v igns3v_
#define ignscu ignscu_
#define ignnxt ignnxt_
#define igng3d igng3d_
#endif

#if defined(CERNLIB_QXCAPT)
#define ignint IGNINT
#define igncre IGNCRE
#define igndel IGNDEL
#define igngar IGNGAR
#define ignspr IGNSPR
#define ignswn IGNSWN
#define ignsvp IGNSVP
#define ignget IGNGET
#define igns3w IGNS3W
#define igns3v IGNS3V
#define ignscu IGNSCU
#define ignnxt IGNNXT
#define igng3d IGNG3D
#endif

#if defined(CERNLIB_IBM)
#pragma linkage (ignint ,FORTRAN)
#pragma linkage (igncre ,FORTRAN)
#pragma linkage (igndel ,FORTRAN)
#pragma linkage (igngar ,FORTRAN)
#pragma linkage (ignspr ,FORTRAN)
#pragma linkage (ignswn ,FORTRAN)
#pragma linkage (ignsvp ,FORTRAN)
#pragma linkage (ignget ,FORTRAN)
#pragma linkage (igns3w ,FORTRAN)
#pragma linkage (igns3v ,FORTRAN)
#pragma linkage (ignscu ,FORTRAN)
#pragma linkage (ignnxt ,FORTRAN)
#pragma linkage (igng3d ,FORTRAN)
#endif


typedef struct _NT  NT; 
struct _NT {
   int             ID;
   int             Priority;
   float           X1_Window;
   float           X2_Window;
   float           Y1_Window;
   float           Y2_Window;
   float           X1_Viewport;
   float           X2_Viewport;
   float           Y1_Viewport;
   float           Y2_Viewport;
   int             Is_3D;
   float           X1_3D_Window;
   float           X2_3D_Window;
   float           Y1_3D_Window;
   float           Y2_3D_Window;
   float           Z1_3D_Window;
   float           Z2_3D_Window;
   float           X1_3D_Viewport;
   float           X2_3D_Viewport;
   float           Y1_3D_Viewport;
   float           Y2_3D_Viewport;
   float           Phi;
   float           Theta;
   float           Psi;
   int             Perspective;
   NT              *Previous_NT;
   NT              *Next_NT;
};

static NT *NT_Root    = NULL;
static NT *NT_Last    = NULL;
static NT *NT_Current = NULL;
static NT *NT_Save    = NULL;
static int Highest_Priority = 0;

/******************************************************************************
 *                                                                            *
 * Initiate the Normalisation Transformation management                       *
 *                                                                            *
 ******************************************************************************/
 void type_of_call ignint()
 {
    NT_Root                 = (NT *) calloc(1, sizeof(NT));
    NT_Root->ID             = 0;
    NT_Root->Priority       = 1;
    NT_Root->X1_Window      = 0.0;
    NT_Root->X2_Window      = 1.0;
    NT_Root->Y1_Window      = 0.0;
    NT_Root->Y2_Window      = 1.0;
    NT_Root->X1_Viewport    = 0.0;
    NT_Root->X2_Viewport    = 1.0;
    NT_Root->Y1_Viewport    = 0.0;
    NT_Root->Y2_Viewport    = 1.0;
    NT_Root->Is_3D          = 0;
    NT_Root->X1_3D_Window   = 0.0;
    NT_Root->X2_3D_Window   = 1.0;
    NT_Root->Y1_3D_Window   = 0.0;
    NT_Root->Y2_3D_Window   = 1.0;
    NT_Root->Z1_3D_Window   = 0.0;
    NT_Root->Z2_3D_Window   = 1.0;
    NT_Root->X1_3D_Viewport = 0.0;
    NT_Root->X2_3D_Viewport = 1.0;
    NT_Root->Y1_3D_Viewport = 0.0;
    NT_Root->Y2_3D_Viewport = 1.0;
    NT_Root->Phi            = 30.0;
    NT_Root->Theta          = 30.0;
    NT_Root->Psi            = 0.0;
    NT_Root->Perspective    = 0;
    NT_Root->Next_NT        = NT_Root;
    NT_Root->Previous_NT    = NT_Root;
    NT_Current              = NT_Root;
    NT_Last                 = NT_Root;
 }

/******************************************************************************
 *                                                                            *
 * Create a new Normalisation Transformation                                  *
 *                                                                            *
 ******************************************************************************/
 void  type_of_call igncre(ID)
 int *ID;
 {
    NT_Current                 = (NT *) calloc(1, sizeof(NT));
    NT_Current->ID             = *ID;
    NT_Current->Priority       = 1;
    NT_Current->X1_Window      = 0.0;
    NT_Current->X2_Window      = 1.0;
    NT_Current->Y1_Window      = 0.0;
    NT_Current->Y2_Window      = 1.0;
    NT_Current->X1_Viewport    = 0.0;
    NT_Current->X2_Viewport    = 1.0;
    NT_Current->Y1_Viewport    = 0.0;
    NT_Current->Y2_Viewport    = 1.0;
    NT_Current->Is_3D          = 0;
    NT_Current->X1_3D_Window   = 0.0;
    NT_Current->X2_3D_Window   = 1.0;
    NT_Current->Y1_3D_Window   = 0.0;
    NT_Current->Y2_3D_Window   = 1.0;
    NT_Current->Z1_3D_Window   = 0.0;
    NT_Current->Z2_3D_Window   = 1.0;
    NT_Current->X1_3D_Viewport = 0.0;
    NT_Current->X2_3D_Viewport = 1.0;
    NT_Current->Y1_3D_Viewport = 0.0;
    NT_Current->Y2_3D_Viewport = 1.0;
    NT_Current->Phi            = 30.0;
    NT_Current->Theta          = 30.0;
    NT_Current->Psi            = 0.0;
    NT_Current->Perspective    = 0;
    NT_Current->Next_NT        = NT_Root;
    NT_Current->Previous_NT    = NT_Last;

    NT_Last->Next_NT           = NT_Current;
    NT_Root->Previous_NT       = NT_Current;
    NT_Last                    = NT_Current;
 }

/******************************************************************************
 *                                                                            *
 * Set the Current Normalisation Transformation                               *
 *    Returns 1 if NT already exits 0 otherwise                               *
 *                                                                            *
 ******************************************************************************/
 int  type_of_call ignscu(ID)
 int *ID;
 {
    if ( NT_Current->ID == *ID) return 1;
    NT_Save = NT_Current;
    while ( NT_Current->Next_NT != NT_Save) {
       NT_Current = NT_Current->Next_NT;
       if ( NT_Current->ID == *ID) return 1;
    }
    return 0;
 }

/******************************************************************************
 *                                                                            *
 * Delete the Current Normalisation Transformation                            *
 *                                                                            *
 ******************************************************************************/
 void  type_of_call igndel()
 {
    if ( NT_Current == NT_Root) return;
    NT_Save              = NT_Current->Previous_NT;
    NT_Save->Next_NT     = NT_Current->Next_NT;
    NT_Save              = NT_Current->Next_NT;
    NT_Save->Previous_NT = NT_Current->Previous_NT;
    free (NT_Current);
    NT_Current           = NT_Save;
    NT_Last              = NT_Root->Previous_NT;
 }

/******************************************************************************
 *                                                                            *
 * Delete the Normalisation Transformation with a priority less than Priority *
 *                                                                            *
 ******************************************************************************/
 void  type_of_call igngar(Priority)
 int *Priority;
 {
    NT_Current = NT_Root->Next_NT;
    while ( NT_Current != NT_Root) {
       if(NT_Current->Priority < *Priority) {
          igndel();
       } else {
          NT_Current = NT_Current->Next_NT;
       }
    }
 }

/******************************************************************************
 *                                                                            *
 * Set the Priority of the Current Normalisation Transformation               *
 *                                                                            *
 ******************************************************************************/
 void  type_of_call ignspr(Priority)
 int *Priority;
 {
    NT_Current->Priority = *Priority;
    if ( *Priority > Highest_Priority) Highest_Priority = *Priority;
 }

/******************************************************************************
 *                                                                            *
 * Set the Window of the Current Normalisation Transformation                 *
 *                                                                            *
 ******************************************************************************/
 void  type_of_call ignswn(X1_Window, X2_Window, Y1_Window, Y2_Window)
 float *X1_Window;
 float *X2_Window;
 float *Y1_Window;
 float *Y2_Window;
 {
    NT_Current->X1_Window = *X1_Window;
    NT_Current->X2_Window = *X2_Window;
    NT_Current->Y1_Window = *Y1_Window;
    NT_Current->Y2_Window = *Y2_Window;
 }

/******************************************************************************
 *                                                                            *
 * Set the Viewport of the Current Normalisation Transformation               *
 *                                                                            *
 ******************************************************************************/
 void  type_of_call ignsvp(X1_Viewport, X2_Viewport, Y1_Viewport, Y2_Viewport)
 float *X1_Viewport;
 float *X2_Viewport;
 float *Y1_Viewport;
 float *Y2_Viewport;
 {
    NT_Current->X1_Viewport = *X1_Viewport;
    NT_Current->X2_Viewport = *X2_Viewport;
    NT_Current->Y1_Viewport = *Y1_Viewport;
    NT_Current->Y2_Viewport = *Y2_Viewport;
 }

/******************************************************************************
 *                                                                            *
 * Set the NEXT Normalisation Transformation as the current one               *
 * init=1 define the beginning of the loop                                    *
 * init=0 set the next NT current                                             *
 * This routine returns 1 when the loop is finished 0 otherwise               *
 *                                                                            *
 ******************************************************************************/
 int  type_of_call ignnxt(init)
 int *init;
 {
    if ( *init == 1 ) {
       NT_Save = NT_Current;
       return 0;
    } else {
       NT_Current = NT_Current->Next_NT;
       if ( NT_Current == NT_Save ) {
          return 1;
       } else {
          return 0;
       }
    }
 }

/******************************************************************************
 *                                                                            *
 * Get the Current 2D Normalisation Transformation parameters                 *
 *                                                                            *
 ******************************************************************************/
 void  type_of_call ignget(NTID, Priority, H_Priority,
             X1_Window, X2_Window, Y1_Window, Y2_Window,
             X1_Viewport, X2_Viewport, Y1_Viewport, Y2_Viewport, Is_3D)
 int *NTID;
 int *Priority;
 int *H_Priority;
 float *X1_Window;
 float *X2_Window;
 float *Y1_Window;
 float *Y2_Window;
 float *X1_Viewport;
 float *X2_Viewport;
 float *Y1_Viewport;
 float *Y2_Viewport;
 int *Is_3D;
 {
    *H_Priority  = Highest_Priority;
    *NTID        = NT_Current->ID;
    *Priority    = NT_Current->Priority;
    *X1_Window   = NT_Current->X1_Window;
    *X2_Window   = NT_Current->X2_Window;
    *Y1_Window   = NT_Current->Y1_Window;
    *Y2_Window   = NT_Current->Y2_Window;
    *X1_Viewport = NT_Current->X1_Viewport;
    *X2_Viewport = NT_Current->X2_Viewport;
    *Y1_Viewport = NT_Current->Y1_Viewport;
    *Y2_Viewport = NT_Current->Y2_Viewport;
    *Is_3D       = NT_Current->Is_3D;
 }

/******************************************************************************
 *                                                                            *
 * Set the 3D Window of the Current Normalisation Transformation              *
 *                                                                            *
 ******************************************************************************/
 void  type_of_call igns3w(X1_3D_Window, X2_3D_Window,
             Y1_3D_Window, Y2_3D_Window,
             Z1_3D_Window, Z2_3D_Window,
             Phi, Theta, Psi)
 float *X1_3D_Window;
 float *X2_3D_Window;
 float *Y1_3D_Window;
 float *Y2_3D_Window;
 float *Z1_3D_Window;
 float *Z2_3D_Window;
 float *Phi;
 float *Theta;
 float *Psi;
 {
    NT_Current->Is_3D        = 1;
    NT_Current->X1_3D_Window = *X1_3D_Window;
    NT_Current->X2_3D_Window = *X2_3D_Window;
    NT_Current->Y1_3D_Window = *Y1_3D_Window;
    NT_Current->Y2_3D_Window = *Y2_3D_Window;
    NT_Current->Z1_3D_Window = *Z1_3D_Window;
    NT_Current->Z2_3D_Window = *Z2_3D_Window;
    NT_Current->Phi          = *Phi;
    NT_Current->Theta        = *Theta;
    NT_Current->Psi          = *Psi;
 }

/******************************************************************************
 *                                                                            *
 * Set the 3D Viewport of the Current Normalisation Transformation            *
 *                                                                            *
 ******************************************************************************/
 void  type_of_call igns3v(X1_3D_Viewport, X2_3D_Viewport, Y1_3D_Viewport, Y2_3D_Viewport)
 float *X1_3D_Viewport;
 float *X2_3D_Viewport;
 float *Y1_3D_Viewport;
 float *Y2_3D_Viewport;
 {
    NT_Current->Is_3D          = 1;
    NT_Current->X1_3D_Viewport = *X1_3D_Viewport;
    NT_Current->X2_3D_Viewport = *X2_3D_Viewport;
    NT_Current->Y1_3D_Viewport = *Y1_3D_Viewport;
    NT_Current->Y2_3D_Viewport = *Y2_3D_Viewport;
 }

/******************************************************************************
 *                                                                            *
 * Get the Current 3D Normalisation Transformation parameters                 *
 *                                                                            *
 ******************************************************************************/
 void  type_of_call igng3d(X1_3D_Window, X2_3D_Window, Y1_3D_Window, Y2_3D_Window,
             Z1_3D_Window, Z2_3D_Window, Phi, Theta, Psi,
             X1_3D_Viewport, X2_3D_Viewport, Y1_3D_Viewport, Y2_3D_Viewport)
 float *X1_3D_Window;
 float *X2_3D_Window;
 float *Y1_3D_Window;
 float *Y2_3D_Window;
 float *Z1_3D_Window;
 float *Z2_3D_Window;
 float *Phi;
 float *Theta;
 float *Psi;
 float *X1_3D_Viewport;
 float *X2_3D_Viewport;
 float *Y1_3D_Viewport;
 float *Y2_3D_Viewport;
 {
    *X1_3D_Window   = NT_Current->X1_3D_Window;
    *X2_3D_Window   = NT_Current->X2_3D_Window;
    *Y1_3D_Window   = NT_Current->Y1_3D_Window;
    *Y2_3D_Window   = NT_Current->Y2_3D_Window;
    *Z1_3D_Window   = NT_Current->Z1_3D_Window;
    *Z2_3D_Window   = NT_Current->Z2_3D_Window;
    *Phi            = NT_Current->Phi;
    *Theta          = NT_Current->Theta;
    *Psi            = NT_Current->Psi;
    *X1_3D_Viewport = NT_Current->X1_3D_Viewport;
    *X2_3D_Viewport = NT_Current->X2_3D_Viewport;
    *Y1_3D_Viewport = NT_Current->Y1_3D_Viewport;
    *Y2_3D_Viewport = NT_Current->Y2_3D_Viewport;
 }
#endif
